{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Data.ML.Repl
Description :  Simple REPL.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

A simple REPL to test your models.
-}
module Data.ML.Repl (
    DataSet(..),
    Repl,
    runRepl,
    repl
    ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader hiding (forM_)
import Data.Foldable
import Data.IORef.Lifted
import Data.ML.Cost
import Data.ML.Model
import Data.ML.Repl.Commands (Command(..))
import Data.ML.Search
import Data.Random
import Data.Traversable
import Data.Typeable
import qualified Data.IORef as R
import Language.Haskell.Interpreter hiding (get)
import Linear
import Prelude hiding (forM_)
import System.Console.Haskeline hiding (catch)
import System.Console.Haskeline.IO

-- | A wrapped data set.
newtype DataSet m = DataSet
    (forall a. Floating a => [(Input m a, Output m a)])

data ReplEnv m = ReplEnv
    { dataSet :: DataSet m
    , cost :: Cost m
    , inputState :: InputState
    , model :: IORef (m Double)
    }

-- | The repl monad.
newtype Repl m a = Repl (ReaderT (ReplEnv m) Interpreter a)
    deriving ( Functor, Applicative, Monad
             , MonadReader (ReplEnv m)
             , MonadIO, MonadCatch, MonadThrow, MonadMask )

instance MonadBase IO (Repl m) where
    liftBase = liftIO

instance MonadInterpreter (Repl m) where
    fromSession f = Repl (lift $ fromSession f)
    modifySessionRef f g = Repl (lift $ modifySessionRef f g)
    runGhc f = Repl (lift $ runGhc f)

-- | Runs the repl monad.
runRepl :: m Double -> DataSet m -> Cost m -> Repl m a -> IO a
runRepl model' dataSet cost (Repl m) = do
    model <- newIORef model'
    inputState <- initializeInput defaultSettings
    result <- runInterpreter $ flip runReaderT (ReplEnv {..}) m
    closeInput inputState
    either throwM return result

-- | The default repl implementation.
repl :: forall m. Model m => Additive m => Traversable m => Typeable m
     => Show (Output m Double) => Repl m ()
repl = do
    inputState' <- asks inputState
    setImports [ "Prelude"
               , "Data.Random"
               , "Data.Foldable"
               , "Data.Traversable"
               , "Control.Applicative"
               , "Data.ML.Repl.Commands"
               ]
    maybeLine <- liftIO $ queryInput inputState' $ getInputLine "> "
    case maybeLine of
      Nothing -> return ()
      Just line -> do
        result <- catch
            (Right <$> interpret line (as :: Command m))
            (return . Left)
        case result of
            Left err -> liftIO $ putStrLn $ formatError err
            Right command -> runCommand command
        repl

formatError :: InterpreterError -> String
formatError (UnknownError s) = s
formatError (WontCompile errs) = unlines (map errMsg errs)
formatError (NotAllowed s) = s
formatError (GhcException s) = s

-- | Executes a command.
runCommand :: Model m => Additive m => Traversable m
           => Show (Output m Double)
           => Command m -> Repl m ()

runCommand (GenModel g) = do
    modelVar <- asks model
    model' <- readIORef modelVar
    newModel <- liftIO $ sample (traverse (const g) model')
    writeIORef modelVar newModel

runCommand PrintModel = do
    model' <- asks model >>= readIORef
    liftIO $ traverse_ print model'

runCommand (Train n) = do
    DataSet dataSet' <- asks dataSet
    cost' <- asks cost
    modelVar <- asks model
    model' <- readIORef modelVar

    shuffled <- liftIO $ sample $ shuffle dataSet'
    let split = floor $ 0.6 * fromIntegral (length shuffled)
        trainingSet = take split shuffled
        validationSet = drop split shuffled
        batches = replicate n trainingSet
        iters = adaGrad batches cost' model'

    forM_ iters $ \(j, m) -> do
        liftIO $ print j
        writeIORef modelVar m

runCommand (Run input) = do
    model' <- asks model >>= readIORef
    liftIO $ print $ predict input model'

runCommand EvalCost = do
    DataSet dataSet' <- asks dataSet
    cost' <- asks cost
    model' <- asks model >>= readIORef

    liftIO $ print $ evalCost cost' dataSet' model'
