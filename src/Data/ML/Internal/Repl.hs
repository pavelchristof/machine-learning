{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Data.ML.Internal.Repl
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Internal.Repl where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Typeable
import Language.Haskell.Interpreter
import System.Console.Haskeline (getInputLine, defaultSettings)
import System.Console.Haskeline.IO

-- | The repl monad.
newtype ReplT m a = ReplT { unReplT :: ReaderT InputState (InterpreterT m) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask )

getInputState :: Monad m => ReplT m InputState
getInputState = ReplT ask

instance MonadTrans ReplT where
    lift = ReplT . lift . lift

instance MonadIO m => MonadBase IO (ReplT m) where
    liftBase = liftIO

instance (MonadIO m, MonadMask m) => MonadInterpreter (ReplT m) where
    fromSession f = ReplT (lift $ fromSession f)
    modifySessionRef f g = ReplT (lift $ modifySessionRef f g)
    runGhc f = ReplT (lift $ runGhc f)

runReplT :: MonadIO m => MonadMask m => ReplT m a -> m a
runReplT (ReplT m) = do
    inputState <- liftIO $ initializeInput defaultSettings
    result <- runInterpreter $ runReaderT m inputState
    liftIO $ closeInput inputState
    case result of
      Left err -> throwM err
      Right val -> return val

repl :: Typeable m => MonadMask m => MonadIO m => ReplT m ()
repl = do
    inputState <- getInputState
    maybeLine <- liftIO $ queryInput inputState $ getInputLine "> "
    case maybeLine of
      Nothing -> return ()
      Just line -> do
        isolate $ do
            command <- interpret line infer
            lift command
        repl

isolate :: forall m. MonadIO m => MonadMask m => ReplT m () -> ReplT m ()
isolate m = catches m [Handler handleInterpError, Handler handleAny]
  where
    handleInterpError = liftIO . putStrLn . formatError
    handleAny (_ :: SomeException) = return ()

formatError :: InterpreterError -> String
formatError (UnknownError s) = s
formatError (WontCompile errs) = unlines (map errMsg errs)
formatError (NotAllowed s) = s
formatError (GhcException s) = s
