{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Data.ML.Internal.Driver
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Internal.Driver where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import Data.ML.Cost
import Data.ML.Model
import Data.RVar
import Linear

class (MonadIO m, Model (ModelOf m), Floating (ScalarOf m)) => MonadDriver m where
    type ModelOf m :: * -> *
    type ScalarOf m :: *

    setModel :: ModelOf m (ScalarOf m) -> m ()
    getModel :: m (ModelOf m (ScalarOf m))

    setTrainingSet :: DataSet (ModelOf m) (ScalarOf m) -> m ()
    getTrainingSet :: m (DataSet (ModelOf m) (ScalarOf m))

    setTestSet :: DataSet (ModelOf m) (ScalarOf m) -> m ()
    getTestSet :: m (DataSet (ModelOf m) (ScalarOf m))

    setCostFun :: Cost (ModelOf m) -> m ()
    getCostFun :: m (Cost (ModelOf m))

type InputOf m = Input (ModelOf m) (ScalarOf m)
type OutputOf m = Output (ModelOf m) (ScalarOf m)

data DriverEnv model scalar = DriverEnv {
    model :: IORef (model scalar),
    trainingSet :: IORef (DataSet model scalar),
    testSet :: IORef (DataSet model scalar),
    costFun :: IORef (Cost model)
}

-- | Basic implementation of MonadDriver.
newtype DriverT model scalar m a = DriverT (ReaderT (DriverEnv model scalar) m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

type Driver model scalar = DriverT model scalar IO

instance MonadTrans (DriverT model scalar) where
    lift = DriverT . lift

instance (MonadIO m, Model model, Floating scalar) => MonadDriver (DriverT model scalar m) where
    type ModelOf (DriverT model scalar m) = model
    type ScalarOf (DriverT model scalar m) = scalar

    setModel m = DriverT $ asks model >>= liftIO . flip writeIORef m
    getModel = DriverT $ asks model >>= liftIO . readIORef

    setTrainingSet ds = DriverT $ asks trainingSet >>= liftIO . flip writeIORef ds
    getTrainingSet = DriverT $ asks trainingSet >>= liftIO . readIORef

    setTestSet ds = DriverT $ asks testSet >>= liftIO . flip writeIORef ds
    getTestSet = DriverT $ asks testSet >>= liftIO . readIORef

    setCostFun c = DriverT $ asks costFun >>= liftIO . flip writeIORef c
    getCostFun = DriverT $ asks costFun >>= liftIO . readIORef

instance MonadRandom m => MonadRandom (DriverT model scalar m) where
    getRandomWord8 = lift getRandomWord8
    getRandomWord16 = lift getRandomWord16
    getRandomWord32 = lift getRandomWord32
    getRandomWord64 = lift getRandomWord64
    getRandomDouble = lift getRandomDouble
    getRandomNByteInteger = lift . getRandomNByteInteger

-- | Runs the driver with model set to zero, empty data set and zero cost function.
runDriverT :: Additive model => Num scalar => MonadIO m
           => DriverT model scalar m a -> m a
runDriverT (DriverT m) = do
    model <- liftIO $ newIORef zero
    trainingSet <- liftIO $ newIORef mempty
    testSet <- liftIO $ newIORef mempty
    costFun <- liftIO $ newIORef 0
    runReaderT m DriverEnv {..}
