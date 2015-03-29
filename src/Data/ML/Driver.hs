{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Data.ML.Driver
Description :  Monadic interface for use in the REPL.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

This module defines a MonadDriver which provides an imperative
way of working with the model.
-}
module Data.ML.Driver (
    -- * Driver monad class.
    MonadDriver(..),
    modifyModel,
    modifyDataSet,
    modifyCostFun,

    -- * Driver transformer.
    DriverT,
    Driver,
    runDriverT,

    -- * Commands.
    genModel,
    printModel,
    printCost,
    train,
    run
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.ML.Cost
import Data.ML.Internal.Driver
import Data.ML.Model
import Data.ML.Search
import Data.Random
import Linear

modifyModel :: MonadDriver m => (ModelOf m (ScalarOf m) -> ModelOf m (ScalarOf m)) -> m ()
modifyModel f = getModel >>= setModel . f

modifyDataSet :: MonadDriver m
              => (DataSet (ModelOf m) (ScalarOf m) -> DataSet (ModelOf m) (ScalarOf m))
              -> m ()
modifyDataSet f = getDataSet >>= setDataSet . f

modifyCostFun :: MonadDriver m => (Cost (ModelOf m) -> Cost (ModelOf m)) -> m ()
modifyCostFun f = getCostFun >>= setCostFun . f

-- | Overwrites the model with a randomly generated one.
genModel :: MonadDriver m => Show (ScalarOf m) => MonadRandom m => Traversable (ModelOf m)
         => RVar (ScalarOf m) -> m ()
genModel g = do
    model <- getModel
    model' <- sample (traverse (const g) model)
    setModel model'

-- | Prints the current model.
printModel :: MonadDriver m => Show (ScalarOf m) => Foldable (ModelOf m) => m ()
printModel = getModel >>= liftIO . traverse_ print

-- | Evalutes the cost function on the whole data set.
printCost :: MonadDriver m => Show (ScalarOf m) => m ()
printCost = do
    dataSet <- getDataSet
    cost <- getCostFun
    model <- getModel
    liftIO $ print $ evalCost cost dataSet model

-- | @train n@ trains the model for n iterations.
train :: MonadDriver m => MonadRandom m
      => Traversable (ModelOf m) => Additive (ModelOf m)
      => RealFloat (ScalarOf m) => Show (ScalarOf m)
      => Int -> m ()
train n = do
    dataSet <- getDataSet
    cost <- getCostFun
    model <- getModel

    -- TODO: write shuffle for vector!
    shuffled <- sample $ shuffle $ toList dataSet
    let split = floor $ (0.6 :: Double) * fromIntegral (length shuffled)
        trainingSet = take split shuffled
        validationSet = drop split shuffled
        batches = replicate n trainingSet
        iters = adaGrad batches cost model

    forM_ iters $ \(j, m) -> do
        liftIO $ print j
        setModel m

-- | Runs the model on an input.
run :: MonadDriver m => Show (Output (ModelOf m) (ScalarOf m))
    => Input (ModelOf m) (ScalarOf m)  -> m ()
run input = do
    model <- getModel
    liftIO $ print $ predict input model
