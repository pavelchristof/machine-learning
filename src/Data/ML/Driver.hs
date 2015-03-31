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
    InputOf,
    OutputOf,

    -- * Driver transformer.
    DriverT,
    Driver,
    runDriverT,

    -- * Model manipulation.
    genModel,
    modifyModel,

    -- * Serialization.
    save,
    load,

    -- * Training and testing.
    train,
    run,
    test,

    CountCorrect(..),
    correct,
    incorrect,

    -- * Printing.
    printModel,
    printCost,

    -- * Benchmarking.
    timed
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Criterion.Measurement
import qualified Data.ByteString.Lazy as BS
import           Data.Bytes.Get
import           Data.Bytes.Put
import           Data.Bytes.Serial
import           Data.Foldable
import           Data.ML.Cost
import           Data.ML.Internal.Driver
import           Data.ML.Model
import           Data.ML.Search
import           Data.Random
import           Data.Vector.Parallel
import           Linear

-- | Overwrites the model with a randomly generated one.
genModel :: MonadDriver m => Show (ScalarOf m) => MonadRandom m => Traversable (ModelOf m)
         => RVar (ScalarOf m) -> m ()
genModel g = do
    model <- getModel
    model' <- sample (traverse (const g) model)
    setModel model'

-- | Modifies the model.
modifyModel :: MonadDriver m => (ModelOf m (ScalarOf m) -> ModelOf m (ScalarOf m)) -> m ()
modifyModel f = getModel >>= setModel . f

-- | Prints the current model.
printModel :: MonadDriver m => Show (ScalarOf m) => Foldable (ModelOf m) => m ()
printModel = getModel >>= liftIO . traverse_ print

-- | Evalutes the cost function on the whole data set.
printCost :: MonadDriver m => Show (ScalarOf m) => m ()
printCost = do
    dataSet <- getTrainingSet
    cost <- getCostFun
    model <- getModel
    liftIO $ print $ evalCost cost dataSet model

-- | Saves the model to a binary file.
save :: MonadDriver m => Serial1 (ModelOf m) => Serial (ScalarOf m)
     => FilePath -> m ()
save filePath = do
    model <- getModel
    let bytes = runPutL (serialize1 model)
    liftIO $ BS.writeFile filePath bytes

-- | Loads the model from a binary file.
load :: MonadDriver m => Serial1 (ModelOf m) => Serial (ScalarOf m)
     => FilePath -> m ()
load filePath = do
    bytes <- liftIO $ BS.readFile filePath
    let model = runGetL deserialize1 bytes
    setModel model

-- | @train n@ trains the model for n iterations.
train :: MonadDriver m => MonadRandom m
      => Traversable (ModelOf m) => Additive (ModelOf m)
      => RealFloat (ScalarOf m) => Show (ScalarOf m)
      => Int -> m ()
train n = do
    trainingSet <- getTrainingSet
    cost <- getCostFun
    model <- getModel

    let batches = replicate n trainingSet
        iters = adaGrad batches cost model

    forM_ iters $ \(j, m) -> do
        liftIO $ print j
        setModel m

-- | Runs the model on an input.
run :: MonadDriver m => Show (OutputOf m) => InputOf m -> m ()
run input = do
    model <- getModel
    liftIO $ print $ predict input model

data CountCorrect = CountCorrect { getCorrectCount :: !Int, getTotalCount :: !Int }

correct :: CountCorrect
correct = CountCorrect 1 1

incorrect :: CountCorrect
incorrect = CountCorrect 0 1

instance Monoid CountCorrect where
    mempty = CountCorrect 0 0
    CountCorrect p q `mappend` CountCorrect p' q' = CountCorrect (p + p') (q + q')

instance Show CountCorrect where
    show (CountCorrect p q) = show p ++ "/" ++ show q ++ " correct"

-- | Runs a test on both training set and test set.
test :: MonadDriver m => Monoid summary => Show summary
     => (OutputOf m -> OutputOf m -> summary) -> m ()
test f = do
    model <- getModel
    trainingSet <- getTrainingSet
    testSet <- getTestSet

    let testOne (i, o) = f (predict i model) o
        runTest = liftIO . print . foldMap testOne . ParVector

    liftIO $ putStrLn "Training set:"
    runTest trainingSet

    liftIO $ putStrLn "Test set:"
    runTest testSet

-- | Measures and prints the time it takes to execute a command.
timed :: MonadDriver m => MonadIO m => m () -> m ()
timed m = do
    liftIO initializeTime

    t0 <- liftIO getTime
    m
    t1 <- liftIO getTime

    let msg = "Took " ++ show (t1 - t0) ++ " seconds."
    liftIO $ putStrLn msg
