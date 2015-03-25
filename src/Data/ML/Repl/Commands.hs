{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
Module      :  Data.ML.Repl.Commands
Description :  Commands usable in the REPL.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

This module exports commands usable in the REPL.
-}
module Data.ML.Repl.Commands where

import Data.Random
import Data.Typeable
import Data.ML.Model

data Command m
    = GenModel (RVar Double)
    | PrintModel
    | Train Int
    | Run (forall a. Floating a => Input m a)
    | EvalCost
    deriving (Typeable)

-- | Overwrites the model with a randomly generated one.
genModel :: RVar Double -> Command m
genModel = GenModel

-- | Prints the current model.
printModel :: Command m
printModel = PrintModel

-- | @train n@ trains the model for n iterations.
train :: Int -> Command m
train = Train

-- | Runs the model on an imput.
run :: (forall a. Floating a => Input m a) -> Command m
run = Run

-- | Evalutes the cost function for the current model.
evalCost :: Command m
evalCost = EvalCost
