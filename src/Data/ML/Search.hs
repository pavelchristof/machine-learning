{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoPatBinds #-}
{- |
Module      :  Data.ML.Search
Description :  Optimum search.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Search where

import Control.Applicative
import Data.Bifunctor
import Data.Foldable
import Data.ML.Cost
import Data.ML.Model
import Data.Reflection
import Data.Traversable
import Linear

-- | Adaptive gradient descent working on batches.
adaGrad :: RealFloat a
        => Model m => Additive m => Traversable m
        => Foldable f => Functor f
        => [f (Input m a, Output m a)] -- ^ List of mini batches
        -> Cost m                      -- ^ The cost function.
        -> m a                         -- ^ Starting point.
        -> [(a, m a)]                  -- ^ A list of models with their cost.
adaGrad i cost m0 = tail $ fmap (\(j, m, _, _) -> (j, m))
                  $ scanl step (0, m0, m0, zero) i
  where
    -- (prevCost, prevModel, model, historicalGrad)
    step (_, _, model, historicalGrad) batch =
      let -- Compute the gradient on this batch.
          (j, g) = evalCostGrad cost batch model

          -- Update the accumulated gradient.
          historicalGrad' = historicalGrad ^+^ liftU2 (*) g g

          -- Adjust the current gradient.
          adjustedGrad = liftI2 (/) g
              (fmap (\x -> sqrt (x + fudgeFactor)) historicalGrad')

          -- Compute the new model.
          model' = model ^-^ stepSize *^ adjustedGrad

      in (j, model, model', historicalGrad')

    fudgeFactor = 1e-6
    stepSize = 1e-1
