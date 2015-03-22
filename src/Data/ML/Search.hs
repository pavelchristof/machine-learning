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
import Numeric.AD
import Numeric.AD.Internal.Reverse
import System.IO.Unsafe

deNaN :: RealFloat a => a -> a
deNaN x | isNaN x = 0
        | otherwise = x

-- | Adaptive gradient descent working on batches.
adaGrad :: forall f m a. Show a => Model m => Additive m => Traversable m => RealFloat a
        => Foldable f => Functor f => Functor (Output m) => Functor (Input m)
        => [f (Input m a, Output m a)] -- ^ List of mini batches
        -> Cost m                      -- ^ The cost function.
        -> m a                         -- ^ Starting point.
        -> [(a, m a)]                  -- ^ A list of models with their cost.
adaGrad i c m0 = tail $ fmap (\(j, m, _, _) -> (j, m))
               $ scanl step (0, m0, m0, zero) i
  where
    -- (prevCost, prevModel, model, historicalGrad)
    step :: (a, m a, m a, m a)
         -> f (Input m a, Output m a)
         -> (a, m a, m a, m a)
    step (_, _, model, historicalGrad) batch =
      let -- Prepare the batch for differentiation.
          batch' :: forall s. Reifies s Tape
                 => f (Input m (Reverse s a), Output m (Reverse s a))
          batch' = fmap (bimap (fmap auto) (fmap auto)) batch

          cost :: forall s. Reifies s Tape
               => m (Reverse s a) -> Reverse s a
          cost m = getCost c (fmap (first (`predict` m)) batch') m

          -- Compute the gradient on this batch.
          (j, g) = grad' cost model

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
