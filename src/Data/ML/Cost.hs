{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Data.ML.Cost
Description :  Cost functions.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Cost (
    Cost(..),

    -- | Classifictation.
    logistic,

    -- | Regularization.
    l1reg,
    l2reg
    ) where

import Control.Applicative
import Data.Foldable
import Data.ML.Model
import Data.Monoid
import Debug.Trace
import Linear
import Prelude hiding (sum)

-- | A cost function for model m is a function taking  the actual
-- output, the expected output, the model and returning a scalar cost.
newtype Cost m = Cost
    { getCost :: forall f a. Foldable f => Floating a => Show a
              => f (Output m a, Output m a) -> m a -> a }

liftCost :: (forall a. Floating a => a -> a) -> Cost m -> Cost m
liftCost f (Cost j) = Cost (\s m -> f (j s m))

liftCost2 :: (forall a. Floating a => a -> a -> a) -> Cost m -> Cost m -> Cost m
liftCost2 f (Cost j) (Cost j') = Cost (\s m -> f (j s m) (j' s m))

instance Num (Cost m) where
    (+) = liftCost2 (+)
    (-) = liftCost2 (-)
    (*) = liftCost2 (*)
    negate = liftCost negate
    abs = liftCost abs
    signum = liftCost signum
    fromInteger x = Cost (\_ _ -> fromInteger x)

instance Fractional (Cost m) where
    (/) = liftCost2 (/)
    recip = liftCost recip
    fromRational r = Cost (\_ _ -> fromRational r)

instance Floating (Cost m) where
    pi = Cost (\_ _ -> pi)
    exp = liftCost exp
    sqrt = liftCost sqrt
    log = liftCost log
    (**) = liftCost2 (**)
    logBase = liftCost2 logBase
    sin = liftCost sin
    tan = liftCost tan
    cos = liftCost cos
    asin = liftCost asin
    atan = liftCost atan
    acos = liftCost acos
    sinh = liftCost sinh
    tanh = liftCost tanh
    cosh = liftCost cosh
    asinh = liftCost asinh
    atanh = liftCost atanh
    acosh = liftCost acosh

-- TODO: research monoidal means
data Mean a = Mean a Int

mean :: a -> Mean a
mean x = Mean x 1

getMean :: Fractional a => Mean a -> a
getMean (Mean x n) = x / fromIntegral n

instance Num a => Monoid (Mean a) where
    mempty = Mean 0 0
    Mean x n `mappend` Mean y m = Mean (x + y) (n + m)

-- | Logistic cost function.
logistic :: Applicative (Output m) => Foldable (Output m) => Cost m
logistic = Cost (\s _ -> - getMean (foldMap (foldMap mean . uncurry (liftA2 j)) s))
  where
    j actual expected = Debug.Trace.traceShow actual $ expected * log (actual + epsilon)
                      + (1 - expected) * log (1 - actual + epsilon)
    epsilon :: Floating a => a
    epsilon = 1e-6

-- | L1 regularization.
l1reg :: Metric m => Cost m
l1reg = Cost (\_ m -> norm m)

-- | L2 regularization.
l2reg :: Metric m => Cost m
l2reg = Cost (\_ m -> quadrance m)
