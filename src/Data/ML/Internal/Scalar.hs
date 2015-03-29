{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{- |
Module      :  Data.ML.Internal.Scalar
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Internal.Scalar where

import Control.Applicative
import Data.Bytes.Serial
import Data.ML.Matrix
import Linear

-- | A scalar value, aka identity functor.
newtype Scalar a = Scalar { getScalar :: a }
    deriving (Show, Functor, Foldable, Traversable, Eq, Ord, Num, Fractional, Floating)

instance Applicative Scalar where
    pure a = Scalar a
    Scalar f <*> Scalar x = Scalar (f x)

instance Monad Scalar where
    return a = Scalar a
    m >>= k  = k (getScalar m)

instance Additive Scalar where
    zero = pure 0
    liftU2 = liftA2
    liftI2 = liftA2

instance Metric Scalar where
    dot (Scalar a) (Scalar b) = a * b

instance Multiplicative Scalar where
    one = pure 1
    (^*^) = liftA2 (*)

instance Serial1 Scalar where
    serializeWith f (Scalar x) = f x
    deserializeWith f = Scalar <$> f
