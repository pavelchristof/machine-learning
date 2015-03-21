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
import Data.Foldable
import Data.Traversable

-- | A scalar value, aka identity functor.
newtype Scalar a = Scalar { getScalar :: a }
    deriving (Functor, Foldable, Traversable)

instance Applicative Scalar where
    pure a = Scalar a
    Scalar f <*> Scalar x = Scalar (f x)

instance Monad Scalar where
    return a = Scalar a
    m >>= k  = k (getScalar m)
