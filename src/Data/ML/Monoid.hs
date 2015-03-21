{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Data.ML.Monoid
Description :  Monoid homomorphism models.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

All models in this category are monoid homomorphisms from
the free monoid over the given domain to some model dependent monoid.
-}
module Data.ML.Monoid where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.Key
import Data.ML.Internal.Compose
import Data.ML.Matrix
import Data.ML.Model
import Data.Monoid
import Data.Traversable
import Linear

-- | A free monoid over a, with an extra unused type parameter to match
-- the kind of model input.
newtype FreeMonoid a b = FreeMonoid (forall m. Monoid m => (a -> m) -> m)

toFreeMonoid :: Foldable f => f a -> FreeMonoid a b
toFreeMonoid x = FreeMonoid (\f -> foldMap f x)

-- | A homomorphism from the free monoid over the given domain to a monoid @m a@.
newtype MonoidHomModel dom m a
    = MonoidHomModel (Compose dom m a)
    deriving (Show, Functor, Applicative, Foldable, Traversable, Additive, Metric)

instance (Serial1 dom, Serial1 m) => Serial1 (MonoidHomModel dom m) where
    serializeWith f (MonoidHomModel m) = serializeWith f m
    deserializeWith f = MonoidHomModel <$> deserializeWith f

instance (Indexable dom, Multiplicative m) => Model (MonoidHomModel dom m) where
    type Input (MonoidHomModel dom m) = FreeMonoid (Key dom)
    type Output (MonoidHomModel dom m) = m

    predict (FreeMonoid input) (MonoidHomModel (Compose m)) =
        getProduct1 $ input (Product1 . index m)
