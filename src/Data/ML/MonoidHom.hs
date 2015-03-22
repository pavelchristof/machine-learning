{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Data.ML.MonoidHom
Description :  Monoid homomorphism model.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

All models in this category are monoid homomorphisms from
the free monoid over the given domain to some model dependent monoid.
-}
module Data.ML.MonoidHom where

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

instance Functor (FreeMonoid a) where
    fmap _ (FreeMonoid m) = FreeMonoid m

instance Monoid (FreeMonoid a b) where
    mempty = FreeMonoid (const mempty)
    FreeMonoid f `mappend` FreeMonoid g = FreeMonoid $ \m -> f m <> g m

instance Applicative (FreeMonoid a) where
    pure _ = mempty
    FreeMonoid f <*> FreeMonoid x = FreeMonoid $ \m -> f m <> x m

instance Foldable (FreeMonoid a) where
    foldMap _ _ = mempty

instance Traversable (FreeMonoid a) where
    traverse _ (FreeMonoid m) = pure (FreeMonoid m)

toFreeMonoid :: Foldable f => f a -> FreeMonoid a b
toFreeMonoid x = FreeMonoid (\f -> foldMap f x)

-- | A homomorphism from the free monoid over the given domain to a multiplicative monoid @m a@.
newtype MonoidHom dom m a = MonoidHom (Compose dom m a)
    deriving (Show, Functor, Applicative, Foldable, Traversable, Additive, Metric)

instance (Serial1 dom, Serial1 m) => Serial1 (MonoidHom dom m) where
    serializeWith f (MonoidHom m) = serializeWith f m
    deserializeWith f = MonoidHom <$> deserializeWith f

instance (Indexable dom, Multiplicative m) => Model (MonoidHom dom m) where
    type Input (MonoidHom dom m) = FreeMonoid (Key dom)
    type Output (MonoidHom dom m) = m

    predict (FreeMonoid input) (MonoidHom (Compose m)) =
        getProduct1 $ input (Product1 . index m)
