{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Data.ML.Domain
import Data.ML.Matrix
import Data.ML.Model
import Data.Monoid
import Data.Traversable
import GHC.TypeLits
import Linear
import Prelude hiding (sum)

-- | A free monoid over a, with an extra unused type parameter to match
-- the kind of model input.
newtype FreeMonoid a b = FreeMonoid (forall m. Monoid m => (a -> m) -> m)

toFreeMonoid :: Foldable f => f a -> FreeMonoid a b
toFreeMonoid x = FreeMonoid (\f -> foldMap f x)

-- | A homomorphism from the free monoid over the given domain to a monoid @m a@.
newtype MonoidHomModel dom m a
    = MonoidHomModel (dom (m a))
    deriving (Functor, Foldable, Traversable)

deriving instance (Show (dom (m a))) => Show (MonoidHomModel dom m a)

instance (Applicative dom, Applicative m) => Applicative (MonoidHomModel dom m) where
    pure = MonoidHomModel . pure . pure
    MonoidHomModel f <*> MonoidHomModel x = MonoidHomModel
        ((<*>) <$> f <*> x)

instance (Additive m, Applicative dom) => Additive (MonoidHomModel dom m) where
    zero = MonoidHomModel (pure zero)
    liftU2 f (MonoidHomModel m) (MonoidHomModel m') =
        MonoidHomModel (liftA2 (liftU2 f) m m')
    liftI2 f (MonoidHomModel m) (MonoidHomModel m') =
        MonoidHomModel (liftA2 (liftI2 f) m m')

instance (Metric m, Applicative dom, Foldable dom) => Metric (MonoidHomModel dom m) where
    dot (MonoidHomModel m) (MonoidHomModel m') = sum (dot <$> m <*> m')

instance (Serial1 dom, Serial1 m) => Serial1 (MonoidHomModel dom m) where
    serializeWith f (MonoidHomModel m) = serializeWith (serializeWith f) m
    deserializeWith f = MonoidHomModel <$> deserializeWith (deserializeWith f)

instance (Indexable dom, Multiplicative m) => Model (MonoidHomModel dom m) where
    type Input (MonoidHomModel dom m) = FreeMonoid (Key dom)
    type Output (MonoidHomModel dom m) = m

    predict (FreeMonoid input) (MonoidHomModel m) =
        getProduct1 $ input (Product1 . index m)
