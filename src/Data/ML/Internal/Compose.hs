{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{- |
Module      :  Data.ML.Internal.Compose
Description :  Functor composition.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Internal.Compose where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.Traversable
import Linear
import Prelude hiding (sum)

-- | Functor composition.
newtype Compose f g a = Compose (f (g a))
    deriving (Show, Functor, Foldable, Traversable)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = Compose . pure . pure
    Compose f <*> Compose x = Compose
        ((<*>) <$> f <*> x)

instance (Applicative f, Additive g) => Additive (Compose f g) where
    zero = Compose (pure zero)
    liftU2 f (Compose m) (Compose m') =
        Compose (liftA2 (liftU2 f) m m')
    liftI2 f (Compose m) (Compose m') =
        Compose (liftA2 (liftI2 f) m m')

instance (Applicative f, Foldable f, Metric g) => Metric (Compose f g) where
    dot (Compose m) (Compose m') = sum (dot <$> m <*> m')

instance (Serial1 f, Serial1 g) => Serial1 (Compose f g) where
    serializeWith f (Compose m) = serializeWith (serializeWith f) m
    deserializeWith f = Compose <$> deserializeWith (deserializeWith f)
