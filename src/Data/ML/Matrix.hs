{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{- |
Module      :  Data.ML.Matrix
Description :  Matrices and multiplicative functors.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

Provides matrices with size known at compile time and some useful wrappers.
-}
module Data.ML.Matrix where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.ML.Model
import Data.Monoid
import Data.Traversable
import GHC.TypeLits
import Linear
import Linear.V

infixl 7 ^*^

-- | A multiplicative monoid over a ring.
class Functor f => Multiplicative f where
    one :: Num a => f a
    (^*^) :: Num a => f a -> f a -> f a

-- | A matrix with size known at compile time.
newtype Matrix (rows :: Nat) (cols :: Nat) a
    = Matrix (V rows (V cols a))
    deriving (Show, Num, Fractional, Floating, Functor, Foldable, Traversable)

-- | A square matrix with size known at compile time.
type Matrix' (size :: Nat) = Matrix size size

instance (KnownNat rows, KnownNat cols) => Applicative (Matrix rows cols) where
    pure = Matrix . pure . pure
    Matrix f <*> Matrix x = Matrix ((<*>) <$> f <*> x)

instance (KnownNat rows, KnownNat cols) => Serial1 (Matrix rows cols) where
    serializeWith f (Matrix m) = serializeWith (serializeWith f) m
    deserializeWith f = Matrix <$> deserializeWith (deserializeWith f)

instance (KnownNat rows, KnownNat cols) => Additive (Matrix rows cols) where
    zero = Matrix zero
    Matrix m ^+^ Matrix m' =
        Matrix (m ^+^ m')
    lerp alpha (Matrix m) (Matrix m') =
        Matrix (liftI2 (lerp alpha) m m')
    liftU2 h (Matrix m) (Matrix m') =
        Matrix (liftU2 (liftU2 h) m m')
    liftI2 h (Matrix m) (Matrix m') =
        Matrix (liftI2 (liftI2 h) m m')

instance (KnownNat rows, KnownNat cols) => Metric (Matrix rows cols)

instance (KnownNat size) => Multiplicative (Matrix size size) where
    one = Matrix identity
    Matrix m ^*^ Matrix m' = Matrix (m !*! m')

-- | Higher order monoid under addition.
newtype Sum1 f a = Sum1 { getSum1 :: f a }

instance (Additive f, Num a) => Monoid (Sum1 f a) where
    mempty = Sum1 zero
    Sum1 m `mappend` Sum1 m' = Sum1 (m ^+^ m')

-- | Higher order monoid under multiplication.
newtype Product1 f a = Product1 { getProduct1 :: f a }

instance (Multiplicative f, Num a) => Monoid (Product1 f a) where
    mempty = Product1 one
    Product1 m `mappend` Product1 m' = Product1 (m ^*^ m')
