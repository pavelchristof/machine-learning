{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{- |
Module      :  Data.ML.Matrix
Description :  Matrices with size known at compile time.
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
import Data.Monoid
import Data.Traversable
import GHC.TypeLits
import Linear
import Linear.V

-- | A known size matrix.
type Matrix (rows :: Nat) (cols :: Nat) a = V rows (V cols a)

-- | A known size square matrix.
type Matrix' (size :: Nat) a = Matrix size size a

-- | Entrywise function application.
(!$!) :: KnownNat rows => KnownNat cols => Matrix rows cols (a -> b)
      -> Matrix rows cols a -> Matrix rows cols b
(!$!) = liftI2 (liftI2 ($))

-- | A linear view of a matrix.
newtype MatrixLV (rows :: Nat) (cols :: Nat) a
    = MatrixLV { getMatrix :: Matrix rows cols a }
    deriving (Show, Num, Fractional, Floating)

-- | A linear view of a square matrix.
type MatrixLV' (size :: Nat) = MatrixLV size size

instance (KnownNat rows, KnownNat cols) => Functor (MatrixLV rows cols) where
    fmap f (MatrixLV m) = MatrixLV $ fmap (fmap f) m

instance (KnownNat rows, KnownNat cols) => Applicative (MatrixLV rows cols) where
    pure = MatrixLV . pure . pure
    MatrixLV f <*> MatrixLV x = MatrixLV (f !$! x)

instance (KnownNat rows, KnownNat cols) => Foldable (MatrixLV rows cols) where
    foldMap f (MatrixLV m) = foldMap (foldMap f) m

instance (KnownNat rows, KnownNat cols) => Traversable (MatrixLV rows cols) where
    traverse f (MatrixLV m) = MatrixLV
        <$> traverse (traverse f) m

instance (KnownNat rows, KnownNat cols) => Serial1 (MatrixLV rows cols) where
    serializeWith f (MatrixLV m) = serializeWith (serializeWith f) m
    deserializeWith f = MatrixLV <$> deserializeWith (deserializeWith f)

instance (KnownNat rows, KnownNat cols) => Additive (MatrixLV rows cols) where
    zero = MatrixLV zero
    MatrixLV m ^+^ MatrixLV m' =
        MatrixLV (m ^+^ m')
    lerp alpha (MatrixLV m) (MatrixLV m') =
        MatrixLV (liftI2 (lerp alpha) m m')
    liftU2 h (MatrixLV m) (MatrixLV m') =
        MatrixLV (liftU2 (liftU2 h) m m')
    liftI2 h (MatrixLV m) (MatrixLV m') =
        MatrixLV (liftI2 (liftI2 h) m m')

-- | Monoid of matrices under multiplication.
newtype MatrixProd (size :: Nat) a =
    MatrixProd { getMatrixProd :: Matrix' size a }

instance (KnownNat size, Num a) => Monoid (MatrixProd size a) where
    mempty = MatrixProd identity
    MatrixProd m `mappend` MatrixProd m' = MatrixProd (m !*! m')
