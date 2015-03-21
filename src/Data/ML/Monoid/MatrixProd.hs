{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{- |
Module      :  Data.ML.Monoid.MatrixProd
Description :  Homomorphism to matrices with multiplication.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Monoid.MatrixProd where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.Key
import Data.ML.Domain
import Data.ML.Matrix
import Data.ML.Model
import Data.ML.Monoid
import Data.Traversable
import GHC.TypeLits
import Linear

-- | A homomorphism from the free monoid over the given domain
-- to the monoid of matrices under multiplication.
newtype MatrixProdModel dom (size :: Nat) a
    = MatrixProdModel (dom (MatrixLV' size a))
    deriving (Functor, Foldable, Traversable)

deriving instance (Show (dom (MatrixLV' size a))) => Show (MatrixProdModel dom size a)

instance (Applicative dom, KnownNat size) => Applicative (MatrixProdModel dom size) where
    pure = MatrixProdModel . pure . pure
    MatrixProdModel f <*> MatrixProdModel x = MatrixProdModel
        ((<*>) <$> f <*> x)

instance (Additive dom, KnownNat size) => Additive (MatrixProdModel dom size) where
    zero = MatrixProdModel zero
    liftU2 h (MatrixProdModel m) (MatrixProdModel m') =
        MatrixProdModel (liftU2 (liftU2 h) m m')
    liftI2 h (MatrixProdModel m) (MatrixProdModel m') =
        MatrixProdModel (liftI2 (liftI2 h) m m')

instance (Serial1 dom, KnownNat size) => Serial1 (MatrixProdModel dom size) where
    serializeWith f (MatrixProdModel m) = serializeWith (serializeWith f) m
    deserializeWith f = MatrixProdModel <$> deserializeWith (deserializeWith f)

instance (DenseDomain dom, KnownNat size) => Model (MatrixProdModel dom size) where
    type Input (MatrixProdModel dom size) = FreeMonoid (Key dom)
    type Output (MatrixProdModel dom size) = MatrixProd size

    predict (FreeMonoid input) (MatrixProdModel m) =
        input (MatrixProd . getMatrix . index m)
