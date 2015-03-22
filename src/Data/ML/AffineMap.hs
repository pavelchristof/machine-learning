{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Data.ML.AffineMap
Description :  Affine map model.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.AffineMap where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.ML.Internal.Compose
import Data.ML.Internal.Product
import Data.ML.Matrix
import Data.ML.Model
import Data.ML.Scalar
import Data.Traversable
import Linear

-- | A structure with added bias.
newtype Biased f a = Biased (Product f Scalar a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

instance (Serial1 f) => Serial1 (Biased f) where
    serializeWith f (Biased m) = serializeWith f m
    deserializeWith f = Biased <$> deserializeWith f

-- | An affine map.
newtype AffineMap f g a = AffineMap (Compose g (Biased f) a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

instance (Serial1 f, Serial1 g) => Serial1 (AffineMap f g) where
    serializeWith f (AffineMap m) = serializeWith f m
    deserializeWith f = AffineMap <$> deserializeWith f

instance (Metric f, Functor g) => Model (AffineMap f g) where
    type Input (AffineMap f g) = f
    type Output (AffineMap f g) = g
    predict f (AffineMap (Compose m)) = fmap (dot (Biased (Pair f one))) m
