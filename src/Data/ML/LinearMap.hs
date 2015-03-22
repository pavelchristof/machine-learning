{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{- |
Module      :  Data.ML.LinearMap
Description :  Linear map model.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.LinearMap where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.ML.Internal.Compose
import Data.ML.Model
import Data.Traversable
import Linear

-- | A linear map.
newtype LinearMap f g a = LinearMap (Compose g f a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

instance (Serial1 f, Serial1 g) => Serial1 (LinearMap f g) where
    serializeWith f (LinearMap m) = serializeWith f m
    deserializeWith f = LinearMap <$> deserializeWith f

instance (Metric f, Functor g) => Model (LinearMap f g) where
    type Input (LinearMap f g) = f
    type Output (LinearMap f g) = g
    predict f (LinearMap (Compose m)) = fmap (dot f) m
