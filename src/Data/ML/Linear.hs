{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{- |
Module      :  Data.ML.Linear
Description :  Linear map model.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Linear where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.ML.Internal.Compose
import Data.ML.Model
import Data.Traversable
import Linear

-- | A linear map.
newtype LinearModel f g a = LinearModel (Compose g f a)
    deriving (Show, Functor, Applicative, Foldable, Traversable, Additive, Metric)

instance (Serial1 f, Serial1 g) => Serial1 (LinearModel f g) where
    serializeWith f (LinearModel m) = serializeWith f m
    deserializeWith f = LinearModel <$> deserializeWith f

instance (Metric f, Functor g) => Model (LinearModel f g) where
    type Input (LinearModel f g) = f
    type Output (LinearModel f g) = g
    predict f (LinearModel (Compose m)) = fmap (dot f) m
