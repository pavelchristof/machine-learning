{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Data.ML.Index
Description :  Key-value map model.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Index where

import Control.Applicative
import Data.Bytes.Serial
import Data.Key
import Data.ML.Internal.Compose
import Data.ML.Model
import Linear

-- | A map from some domain dom to g.
newtype Index dom g a = Index' (Compose dom g a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

pattern Index m = Index' (Compose m)

instance (Serial1 dom, Serial1 g) => Serial1 (Index dom g) where
    serializeWith f (Index' m) = serializeWith f m
    deserializeWith f = Index' <$> deserializeWith f

instance (Indexable dom, Functor g) => Model (Index dom g) where
    type Input (Index dom g) = Const (Key dom)
    type Output (Index dom g) = g
    predict (Const i) (Index m) = index m i
