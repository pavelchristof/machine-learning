{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Data.ML.Folds
Description :  Common folds.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Folds where

import Data.Bytes.Serial
import Data.ML.Model
import Data.ML.Scalar
import Linear

-- | Sums the structure.
newtype SumOf (f :: * -> *) a = SumOf' (V0 a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

pattern SumOf = SumOf' V0

instance (Serial1 f) => Serial1 (SumOf f) where
    serializeWith _ _ = return ()
    deserializeWith _ = return SumOf

instance (Functor f, Foldable f) => Model (SumOf f) where
    type Input (SumOf f) = f
    type Output (SumOf f) = Scalar
    predict f _ = Scalar (sum f)
