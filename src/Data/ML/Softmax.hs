{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Data.ML.Softmax
Description :  Softmax model.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Softmax where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.ML.Internal.Compose
import Data.ML.Model
import Data.Traversable
import Linear
import Prelude hiding (sum)

-- | Applies the softmax transformation.
newtype Softmax (f :: * -> *) a = Softmax (V0 a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

instance (Serial1 f) => Serial1 (Softmax f) where
    serializeWith _ _ = return ()
    deserializeWith _ = return (Softmax V0)

instance (Functor f, Foldable f) => Model (Softmax f) where
    type Input (Softmax f) = f
    type Output (Softmax f) = f
    predict f _ = fmap (\x -> exp x / total) f
      where total = sum (fmap exp f)
