{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Data.ML.Model
Description :  Model class.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

-}
module Data.ML.Model where

import Data.Bytes.Serial
import Data.ML.Internal.Product
import Data.Vector
import Linear

-- | A machine learning model.
class (Functor (Input m), Functor (Output m)) => Model m where
    type Input m :: * -> *
    type Output m :: * -> *
    predict :: Floating a => Input m a -> m a -> Output m a

-- | A data set is a vector of (input, output) pairs.
type DataSet m a = Vector (Input m a, Output m a)

-- | Generates a model with an applicative.
generate :: (Applicative f, Applicative g, Traversable g) => f a -> f (g a)
generate f = traverse (const f) (pure ())

-- | Chaining of models.
newtype (f :>> g) a = Chain (Product f g a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

pattern f :>> g = Chain (Pair f g)

instance (Serial1 f, Serial1 g) => Serial1 (f :>> g) where
    serializeWith f (Chain m) = serializeWith f m
    deserializeWith f = Chain <$> deserializeWith f

instance (Model f, Model g, Output f ~ Input g) => Model (f :>> g) where
    type Input (f :>> g) = Input f
    type Output (f :>> g) = Output g
    predict x (f :>> g) = predict (predict x f) g
