{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Data.ML.Linear where

import Data.ML.Matrix
import Data.ML.Model
import GHC.TypeLits
import Linear

data LinearModel f g a = LinearModel (g (f a))

instance (Metric f, Functor g) => Model (LinearModel f g) where
    type Input (LinearModel f g) = f
    type Output (LinearModel f g) = g
    predict f (LinearModel m) = fmap (dot f) m
