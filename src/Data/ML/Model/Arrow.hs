{-# LANGUAGE GADTs #-}
{- |
Module      :  Data.ML.Model.Arrow
Description :  Arrowized models.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

This module packs models in a GADT to make them an Arrow.
-}
module Data.ML.Model.Arrow where

import Data.ML.Model
import Data.Semigroupoid
import Linear

-- | A semigroupoid of models.
data ModelSemigroupoid x a b where
    WrapModel :: Model m => m x -> ModelSemigroupoid x (Input m x) (Output m x)

instance Semigroupoid (ModelSemigroupoid x) where
    WrapModel m `o` WrapModel m' = WrapModel (CompositeModel m' m)
