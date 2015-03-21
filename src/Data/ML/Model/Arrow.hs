{-# LANGUAGE TypeFamilies #-}
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

import Control.Category
import Data.ML.Model
import Linear

-- | A category of models.
data ModelCat x a b where
    WrapModel :: Model m => m x -> ModelCat x (Input m) (Output m)

instance Category (ModelCat x) where
    WrapModel m . WrapModel m' = WrapModel (m' :>> m)
    id = WrapModel (IdentityModel V0)
