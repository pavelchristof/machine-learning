{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Data.ML.Internal.TH
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Internal.TH where

import Control.Applicative
import Data.Bytes.Serial
import Data.Coerce
import Data.Foldable
import Data.ML.Internal.Scalar
import Data.ML.Model
import Data.Traversable
import Language.Haskell.TH
import Linear

mkScalarModel :: ExpQ -> String -> Q [Dec]
mkScalarModel f name' = do
   let name = mkName name'
       a = mkName "a"
   v0 <- [t|V0 $(varT a)|]
   let dataDecl = NewtypeD
           [] name
           [PlainTV a]
           (NormalC name [(NotStrict, v0)])
           [ ''Functor, ''Applicative, ''Monad
           , ''Foldable, ''Traversable
           , ''Additive, ''Metric ]
   instances <- [d|
       instance Serial1 $(conT name) where
           serializeWith _ _ = return ()
           deserializeWith _ = return ($(conE name) V0)

       instance Model $(conT name) where
           type Input $(conT name) = Scalar
           type Output $(conT name) = Scalar
           predict x _ = Scalar $ $(f) $ getScalar x
       |]
   return (dataDecl : instances)
