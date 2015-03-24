{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Data.ML.Cata
Description :  Catamorphism (generalized fold) model.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Cata where

import Control.Applicative
import Data.Bifunctor
import Data.Foldable
import Data.ML.Model
import Data.ML.Scalar
import Data.Traversable
import Linear

-- | Folds the fixpoint of f to g using a model m.
--
-- The model m should transform @f g@ to @g@.
newtype Cata (f :: (* -> *) -> * -> *) (m :: * -> *) (g :: * -> *) (a :: *) = Cata (m a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

-- | Higher order functor fixpoint.
newtype Fix1 (f :: (* -> *) -> * -> *) (a :: *) = Fix1 (f (Fix1 f) a)

-- | Higher-order functor in the first parameter, usual functor in the second
-- parameter. The second parameter gets applies to the first one to form a
-- fully applied type. You can map over both parameters.
class HBiFunctor (f :: (* -> *) -> * -> *) where
    -- | Maps over the first parameter of @f@.
    hlmap :: (g a -> h a) -> f g a -> f h a

    -- | Maps over the second parameter of @f@.
    hrmap :: Functor g => (a -> b) -> f g a -> f g b

instance (HBiFunctor f, Model m, Input m ~ f g, Output m ~ g)
         => Model (Cata f m g) where
    type Input (Cata f m g) = Fix1 f
    type Output (Cata f m g) = g
    predict (Fix1 x) m@(Cata m') = predict (hlmap (`predict` m) x) m'

-- | Binary tree suitable for use with the catamorphism model.
data Tree g f a
    = Leaf (g a)
    | Node (f a) (f a)

instance Functor g => HBiFunctor (Tree g) where
    hlmap f (Leaf g) = Leaf g
    hlmap f (Node l r) = Node (f l) (f r)

    hrmap f (Leaf g) = Leaf (fmap f g)
    hrmap f (Node l r) = Node (fmap f l) (fmap f r)
