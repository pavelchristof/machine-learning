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

import Data.ML.Internal.Product
import Data.ML.Model
import Linear

-- | Folds the fixpoint of f to g using a model m.
--
-- The model m should transform @f g@ to @g@.
newtype Cata (f :: (* -> *) -> * -> *) (m :: * -> *) (g :: * -> *) (a :: *) = Cata (m a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

-- | Higher order functor fixpoint.
newtype Fix1 (f :: (* -> *) -> * -> *) (a :: *) = Fix1 (f (Fix1 f) a)

instance Functor1 f => Functor (Fix1 f) where
    fmap f (Fix1 x) = Fix1 (hrmap f x)

-- | Higher-order functor in the first parameter, usual functor in the second
-- parameter. The second parameter gets applies to the first one to form a
-- fully applied type. You can map over both parameters.
--
-- prop>
class Functor1 (n :: (* -> *) -> (* -> *)) where
    hlmap :: (g a -> h a) -> n g a -> n h a
    hrmap :: Functor g => (a -> b) -> n g a -> n g b

instance (Functor1 f, Model m, Input m ~ f g, Output m ~ g)
         => Model (Cata f m g) where
    type Input (Cata f m g) = Fix1 f
    type Output (Cata f m g) = g
    predict (Fix1 x) m@(Cata m') = predict (hlmap (`predict` m) x) m'

-- | Binary tree suitable for use with the catamorphism model.
data Tree g f a
    = Leaf (g a)
    | Node (f a) (f a)

instance (Functor g, Functor f) => Functor (Tree g f) where
    fmap f (Leaf x) = Leaf (fmap f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Functor g => Functor1 (Tree g) where
    hlmap f (Leaf g) = Leaf g
    hlmap f (Node l r) = Node (f l) (f r)

    hrmap f (Leaf g) = Leaf (fmap f g)
    hrmap f (Node l r) = Node (fmap f l) (fmap f r)

newtype TreeFAlgebra (leaf :: * -> *) (node :: * -> *) a
    = TreeFAlgebra (Product leaf node a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

instance ( Model leaf, Model node
         , Output node ~ Output leaf
         , Input node ~ Product (Output leaf) (Output leaf))
         => Model (TreeFAlgebra leaf node) where
    type Input (TreeFAlgebra leaf node) = Tree (Input leaf) (Output leaf)
    type Output (TreeFAlgebra leaf node) = Output leaf
    predict (Leaf x) (TreeFAlgebra (Pair leaf _)) = predict x leaf
    predict (Node l r) (TreeFAlgebra (Pair _ node)) = predict (Pair l r) node
