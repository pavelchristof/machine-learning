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

import Data.Bytes.Serial
import Data.ML.Internal.Product
import Data.ML.Model
import Linear

-- | Folds the fixpoint of f to g using a model m.
--
-- The model m should transform @f g@ to @g@.
newtype Cata (f :: (* -> *) -> * -> *) (m :: * -> *) (a :: *) = Cata (m a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

-- | Higher order functor fixpoint.
newtype Fix1 (f :: (* -> *) -> * -> *) (a :: *) = Fix1 (f (Fix1 f) a)

cata1 :: Functor1 f => (f g a -> g a) -> Fix1 f a -> g a
cata1 f (Fix1 x) = f (hlmap (cata1 f) x)

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

instance Serial1 m => Serial1 (Cata f m) where
    serializeWith f (Cata m) = serializeWith f m
    deserializeWith f = Cata <$> deserializeWith f

instance (Functor1 f, Model m, Input m ~ f (Output m))
         => Model (Cata f m) where
    type Input (Cata f m) = Fix1 f
    type Output (Cata f m) = Output m
    predict (Fix1 x) m@(Cata m') = predict (hlmap (`predict` m) x) m'

-- | Binary tree suitable for use with the catamorphism model.
data Tree g f a
    = Leaf (g a)
    | Node (f a) (f a)

instance (Functor g, Functor f) => Functor (Tree g f) where
    fmap f (Leaf x) = Leaf (fmap f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Functor g => Functor1 (Tree g) where
    hlmap _ (Leaf g) = Leaf g
    hlmap f (Node l r) = Node (f l) (f r)

    hrmap f (Leaf g) = Leaf (fmap f g)
    hrmap f (Node l r) = Node (fmap f l) (fmap f r)

newtype TreeAlgebra (leaf :: * -> *) (node :: * -> *) a
    = TreeAlgebra (Product leaf node a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric)

instance (Serial1 leaf, Serial1 node) => Serial1 (TreeAlgebra leaf node) where
    serializeWith f (TreeAlgebra m) = serializeWith f m
    deserializeWith f = TreeAlgebra <$> deserializeWith f

instance ( Model leaf, Model node
         , Output node ~ Output leaf
         , Input node ~ Product (Output leaf) (Output leaf))
         => Model (TreeAlgebra leaf node) where
    type Input (TreeAlgebra leaf node) = Tree (Input leaf) (Output leaf)
    type Output (TreeAlgebra leaf node) = Output leaf
    predict (Leaf x) (TreeAlgebra (Pair leaf _)) = predict x leaf
    predict (Node l r) (TreeAlgebra (Pair _ node)) = predict (Pair l r) node
