{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.ML.Internal.Product
import Data.ML.Model
import GHC.Generics
import Linear

class HFunctor (n :: (* -> *) -> (* -> *)) where
    hmap :: (g a -> h a) -> n g a -> n h a

class Functor1 (n :: (* -> *) -> (* -> *)) where
    map1 :: Functor g => (a -> b) -> n g a -> n g b

class Foldable1 (n :: (* -> *) -> (* -> *)) where
    foldHmap :: (Foldable g, Monoid m) => (a -> m) -> (n g a -> m)

class HSerial1 (n :: (* -> *) -> * -> *) where
    hserializeWith :: (MonadPut m, Serial1 f) => (a -> m ()) -> n f a -> m ()
    hdeserializeWith :: (MonadGet m, Serial1 f) => m a -> m (n f a)

-- | Higher order functor fixpoint.
newtype Fix1 (f :: (* -> *) -> * -> *) (a :: *) = Fix1 (f (Fix1 f) a)
    deriving (Generic, Generic1)

instance Functor1 f => Functor (Fix1 f) where
    fmap f (Fix1 x) = Fix1 (map1 f x)

instance Foldable1 f => Foldable (Fix1 f) where
    foldMap f (Fix1 x) = foldHmap f x

instance HSerial1 f => Serial1 (Fix1 f) where
    serializeWith f (Fix1 x) = hserializeWith f x
    deserializeWith f = Fix1 <$> hdeserializeWith f

cata1 :: HFunctor f => (f g a -> g a) -> Fix1 f a -> g a
cata1 f (Fix1 x) = f (hmap (cata1 f) x)

-- | Folds the fixpoint of f to g using a model m.
--
-- The model m should transform @f g@ to @g@.
newtype Cata (f :: (* -> *) -> * -> *) (m :: * -> *) (a :: *) = Cata (m a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric, Generic1)

instance Serial1 m => Serial1 (Cata f m)

instance (HFunctor f, Functor1 f, Model m, Input m ~ f (Output m))
         => Model (Cata f m) where
    type Input (Cata f m) = Fix1 f
    type Output (Cata f m) = Output m
    predict (Fix1 x) m@(Cata m') = predict (hmap (`predict` m) x) m'

-- | Binary tree suitable for use with the catamorphism model.
data Tree g f a
    = Leaf (g a)
    | Node (f a) (f a)
    deriving (Functor, Foldable, Traversable, Generic, Generic1)

instance (Serial (g a), Serial (f a), Serial a) => Serial (Tree g f a)
instance (Serial1 g, Serial1 f) => Serial1 (Tree g f)
instance Serial1 g => HSerial1 (Tree g) where
    hserializeWith f (Leaf x) = do
        putWord8 0
        serializeWith f x
    hserializeWith f (Node l r) = do
        putWord8 1
        serializeWith f l
        serializeWith f r
    hdeserializeWith f = do
        i <- getWord8
        case i of
          0 -> Leaf <$> deserializeWith f
          1 -> Node <$> deserializeWith f <*> deserializeWith f
          _ -> error "invalid data"

instance HFunctor (Tree g) where
    hmap _ (Leaf g) = Leaf g
    hmap f (Node l r) = Node (f l) (f r)

instance Functor g => Functor1 (Tree g) where
    map1 f (Leaf g) = Leaf (fmap f g)
    map1 f (Node l r) = Node (fmap f l) (fmap f r)

instance Foldable g => Foldable1 (Tree g) where
    foldHmap f (Leaf g) = foldMap f g
    foldHmap f (Node l r) = foldMap f l `mappend` foldMap f r

newtype TreeAlgebra (leaf :: * -> *) (node :: * -> *) a
    = TreeAlgebra (Product leaf node a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric, Generic1)

instance (Serial1 leaf, Serial1 node) => Serial1 (TreeAlgebra leaf node)

instance ( Model leaf, Model node
         , Output node ~ Output leaf
         , Input node ~ Product (Output leaf) (Output leaf))
         => Model (TreeAlgebra leaf node) where
    type Input (TreeAlgebra leaf node) = Tree (Input leaf) (Output leaf)
    type Output (TreeAlgebra leaf node) = Output leaf
    predict (Leaf x) (TreeAlgebra (Pair leaf _)) = predict x leaf
    predict (Node l r) (TreeAlgebra (Pair _ node)) = predict (Pair l r) node

newtype MapTree (f :: * -> *) (a :: *) = MapTree (f a)
    deriving (Functor, Applicative, Foldable, Traversable, Additive, Metric, Generic1)

instance Serial1 f => Serial1 (MapTree f)

instance Model f => Model (MapTree f) where
    type Input (MapTree f) = Fix1 (Tree (Input f))
    type Output (MapTree f) = Fix1 (Tree (Output f))
    predict (Fix1 (Leaf x)) (MapTree m) = Fix1 (Leaf (predict x m))
    predict (Fix1 (Node l r)) m = Fix1 (Node (predict l m) (predict r m))

listToTree :: g a -> [g a] -> Fix1 (Tree g) a
listToTree leaf [] = Fix1 $ Leaf leaf
listToTree leaf (x:xs) = Fix1 $ Node (Fix1 $ Leaf x) (listToTree leaf xs)

listToTree1 :: [g a] -> Fix1 (Tree g) a
listToTree1 [x] = Fix1 $ Leaf x
listToTree1 (x:xs) = Fix1 $ Node (Fix1 $ Leaf x) (listToTree1 xs)
listToTree1 _ = error "listToTree1: empty list"
