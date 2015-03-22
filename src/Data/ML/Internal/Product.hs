{- |
Module      :  Data.ML.Internal.Product
Description :  Functor product.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental
-}
module Data.ML.Internal.Product where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.Monoid ((<>))
import Data.Traversable
import Linear

data Product f g a = Pair (f a) (g a)

instance (Functor f, Functor g) => Functor (Product f g) where
    fmap f (Pair x y) = Pair (fmap f x) (fmap f y)

instance (Foldable f, Foldable g) => Foldable (Product f g) where
    foldMap f (Pair x y) = foldMap f x <> foldMap f y

instance (Traversable f, Traversable g) => Traversable (Product f g) where
    traverse f (Pair x y) = Pair <$> traverse f x <*> traverse f y

instance (Applicative f, Applicative g) => Applicative (Product f g) where
    pure x = Pair (pure x) (pure x)
    Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)

instance (Alternative f, Alternative g) => Alternative (Product f g) where
    empty = Pair empty empty
    Pair x1 y1 <|> Pair x2 y2 = Pair (x1 <|> x2) (y1 <|> y2)

instance (Additive f, Additive g) => Additive (Product f g) where
    zero = Pair zero zero
    Pair f g ^+^ Pair f' g' =
        Pair (f ^+^ f') (g ^+^ g')
    Pair f g ^-^ Pair f' g' =
        Pair (f ^-^ f') (g ^-^ g')
    lerp alpha (Pair f g) (Pair f' g') =
        Pair (lerp alpha f f') (lerp alpha g g')
    liftU2 h (Pair f g) (Pair f' g') =
        Pair (liftU2 h f f') (liftU2 h g g')
    liftI2 h (Pair f g) (Pair f' g') =
        Pair (liftI2 h f f') (liftI2 h g g')

instance (Metric f, Metric g) => Metric (Product f g) where
    dot (Pair f g) (Pair f' g') = dot f f' + dot g g'
    quadrance (Pair f g) = quadrance f + quadrance g
    qd (Pair f g) (Pair f' g') = qd f f' + qd g g'

instance (Serial1 f, Serial1 g) => Serial1 (Product f g) where
    serializeWith h (Pair f g) =
        serializeWith h f *>
        serializeWith h g
    deserializeWith h = Pair
        <$> deserializeWith h
        <*> deserializeWith h
