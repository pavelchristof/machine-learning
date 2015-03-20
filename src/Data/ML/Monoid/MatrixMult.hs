{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
module Data.ML.Monoid.MatrixMult where

import Control.Applicative
import Data.Foldable
import Data.Functor.Constant
import Data.Key
import Data.ML.Domain
import Data.ML.Matrix
import Data.ML.Model
import Data.Monoid
import Data.Proxy
import Data.Traversable
import GHC.TypeLits
import Linear

newtype MatrixMultModel dom (size :: Nat) a
    = MatrixMultModel (dom (MatrixLV' size a))
    deriving (Functor, Foldable, Traversable)

deriving instance (Show (dom (MatrixLV' size a))) => Show (MatrixMultModel dom size a)

instance (Applicative dom, KnownNat size) => Applicative (MatrixMultModel dom size) where
    pure = MatrixMultModel . pure . pure
    MatrixMultModel f <*> MatrixMultModel x = MatrixMultModel
        ((<*>) <$> f <*> x)

instance (Additive dom, KnownNat size) => Additive (MatrixMultModel dom size) where
    zero = MatrixMultModel zero
    liftU2 h (MatrixMultModel m) (MatrixMultModel m') =
        MatrixMultModel (liftU2 (liftU2 h) m m')
    liftI2 h (MatrixMultModel m) (MatrixMultModel m') =
        MatrixMultModel (liftI2 (liftI2 h) m m')

instance (DenseDomain dom, KnownNat size) => Model (MatrixMultModel dom size) where
    type Input (MatrixMultModel dom size) = Constant [Key dom]
    type Output (MatrixMultModel dom size) = MatrixLV' size

    predict (Constant input) (MatrixMultModel m) =
        MatrixLV $ getMatrixProd $ foldMap (MatrixProd . getMatrix . index m) input
