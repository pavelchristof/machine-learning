{-# LANGUAGE GADTs #-}
module Data.ML.Model.Dynamic where

import Data.Foldable
import Data.ML.Model
import Data.Proxy
import Linear

data AModel a b where
    AModel :: Model m => Proxy m -> AModel (Input m) (Output m)

-- | Chains two dynamic models.
(@>) :: AModel a b -> AModel b c -> AModel a c
AModel (_ :: Proxy f) @> AModel (_ :: Proxy g) =
    AModel (Proxy :: Proxy (CompositeModel f g))

data ModelInst x a b where
    ModelInst :: Model m => m x -> ModelInst x (Input m) (Output m)

instModel :: forall x a b. Num x => AModel a b -> ModelInst x a b
instModel (AModel (_ :: Proxy m)) = ModelInst (zero :: m x)

printModel :: Show x => ModelInst x a b -> IO ()
printModel (ModelInst m) = traverse_ print m
