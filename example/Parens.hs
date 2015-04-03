{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
import           Control.Monad
import           Data.Bifunctor
import           Data.ML
import           Data.Random
import           Data.ML.Repl
import           Data.String
import           Data.Bytes.Serial
import           GHC.Generics
import           Data.Total.Array
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

data Paren = Open | Close
    deriving (Eq, Ord, Enum, Bounded, Generic)

instance Serial Paren

instance Show Paren where
    show Open = "("
    show Close = ")"
    showList xs s = foldMap show xs ++ s

instance IsString [Paren] where
    fromString = map fromChar
      where fromChar '(' = Open
            fromChar ')' = Close
            fromChar _   = error "Not a paren."

type ExampleModel
    = MonoidHom (TotalArray Paren) (Matrix' 6)
  :>> AffineMap (Matrix' 6) Scalar
  :>> Sigmoid

cost :: Cost ExampleModel
cost = logistic + 0.1 * l2reg

balanced :: [Paren] -> Bool
balanced xs0 = go xs0 0
  where
    go [] 0 = True
    go [] _ = False
    go (Open:xs) n = go xs (n+1)
    go (Close:_) 0 = False
    go (Close:xs) n = go xs (n-1)

dataset :: Int -> Vector (Const [Paren] Double, Scalar Double)
dataset n = Vector.fromList
          $ map (bimap Const Scalar)
          $ map (\s -> (s, if balanced s then 1 else 0))
          $ [0 .. n] >>= flip replicateM "()"

isOk :: Scalar Double -> Scalar Double -> CountCorrect
isOk x y = if abs (x - y) < 0.5
              then correct
              else incorrect

driver :: Driver ExampleModel Double ()
driver = do
    setTrainingSet (dataset 6)
    setTestSet (dataset 15)
    setCostFun cost

    load "models/parens.bm"
    timed (test isOk)

    runReplT (driverRepl "example/Parens.hs")

main :: IO ()
main = runDriverT driver
