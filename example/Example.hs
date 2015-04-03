{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Data.ML
import           Data.Random
import           Data.ML.Repl
import           Data.Total.Array
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Control.Monad.IO.Class

newtype Letter = Letter Char
    deriving (Eq, Ord, Enum)

instance Show Letter where
    show (Letter l) = show l

instance Bounded Letter where
    minBound = Letter 'a'
    maxBound = Letter 'b'

type ExampleModel
    = MonoidHom (TotalArray Letter) (Matrix' 5)
  :>> AffineMap (Matrix' 5) Scalar
  :>> Sigmoid

cost :: Cost ExampleModel
cost = logistic + 0.01 * l2reg

evenCount :: Char -> [Char] -> Bool
evenCount x xs = length (elemIndices x xs) `rem` 2 == 0

isAccepted :: String -> Bool
isAccepted s = evenCount 'a' s && evenCount 'b' s

dataset :: Int -> Vector (Const [Letter] Double, Scalar Double)
dataset n = Vector.fromList
          $ map (bimap (Const . map Letter) Scalar)
          $ map (\s -> (s, if isAccepted s then 1 else 0))
          $ [0 .. n] >>= flip replicateM ['a', 'b']

isOk :: Scalar Double -> Scalar Double -> CountCorrect
isOk x y = if abs (x - y) < 0.5
              then correct
              else incorrect

driver :: Driver ExampleModel Double ()
driver = do
    setTrainingSet (dataset 6)
    setTestSet (dataset 15)
    setCostFun cost
    runReplT (driverRepl "example/Example.hs")

main :: IO ()
main = runDriverT driver
