{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Data.ML
import           Data.ML.Repl
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
    = MonoidHom (OrdDomain Letter) (Matrix' 5)
  :>> AffineMap (Matrix' 5) (V 5)
  :>> Over (V 5) Sigmoid
  :>> AffineMap (V 5) (V 5)
  :>> Over (V 5) Sigmoid
  :>> AffineMap (V 5) Scalar
  :>> Sigmoid

cost :: Cost ExampleModel
cost = logistic + 0.01 * l2reg

evenCount :: Char -> [Char] -> Bool
evenCount x xs = length (elemIndices x xs) `rem` 2 == 0

isAccepted :: String -> Bool
isAccepted s = evenCount 'a' s && evenCount 'b' s

dataset :: Vector (Const [Letter] Double, Scalar Double)
dataset = Vector.fromList
        $ map (bimap (Const . map Letter) Scalar)
        $ map (\s -> (s, if isAccepted s then 1 else 0))
        $ [0 .. 7] >>= flip replicateM ['a', 'b']

driver :: Driver ExampleModel Double ()
driver = do
    setDataSet dataset
    setCostFun cost
    runReplT (driverRepl "example/Example.hs")

main :: IO ()
main = runDriverT driver
