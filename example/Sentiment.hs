{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Bifunctor
import           Data.Bytes.Serial
import           Data.Char
import           Data.Foldable
import           Data.ML
import           Data.Random
import           Data.Reflection
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Total.Array.Subset
import           Data.Typeable
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           GHC.Generics
import           System.Exit

--------------------------------------------------------------------------------
-- Data structures
--------------------------------------------------------------------------------

data Tagged (tag :: *) (f :: (* -> *) -> * -> *)
            (g :: * -> *) (a :: *) = Tagged !tag !(f g a)
    deriving (Functor, Foldable, Traversable)

instance Functor1 f => Functor1 (Tagged tag f) where

type TreeBankF = Tree (Const Text)
type TaggedTreeBankF a = Tagged a (Tree (Const Text))
type SentimentsF = Tagged Double (Tree V0)

type TreeBank = Fix1 TreeBankF
type TaggedTreeBank a = Fix1 (TaggedTreeBankF a)
type Sentiments = Fix1 SentimentsF

untag :: Fix1 (Tagged tag f) a -> Fix1 f a
untag _ = _

onlyTags :: TaggedTreeBank Double a -> Sentiments a
onlyTags = _

mapTag :: (tag -> tag') -> Fix1 (Tagged tag g) a -> Fix1 (Tagged tag' g) a
mapTag f = refix _

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

type WordVec = Scalar

type VecTable s = Index (TotalSubsetArray s Text) WordVec
type Combinator = AffineMap (Product WordVec WordVec) WordVec :>> Over WordVec Tanh

newtype Fold s a = Fold (Product (VecTable s) Combinator a)
    deriving (Functor, Foldable, Traversable, Generic1)

deriving instance Subset s Text => Applicative (Fold s)
deriving instance Subset s Text => Additive (Fold s)
deriving instance Subset s Text => Metric (Fold s)
instance Subset s Text => Serial1 (Fold s)

instance Subset s Text => Model (Fold s) where
    type Input (Fold s) = TreeBankF Sentiments
    type Output (Fold s) = Sentiments

type SentimentModel s
    = Cata TreeBankF (Fold s)

--------------------------------------------------------------------------------
-- Cost function
--------------------------------------------------------------------------------

cost :: Subset s Text => Cost (SentimentModel s)
cost =  0.01 * l2reg

--------------------------------------------------------------------------------
-- Driver
--------------------------------------------------------------------------------

driver :: Subset s Text
       => DataSet (SentimentModel s) Double -- ^ The training set.
       -> DataSet (SentimentModel s) Double -- ^ The test set.
       -> Proxy s
       -> Driver (SentimentModel s) Double ()
driver trainingSet testSet _ = do
      setCostFun cost
      setTrainingSet trainingSet
      setTestSet testSet

      genModel (uniform (-0.01) 0.01)
      timed (train 30)

      save "models/sentiment.bm"

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

treeBankLeaf :: Parser (TaggedTreeBank Int a)
treeBankLeaf = do
    s <- decimal
    skipSpace
    t <- takeTill (== ')')
    return (Fix1 $ Tagged s (Leaf (Const t)))

treeBankNode :: Parser (TaggedTreeBank Int a)
treeBankNode = do
    s <- decimal
    skipSpace
    l <- treeBank
    skipSpace
    r <- treeBank
    return (Fix1 $ Tagged s (Node l r))

treeBank :: Parser (TaggedTreeBank Int a)
treeBank = do
    char '('
    v <- treeBankNode <|> treeBank
    char ')'
    return v

treeBanks :: Parser [TaggedTreeBank Int a]
treeBanks = sepBy' treeBank endOfLine

parseFile :: FilePath -> IO [TaggedTreeBank Int a]
parseFile filePath = do
    content <- Text.readFile filePath
    let result = parseOnly treeBanks content
    either die return result

--------------------------------------------------------------------------------
-- Preprocessing
--------------------------------------------------------------------------------

preprocess :: Text -> Text
preprocess = Text.toLower

sentToReal :: Int -> Double
sentToReal 0 = 1 / 10
sentToReal 1 = 3 / 10
sentToReal 2 = 5 / 10
sentToReal 3 = 7 / 10
sentToReal 4 = 9 / 10
sentToReal _ = error "Wrong sentiment."

sentFromReal :: Double -> Int
sentFromReal x
    | x >= 0/5 && x <  1/5 = 0
    | x >= 1/5 && x <  2/5 = 1
    | x >= 2/5 && x <  3/5 = 2
    | x >= 3/5 && x <  4/5 = 3
    | x >= 4/5 && x <= 5/5 = 4
    | otherwise = error "Wrong sentiment real."

prepareSet :: [TaggedTreeBank Int a] -> Vector (TreeBank a, Sentiments a)
prepareSet = Vector.fromList . map (\t -> (
    _ preprocess $ untag t,
    onlyTags $ mapTag sentToReal t))

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Parse the files.
    trainingSet <- parseFile "data/train.txt"
    testSet <- parseFile "data/test.txt"

    -- Extract the word set.
    let wordSet = _

    -- Prepare the data sets.
    let trainingSet' = prepareSet trainingSet
        testSet' = prepareSet testSet

    -- Run the driver.
    reify wordSet $ runDriverT . driver trainingSet' testSet'
