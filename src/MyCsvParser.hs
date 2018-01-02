{-# LANGUAGE DeriveGeneric #-}

module MyCsvParser where

import Data.ByteString.Lazy hiding (map, zip)
import Data.Vector (Vector)
import qualified Data.Vector as V hiding (Vector)
import Data.Csv
import GHC.Generics (Generic)

import TopDownExample
import TopDownItems


data HeightRecord = HeightRecord {
    name :: String,
    height :: Int
} deriving (Generic, Show)
instance FromRecord HeightRecord
instance ToRecord HeightRecord

data TestSentence = TestSentence {
    identifier :: Int,
    sentence :: Sentence Char,
    expected :: Int, -- easier because `ToField Bool` is not implemented in cassava
    actual :: Int -- easier because `ToField Bool` is not implemented in cassava
} deriving (Generic, Show)
instance FromRecord TestSentence
instance ToRecord TestSentence



parseHeights :: ByteString -> Either String (Vector HeightRecord)
parseHeights content = decode NoHeader content :: Either String (Vector HeightRecord)


justHeightValues :: Either String (Vector HeightRecord) -> Vector Int
justHeightValues heights = V.map (\x -> height x) records
    where
        records = either (const $ V.fromList []) id heights


sentences2Grammaticality :: [String] -> [Bool]
sentences2Grammaticality s = map bugsRecognizer s


parseTestSentences :: ByteString -> Either String (Vector TestSentence)
parseTestSentences content = decode NoHeader content :: Either String (Vector TestSentence)

testSentences2Csv :: Vector TestSentence -> ByteString
testSentences2Csv = encode . V.toList

ids :: Either String (Vector TestSentence) -> [Int]
ids testSentences = V.toList $ V.map (\x -> identifier x) records
    where
        records = either2Vector testSentences

sentences :: Either String (Vector TestSentence) -> [Sentence Char]
sentences testSentences = V.toList $ V.map (\x -> sentence x) records
    where
        records = either2Vector testSentences

expectations :: Either String (Vector TestSentence) -> [Bool]
expectations testSentences = V.toList $ V.map (\x -> int2Bool $ expected x) records
    where
        records = either2Vector testSentences

actuals :: Either String (Vector TestSentence) -> [Bool]
actuals testSentences = V.toList $ V.map (\x -> int2Bool $ actual x) records
    where
        records = either2Vector testSentences

updateActualFields :: Vector TestSentence -> [Bool] -> Vector TestSentence
updateActualFields records values = records V.// (zip [0,1..] updatedRecords)
    where updatedRecords = [ updateActualField record value | (record, value) <- zip (V.toList records) values]

updateActualField :: TestSentence -> Bool -> TestSentence
updateActualField record value = record {actual = bool2Int value}


either2Vector :: Either a (Vector b) -> Vector b
either2Vector = either (const $ V.fromList []) id

int2Bool :: Int -> Bool
int2Bool 1 = True
int2Bool _ = False

bool2Int :: Bool -> Int
bool2Int True = 1
bool2Int _    = 0
