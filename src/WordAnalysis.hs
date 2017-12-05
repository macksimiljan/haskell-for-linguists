{-|
Description : Statistics on German words

In this __fancy__ and /awesome/ module, nice functions for statistics on German words are presented.
For instance, the amazing @isGermanNoun@ predicate.
-}
module WordAnalysis
( -- * Predicates
  isGermanNoun
  -- * Functions
, numberPlosives
) where

import Data.Char (isUpper, toLower)

plosives = ['p', 'b', 't', 'd', 'k', 'g']

isPlosive :: Char -> Bool
isPlosive c = toLower c `elem` plosives


-- | Checks whether a given word is a German noun,
--   i.e. has an upper first letter.
--
-- >>> isGermanNoun "Tisch"
-- True
isGermanNoun :: String -- ^ A German word
             -> Bool   -- ^ Property of being a noun
isGermanNoun (firstLetter:otherLetters) = isUpper firstLetter


-- | Counts the number of plosives in a given word.
--
-- >>> numberPlosives "Tisch"
-- 1
numberPlosives :: String -- ^ A German word
               -> Int    -- ^ Number of plosives
numberPlosives word = length $ filter isPlosive word


