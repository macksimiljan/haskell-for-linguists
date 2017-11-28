module ParsingUtilities
(

) where

import Data.Set as Set (toList, fromList)
import Data.List (partition)

import TreeGrams

-- | Deduplicates a grammar/ the list of bigrams.
simplifyGrammar :: (Ord a, Eq a) => [a] -> [a]
simplifyGrammar g = Set.toList (Set.fromList g)


-- | Maps a grammar (list of bigrams) to a nicely formated string.
grammar2Str :: (Show a) => [TreeBiGram a] -> String
grammar2Str []     = ""
grammar2Str (x:xs) = showBigram(x) ++ "\n" ++ (grammar2Str xs)

showBigram :: (Show a) => TreeBiGram a -> String
showBigram bigram = show mother ++ "\t-->\t" ++ showDaughters daughters
  where
    mother = fst bigram
    daughters = snd bigram

showDaughters :: (Show a) => [Rooted a] -> String
showDaughters []     = ""
showDaughters (x:xs) = show(x) ++ "\t" ++ showDaughters xs
