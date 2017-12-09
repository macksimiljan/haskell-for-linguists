module Utilities (
    list2Str,
    grammar2Str
) where

import LeafyTreeGrams
import Data.List (intercalate)

list2Str :: (Show a) => [a] -> String
list2Str list = intercalate "\n" [show e | e <- list] ++ "\n"

-- call: > putStr grammarStr
grammar2Str :: (Show a) => [TreeBiGram a] -> String
grammar2Str []     = ""
grammar2Str (x:xs) = showBigram(x) ++ "\n" ++ (grammar2Str xs)

showBigram :: (Show a) => TreeBiGram a -> String
showBigram bigram = show mother ++ "\t-->\t" ++ showDaughters daughters
  where
    mother = lhs bigram
    daughters = rhs bigram

showDaughters :: (Show a) => [a] -> String
showDaughters []     = ""
showDaughters (x:xs) = show(x) ++ "\t" ++ showDaughters xs


