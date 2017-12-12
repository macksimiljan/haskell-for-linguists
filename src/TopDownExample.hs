module TopDownExample where

import Data.List (union)

import LeafySearch
import LeafyTreeGrams
import LeafyTrees
import ParseTest
import TopDownItems
import Utilities

daffy = name "Daffy"
bugs = name "Bugs"
theAnvil = dp "the" "anvil" id
aTruck = dp "a" "truck" id

hit = tvp "hit"
fellOver = ivp "fell_over"

theAnvilHitDaffy = hit theAnvil daffy
daffyFellOver = fellOver daffy
aTruckFellOver = fellOver aTruck
aTruckHitBugs = hit aTruck bugs

bigrams_daffyFellOver = treeBiGram daffyFellOver
bigrams_theAnvilHitDaffy = treeBiGram theAnvilHitDaffy

searchBF_theAnvilHitDaffy = breadthFirst theAnvilHitDaffy
searchDF_theAnvilHitDaffy = depthFirst theAnvilHitDaffy
searchBest_theAnvilHitDaffy = bestFirst (flip compare) theAnvilHitDaffy

steps = [(+1) $ length $ takeWhile (\x -> x /= (Left "the")) list | list <- [searchBF_theAnvilHitDaffy,
                                                                             searchDF_theAnvilHitDaffy,
                                                                             searchBest_theAnvilHitDaffy] ]

mySentence = "Daffy fell over." :: Sentence Char
myWordOrSymbol = Right "S" :: WordOrSymbol String String
left = Nothing :: Rooted (WordOrSymbol String String)
right = [Right "S"] :: [WordOrSymbol String String]
myRule = TreeBiGram {lhs = left, rhs = right} :: Rule String String
myPosition = 0 :: Position
myPredictions = [left, Just (Right "VP")] :: Predictions String String
myHistory = [myRule] :: History String String
myGrammar = [myRule] :: Grammar String String

functionWithLostArg :: Int -> Int -> Int
functionWithLostArg x = (+) x

grammar :: Grammar String String
grammar = []
            `union` treeBiGram theAnvilHitDaffy
            `union` treeBiGram daffyFellOver
            `union` treeBiGram aTruckFellOver
            `union` treeBiGram aTruckHitBugs

bugsParser :: String -> [LeafyTree String String]
bugsParser = parse2Trees grammar . words

bugsRecognizer :: String -> Bool
bugsRecognizer = recognizer grammar . words