import Data.Set as Set (toList, fromList)

import Bigrams
import TreeGrams


data Wochentag = Montag | Dienstag | Mittwoch | Donnerstag | Freitag | Samstag | Sonntag deriving (Show, Eq, Ord, Enum)

-- #### strings ####

name = "max"

sentence1 = "Hans mag Erdbeeren."
sentence2 = "Hans mag keine Rosen."
sentence3 = "Maria mag Rosen."
corpus = [sentence1, sentence2, sentence3]

-- text2Grams calls makeBounded
grammar = simplifyGrammar (concat (map text2Grams corpus))

-- #### trees ####

treeVP = makeRooted (Node "VP" [ Node "V" [], Node "NP" [] ])

tree1 = Node "S" [Node "NP" [Node "Hans" []], Node "VP" [Node "V" [Node "mag" []], Node "NP" [Node "Erdbeeren" []]]]
tree2 = Node "S" [Node "NP" [Node "Hans" []], Node "VP" [Node "Neg" [Node "keine" []], Node "VP" [Node "V" [Node "mag" []], Node "NP" [Node "Erdbeeren" []]]]]
tree3 = Node {mother="S", daughters=[
  Node {mother="NP", daughters=[
    Node {mother="Maria", daughters=[]}
  ]},
  Node {mother="VP", daughters=[
    Node {mother="V", daughters=[ 
      Node {mother="mag", daughters=[]}
    ]},
    Node {mother="NP", daughters=[
      Node {mother="Rosen", daughters=[]}
    ]}
  ]}
]}  


corpusTree = [tree1, tree2, tree3]

grammarTree = simplifyGrammar ( concat [ treeBiGram (makeRooted x) | x <- corpusTree ])


simplifyGrammar :: (Ord a, Eq a) => [a] -> [a]
simplifyGrammar g = Set.toList (Set.fromList g)

-- call: > putStr grammarStr
grammar2Str :: (Show a) => [a] -> String
grammar2Str []     = ""
grammar2Str (x:xs) = show(x) ++ "\n" ++ (grammar2Str xs)

