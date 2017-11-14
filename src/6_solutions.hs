import Data.Set as Set (toList, fromList)
import Data.List (partition)

import IntersectionParser
import TreeGrams
import Trees


-- Person example

data Person = Person {
  firstName :: String,
  lastName  :: String,
  city      :: String,
  age       :: Int
} deriving (Show)

person = Person {firstName = "Arthur", lastName = "Dent", city = "London", age = 20}

fullName =  firstName person ++ " " ++ lastName person


-- Cafe example

data Cafe a = Cafe {
  name           :: String,
  iceCream  :: a,
  coffee    :: a,
  croissant :: a
}
instance Show a => Show (Cafe a) where
  show cafe = name cafe ++ ": ice cream: " ++ show (iceCream cafe)
                        ++ ", coffee: " ++ show (coffee cafe)
                        ++ ", croissant: " ++ show (croissant cafe)
instance Eq a => Eq (Cafe a) where
  cafe1 == cafe2 = iceCream cafe1 == iceCream cafe2
                    && coffee cafe1 == coffee cafe2
                    && croissant cafe1 == croissant cafe2
instance Functor Cafe where
  fmap f cafe = Cafe {
    name = name cafe,
    iceCream = f (iceCream cafe),
    coffee = f (coffee cafe),
    croissant = f (croissant cafe)
  }

cafe1 = Cafe {name="Cafe Albert", iceCream=1.0, coffee=2.5, croissant=2.0}
cafe2 = Cafe {name="Cafe Kater", iceCream="low", coffee="low", croissant="expensive"}


-- Intersection Parsing

tree1 = Node "S" [Node "NP" [Node "Hans" []], Node "VP" [Node "V" [Node "mag" []], Node "NP" [Node "Erdbeeren" []]]]
tree2 = Node "S" [Node "NP" [Node "Hans" []], Node "VP" [Node "Neg" [Node "keine" []], Node "VP" [Node "V" [Node "mag" []], Node "NP" [Node "Erdbeeren" []]]]]
tree3 = Node "S" [Node "NP" [Node "Maria"[]], Node "VP" [Node "V" [Node "mag" []], Node "NP" [Node "Rosen" []]]]

corpusTree = [tree1, tree2, tree3]
grammarTree = simplifyGrammar ( concat [ treeBiGram (makeRooted x) | x <- corpusTree ])

simplifyGrammar :: (Ord a, Eq a) => [a] -> [a]
simplifyGrammar g = Set.toList (Set.fromList g)

-- call: > putStr grammarStr
grammar2Str :: (Show a) => [a] -> String
grammar2Str []     = ""
grammar2Str (x:xs) = show(x) ++ "\n" ++ (grammar2Str xs)

(roots, nonRoots) = partition (isRoot . fst) grammarTree
(leaves, nonLeaves) = partition (null . snd) nonRoots

test1 = "Maria mag Erdbeeren."
test1Rooted = [R "Maria", R "mag", R "Erdbeeren"]

intersectionGrammar = simplifyGrammar (intParse grammarTree test1Rooted)

