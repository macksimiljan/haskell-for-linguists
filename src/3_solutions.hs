-- Fehlersuche
sTrangesUM = (+) a (length xs)
    where
      a = 37
      xs = [1, 2, 3, 4, 5]


-- Deep Thought
theAnswer = 42

deepThought :: String -> String
deepThought question = "The ultimate answer to your question is: " ++ show(theAnswer)

-- das Allerletzte
finalInduc :: [a] -> a
finalInduc [last]         = last
finalInduc (first:others) = finalInduc(others)

-- Woerter
-- | Buchstabe
data Symbol = A | B | C deriving(Show)

-- | Wort
data Wort = LeeresWort | Wort Symbol Wort
instance Show Wort where
    show LeeresWort    = "#"
    show (Wort s w) = show(s) ++ ":" ++ show(w)

-- Baeume
data Tree a = Node a [Tree a] deriving Show

leaf1 = Node 1 []
leaf2 = Node 2 []
leaf3 = Node 3 []
tree12 = Node 12 [leaf1, leaf2]
tree123 = Node 123 [tree12, leaf3]

isLeaf :: Tree a -> Bool
isLeaf (Node b []) = True
isLeaf _ = False

mother :: Tree a -> a
mother (Node a ts) = a

daughters :: Tree a -> [Tree a]
daughters (Node a ts) = ts