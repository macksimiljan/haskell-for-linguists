data Tree a = Node a [Tree a] deriving (Eq,Ord,Show)

data Rooted a = Root | R a deriving (Show, Eq, Ord)

type TreeBiGram a = (Rooted a, [Rooted a]) 

data SituatedNode a = Sit {left :: Int, cat :: a, right :: Int} deriving (Eq,Ord)


intParse :: Eq a =>
            [TreeBiGram a]                -- ^ The grammar
         -> [a]                           -- ^ The input sentence (probably tokenized)
         -> [TreeBiGram (SituatedNode a)] -- ^ The intersection grammar


