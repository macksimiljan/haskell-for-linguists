import Prelude hiding (filter)

{- The data structures we are using (this hasn't been explained yet)-}
data Tree a = Node a [Tree a] deriving Show
{- As we mentioned in class, a /Tree/ is a structure which _contains_ data.  We would like a way of changing the data inside of a structure.  This is what the function /fmap/ does.  Because the idea of 'changing data inside of a structure' is more general than the specific case of trees, there is a _class_ /Functor/ which is the kind of structure that supports this sort of operation.  We declare /Tree/ to be that kind of structure as follows: -}
instance Functor Tree where
  fmap f (Node b bs) = Node (f b) (fmap (fmap f) bs)
{- Note that the first call to /fmap/ on the right hand side is the /fmap/ for _lists_.  (Lists are a built-in datatype in Haskell, and are already declared as being Functors.)
-}

isLeaf (Node b []) = True
isLeaf _ = False

mother :: Tree a -> a
mother (Node a ts) = a

daughters :: Tree a -> [Tree a]
daughters (Node a ts) = ts

{-
  Some list functions we defined in class
-}

laenge :: [a] -> Int
laenge [] = 0
laenge (b:bs) = 1 + laenge bs

topDown :: [a] -> [a]
topDown [] = []
topDown (b:bs) = [b] ++ topDown bs

bottomUp :: [a] -> [a]
bottomUp [] = []
bottomUp (b:bs) = bottomUp bs ++ [b]

final (b:[]) = [b]
final (b:bs) = final bs
{-
  In order to view a list as a monadic tree, we must explicitly define a coercion (This will only work if the list is non-empty)
-}
list2Tree :: [a] -> Tree a
list2Tree (b:[]) = Node b []
list2Tree (b:bs) = Node b [list2Tree bs]

{- We would like to generalize the functions above to work on all trees.  They should of course treat monadic trees just like lists.  This means that for any of the above functions f, we should have:
  f l = f' (list2Tree l)
where f' is the tree generalization of f.
-}

{- We would like to define generalizations of /laenge/ that work on all trees, not just 'listy' trees -}
{-Idea 1: /laenge/ counts all the nodes of the tree
         (/sum/ adds all the numbers in a list)
-}

countNodes (Node b bs) = 1 + sum (fmap countNodes bs)
{-Idea 2: /laenge/ counts how many levels there are in the tree
          (/maximum/ finds the biggest number in a list)
-}
levels (Node b []) = 1
levels (Node b bs) = 1 + maximum (fmap levels bs)

{- We would like to generalize /topDown/ to work on all trees. -}
{-Idea 1: /topDown/ visits mother before any daughters
          (/concat/ concatenates a list of lists together from left to right)
-}
td (Node b bs) = [b] ++ concat (fmap td bs)
{-Idea 2: /topDown/ visits each level at a time
          here it is easier to define a function operating on a list of trees.  So we
          define the generalization of /topDown/ in terms of this function.
-}
bf t = breadthFirst [t]
  where
    breadthFirst [] = []
    breadthFirst ((Node b bs):ts) = [b] ++ breadthFirst (ts ++ bs)
{-Idea 3: /topDown/ extracts a list of all of the paths from a root to a leaf.  (Because /topDown/ is defined on a string, there is exactly one path, and it is returned, instead of the list containing it). -}
extractPaths :: Tree a -> [[a]]
extractPaths (Node b []) = [[b]]
extractPaths (Node b bs) = fmap (\xs -> b:xs) (concat (fmap extractPaths bs))

{- We would like to generalize /bottomUp/ to work on all trees. -}
{-Idea 1: /bottomUp/ visits mother after all daughters-}
bu (Node b bs) = concat (fmap bu bs) ++ [b]
{-Idea 2: /bottomUp/ visits mother after first daughter (if any)-}
lc (Node b []) = [b]
lc (Node b (c:cs)) = lc c ++ [b] ++ concat (fmap lc cs)

{- We would like to generalize /final/-}
{-Idea 1: /final/ reads the list of leaves from left to right-}
yield (Node b []) = [b]
yield (Node b bs) = concat (fmap yield bs)
{-Idea 2: I can't think of any other reasonable generalization...-}


