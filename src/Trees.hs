{- | Here we define trees, and generalize functions on strings to functions on trees -}
module Trees
(
  -- * The Tree datatype
  Tree(Node),
  -- ** Basic operations on Trees
  isLeaf, mother, daughters,
  -- * Some List functions defined in class
  laenge, topDown, bottomUp, final,
  -- ** Viewing Lists as Trees
  --
  -- $listsAsTrees
  list2Tree,
  -- ** Generalizing @laenge@
  --
  -- $generalizingLaenge
  countNodes, levels,
  -- ** Generalizing @topDown@
  --
  -- $generalizingTopDown
  td, bf, extractPaths,
  -- ** Generalizing @bottomUp@
  --
  -- $generalizingBottomUp
  bu, lc, glc,
  -- ** Generalizing @final@
  -- 
  -- $generalizingFinal
  yield, rev_yield
  )
where

{- | An object of type @'Tree' a@, for any type @a@, consists of an
   object of type @a@, followed by a list of objects of type @'Tree'
   a@.  This kind of object is written as @'Node' a [t1,...,tn]@.
   This (and more!) is already implemented in the module
   [Data.Tree](https://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Tree.html). -}
data Tree a = Node a [Tree a] deriving Show

{- | As we mentioned in class, a 'Tree' is a structure which
 /contains/ data.  We would like a way of changing the data inside
 of a structure.  This is what the function 'fmap' does.  Because the
 idea of "changing data inside of a structure" is more general than
 the specific case of trees, there is a __class__ 'Functor' which is
 the kind of structure that supports this sort of operation.  We
 declare 'Tree' to be that kind of structure as follows: -}
instance Functor Tree where
  fmap f (Node b bs) = Node (f b) (fmap (fmap f) bs)
-- ^ Note that the first call to 'fmap' on the right hand side is the
-- 'fmap' for /lists/.  (Lists are a built-in datatype in Haskell, and
-- are already declared as being 'Functor's.)

-- | checks whether a 'Tree' is a leaf; i.e. whether it has any daughters
isLeaf :: Tree a -> Bool
isLeaf (Node b []) = True
isLeaf _ = False

-- | returns the label of the root node
mother :: Tree a -> a
mother (Node a ts) = a

-- | returns the list of daughters of the root node
daughters :: Tree a -> [Tree a]
daughters (Node a ts) = ts

-- | counts the number of elements in a list.  (A more general version
-- of this function already exists, and is called
-- [length](https://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:length).)
laenge :: [a] -> Int
laenge [] = 0
laenge (b:bs) = 1 + laenge bs

-- | takes apart a list, and puts it back together in the same order
topDown :: [a] -> [a]
topDown [] = []
topDown (b:bs) = [b] ++ topDown bs

-- | takes apart a list, and puts it back together in the reverse order
bottomUp :: [a] -> [a]
bottomUp [] = []
bottomUp (b:bs) = bottomUp bs ++ [b]

-- | maps a list to the list containing its final element. (A better
-- version of this function already exists, and is called
-- [last](https://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:last).)
final (b:[]) = [b]
final (b:bs) = final bs

-- $listsAsTrees
--
-- We would like to generalize the functions above to work on all
-- trees.  They should of course treat monadic trees just like
-- lists.  This means that for any list function @f@, we should
-- have:
--
-- prop> f l = f' (list2Tree l) 
--
-- where @f'@ is the tree generalization of @f@, and @list2Tree@ is
-- defined below.

-- | In order to view a list as a monadic tree, we must explicitly
-- define a coercion (this will only work if the list is non-empty).
-- This is because, in Haskell, lists and trees are __formally__
-- __different__ types; a list is built out of empty lists @[]@ using
-- the constructor @:@, whereas trees are built using the constructor
-- @'Node'@.  By defining this function, we are __showing__ how we
-- want to think of lists as trees.
list2Tree :: [a] -> Tree a
list2Tree (b:[]) = Node b []
list2Tree (b:bs) = Node b [list2Tree bs]

-- $generalizingLaenge
--
-- We would like to define generalizations of 'laenge' that work
-- on all trees, not just /listy/ trees.

-- | [Idea 1]: 'laenge' counts all the nodes of the tree
--             (@sum@ adds all the numbers in a list)
countNodes (Node b bs) = 1 + sum (fmap countNodes bs)

-- | [Idea 2]: 'laenge' counts how many levels there are in the tree
--          (@maximum@ finds the biggest number in a list)
levels (Node b []) = 1
levels (Node b bs) = 1 + maximum (fmap levels bs)

-- $generalizingTopDown
--
-- We would like to generalize 'topDown' to work on all trees.

-- | [Idea 1]: 'topDown' visits mother before any daughters (@concat@
--            concatenates a list of lists together from left to right)
td (Node b bs) = [b] ++ concat (fmap td bs)

-- | [Idea 2]: 'topDown' visits each level at a time here it is easier to
--          define a function operating on a list of trees.  So we
--          define the generalization of 'topDown' in terms of this
--          function.
bf t = breadthFirst [t]
  where
    breadthFirst [] = []
    breadthFirst ((Node b bs):ts) = [b] ++ breadthFirst (ts ++ bs)

-- | [Idea 3]: 'topDown' extracts a list of all of the paths from a root
--   to a leaf.  (Because 'topDown' is defined on a string, there is
--   exactly one path, and it is returned, instead of the list
--   containing it).
extractPaths :: Tree a -> [[a]]
extractPaths (Node b []) = [[b]]
extractPaths (Node b bs) = fmap (\xs -> b:xs) (concat (fmap extractPaths bs))

-- $generalizingBottomUp
--
-- We would like to generalize 'bottomUp' to work on all trees.

-- | [Idea 1]: 'bottomUp' visits mother after all daughters
bu (Node b bs) = concat (fmap bu bs) ++ [b]

-- | [Idea 2]: 'bottomUp' visits mother after first daughter (if any)
lc (Node b []) = [b]
lc (Node b (c:cs)) = lc c ++ [b] ++ concat (fmap lc cs)

-- | [Idea 3]: 'bottomUp' visits mother after first __k__ daughters
-- (if any).  Note that @'td' = 'glc' 0@, and @'lc' = 'glc' 1@.
-- @splitAt k@ breaks a list @bs@ in half at its \(k^{th}\) member.
-- That is,
--
-- > splitAt k bs = (cs,ds)
--
-- where
--
-- > laenge cs = k
-- > cs ++ ds = bs
--
glc k (Node b bs) = concat (fmap (glc k) cs ++ [[b]] ++ fmap (glc k) ds)
  where
    (cs,ds) = splitAt k bs

-- $generalizingFinal
--
-- We would like to generalize 'final'.  The indeterminacy in
-- projecting from lists to trees in this case comes from how to order
-- the leaves of the daughters.

-- | [Idea 1]: 'final' reads the list of leaves from left to right
yield (Node b []) = [b]
yield (Node b bs) = concat (fmap yield bs)

-- | [Idea 2]: 'final' reads the list of leaves from right to left
rev_yield (Node b []) = [b]
rev_yield (Node b bs) = concat (fmap yield (reverse bs))


