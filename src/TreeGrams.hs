{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module TreeGrams
(
  -- * N-grams over trees
  --
  -- We now extend our n-gram perspective on grammars from strings to
  -- trees.
  
  -- ** Trees
  Tree(..),
  depth,
  subtrees,
  prune,
  Rooted,
  makeRooted,
  -- ** Bigrams
  TreeBiGram,
  -- $treebigrams
  --
  treeBiGram,
  treeBiGram',
  treeZip,
  -- ** Ngrams
  TreeNGram,
  -- $treengrams
  treeNGram,treeNGram',
  -- ** Classes
  Bigram,Ngram,TwoGram
  )
where

import Data.Maybe (catMaybes,isNothing)

-- | A @'Tree' a@ (this definition comes from 'Data.Tree') consists of
-- a mother of type @a@, and a list of daughter trees.
data Tree a = Node {mother :: a, daughters :: [Tree a]} deriving (Show, Eq, Ord)

-- | A 'Tree' is a 'Functor'; the data it carries can be modified in
-- place, whilst preserving the overall tree structure.
instance Functor Tree where
  fmap f (Node m ds) = Node (f m) (fmap (fmap f) ds)

-- | The 'depth' of a 'Tree' is one generalization of the 'length'
-- function on lists.  It counts how many levels the tree has.
depth :: Tree a -> Int
depth (Node _ xs) = 1 + maximum (0 : fmap depth xs)

-- | The function 'subtrees' collects all of the subtrees of a tree
subtrees :: Tree a -> [Tree a]
subtrees t@(Node _ ds) = t : concat (fmap subtrees ds)

-- | @'prune' n@ is the generalization to 'Tree's of the list
-- operation @'take' n@ (which returns the sublist of the first @n@
-- elements).  I want to rigidly enforce that the depth of a @'prune'
-- n@ed 'Tree' is @n@.  So if the input tree isn't big enough to
-- support a particular pruning depth, the result is 'Nothing'.
--
-- > if depth t < n
-- > then isNothing (prune n t)
-- > else fmap depth (prune n t) == Just n
--
prune :: Int -> Tree a -> Maybe (Tree a)
prune 1 (Node x _) = Just (Node x [])
prune _ (Node x []) = Nothing
prune n (Node x xs) | n < 1 = Nothing
                    | otherwise = if any isNothing ps
                                  then Nothing
                                  else Just (Node x (catMaybes ps))
  where
    ps = fmap (prune (n - 1)) xs

-- | Whereas in the case of strings, we added a 'Start' and a 'Finish'
-- symbol to our vocabulary to indicate word boundaries, here we will
-- see that we can get away with just adding a special symbol
-- explicitly indicating the root of the tree.  A type becomes
-- 'Rooted' once a special symbol 'Root' is added to it.
data Rooted a = Root | R a deriving (Show, Eq, Ord)

-- | A 'Rooted' type also carries data, which can be modified.
instance Functor Rooted where
  fmap f Root = Root
  fmap f (R a) = R (f a)

-- | Given a 'Tree', we make it 'Rooted' by adding the symbol 'Root'
-- at the top.
makeRooted :: Tree a -> Tree (Rooted a)
makeRooted = Node Root . return . fmap R

-- | The generalization of bigrams over strings to bigrams over trees
-- is made by viewing bigrams over strings as consisting of a node,
-- followed by its immediate daughters.  In the case of strings, there
-- is but one daughter, and so we have, in effect, a pair of symbols.
-- However, in the case of trees, there may be multiple daughters, and
-- so we have a pair of a symbol and a /list/ of symbols.
type TreeBiGram a = (Rooted a, [Rooted a])

-- $treebigrams
--
-- The two basic ideas for extracting a set of bigrams from strings
-- (one based on zipping a list together with itself, and the other on
-- traversing a list and recording the encountered bigrams) can be
-- extended to the case of trees.

-- | The strategy of walking along a list and recording the bigrams
-- extends immediately to the case of trees.
treeBiGram :: Tree (Rooted a) -> [TreeBiGram a]
treeBiGram (Node x xs) =
  (x,fmap mother xs) : concat (fmap treeBiGram xs)

-- | The zipping approach can be stated at a high level similarly to
-- the case of lists, where 'daughters' plays the role of 'tail'.
treeBiGram' :: Tree a -> [TreeBiGram a]
treeBiGram' t = treeZip t' (daughters t')
  where
    t' = makeRooted t

-- | However, the concept of zipping itself must be revisited.  In the
-- case of lists, the function @zip@ maps two lists to another list.
-- One obvious generalization is to simply replace the list type
-- everywhere with the type 'Tree'.  This, while reasonable, would not
-- get us what we want, which is a /list/ of bigrams.  In the case of
-- lists, we zipped up a list together with its tail, which is the
-- same type of thing.  In the case of trees, we should zip up a tree
-- with its "tail", which is a list of trees.  In the code we can see
-- more similarity emerging between the zipping approach and the tree
-- walking one.
treeZip :: Tree a -> [Tree a] -> [(a,[a])]
treeZip (Node x xs) ds =
  (x,fmap mother ds) : concat (fmap (uncurry treeZip) $ zip xs (fmap daughters ds))




-- | A 'TreeNGram' is most naturally represented as a chunk of tree
-- (with depth @n@).  This generalizes the case of list n-grams, which
-- are represented as lists of length @n@.  It also generalizes the
-- case of /Tree/ bigrams, which are represented as a pair of a symbol
-- and a list of symbols; this is just a 'Tree' of depth two (the
-- first symbol in the pair is the mother node, and the list of
-- symbols are the mother nodes of the daughter subtrees, which are
-- all leaves).
type TreeNGram a = Tree (Rooted a)

-- | We can collect all n-grams in a tree by pruning all of its subtrees down to depth n
treeNGram :: Int -> Tree a -> [TreeNGram a]
treeNGram n = catMaybes . fmap (prune n) . subtrees . makeRooted

-- | If we have many subtrees which are smaller than depth @n@, it
-- will be wasteful to try to prune them all.  Noting that the depth
-- of a tree only decreases as one descends the tree, we can stop
-- constructing subtrees as soon as the current tree is too short.
treeNGram' :: Int -> Tree a -> [Tree a]
treeNGram' n t@(Node _ ds) = case prune n t of
                               Nothing -> []
                               Just t' -> t' : concat (fmap (treeNGram' n) ds)


-- | The "almost-but-not-quite" similarity of the string and tree
-- Bigram definitions motivates the /abstraction/ of bigrams into a
-- __class__.  The class 'Bigram' holds of a type function @f@ (we
-- have encountered @f = []@ and @f = Tree@) just in case there is a
-- data type of /bigrams over f/ (written @'Bi' f@), and functions
-- 'extract2grams' and 'is2Licensed' which behave as we expect.
-- Moreover, as we have noted previously, 'is2Licensed' can be defined
-- in terms of 'extract2grams'.
class Bigram f where
  data Bi f :: * -> *
  extract2grams :: f a -> [Bi f a]
  is2Licensed :: Eq (Bi f a) => [Bi f a] -> f a -> Bool
  is2Licensed b = all (`elem` b) . extract2grams
-- | Similarly, 'Ngram's can be abstracted into a class as well.
-- Other than the addition of an 'Int' parameter in the extraction
-- function signature, we have included a function 'degree' which
-- computes the size of the @n@-gram from a given n-gram.
class Ngram f where
  data N f :: * -> *
  extractNgrams :: Int -> f a -> [N f a]
  degree :: N f a -> Int
  isNLicensed :: Eq (N f a) => [N f a] -> f a -> Bool
  isNLicensed b = all (`elem` b) . extractNgrams n
    where
      n = degree $ head b

-- | The type @'TwoGram' f a@ is just a wrapper for an object of type
-- @f a@.
newtype TwoGram f a = TwoGram {unTwo :: f a}
-- | As one might think, if we have an 'Ngram' we can create a
-- 'Bigram'.  For bureaucratic reasons, we wrap the 'Ngram' type @f@
-- in the type wrapper 'TwoGram'.
instance Ngram f => Bigram (TwoGram f) where
  data Bi (TwoGram f) a = NtoBi (N f a)
  extract2grams = fmap NtoBi . extractNgrams 2 . unTwo

-- | We can make 'Tree' into an instance of class 'Bigram'.  Because
-- we want to keep the default definition of 'is2Licensed', we do not
-- need to redefine it in the instance.
instance Bigram Tree where
  data Bi Tree a = BiGramTree (TreeBiGram a) deriving (Eq,Show)
  extract2grams = fmap BiGramTree . treeBiGram . makeRooted

-- | A 'Tree' is also an instance of the class 'Ngram'.
instance Ngram Tree where
  data N Tree a = NGramTree (TreeNGram a) deriving (Eq,Ord,Show)
  extractNgrams n = fmap NGramTree . treeNGram' n . makeRooted
  degree (NGramTree t) = depth t

