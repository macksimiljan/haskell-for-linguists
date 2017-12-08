module LeafyTreeGrams
(
  -- * N-grams over (leafy) trees
  -- ** Trees
  Rooted,root,r,
  isRoot,
  unR,
  -- ** Bigrams
  TreeBiGram(TreeBiGram,lhs,rhs),
  -- $treebigrams
  --
  treeBiGram
  )
where
import LeafyTrees
import Data.Maybe (catMaybes,isNothing,fromJust)
import Data.List (nub)

-- | Whereas in the case of strings, we added a 'Start' and a 'Finish'
-- symbol to our vocabulary to indicate word boundaries, here we will
-- see that we can get away with just adding a special symbol
-- explicitly indicating the root of the tree.  A type becomes
-- 'Rooted' once a special symbol 'Root' is added to it.
--
-- Here is one way of doing this:
--
-- > data Rooted a = Root | R a deriving (Show, Eq, Ord)
--
-- While this is perfectly reasonable, the 'Maybe' data type is
-- identical to this newly defined 'Rooted' one in all but the names.
-- We will accordingly make use of 'Maybe' instead.
type Rooted a = Maybe a

root :: Rooted a
root = Nothing

r :: a -> Rooted a
r = Just

-- | We want to be able to see if something is the 'Root'.  As we are
-- using the 'Maybe' data type, this amounts to asking whether the
-- value we are looking at is 'Nothing'.  Unsurprisingly, there is
-- already a way of doing this in Haskell.
isRoot :: Rooted a -> Bool
isRoot = isNothing

-- | It is convenient to have a way of extracting data from a
-- non-'Root' member of this type.
unR :: Rooted a -> a
unR = fromJust

-- | The generalization of bigrams over strings to bigrams over trees
-- is made by viewing bigrams over strings as consisting of a node,
-- followed by its immediate daughters.  In the case of strings, there
-- is but one daughter, and so we have, in effect, a pair of symbols.
-- However, in the case of trees, there may be multiple daughters, and
-- so we have a pair of a symbol and a /list/ of symbols.
data TreeBiGram a = TreeBiGram {lhs :: Rooted a, rhs :: [a]} deriving (Eq,Ord,Show)

-- | we construct a list of all tree-bigrams in a tree by walking down
-- from root to frontier, at each step recording mothers and
-- daughters.  So as to try to keep the grammars as small as possible,
-- we use set operations to remove duplicates.
treeBiGram :: Eq a => TreeLike tree a => tree -> [TreeBiGram a]
treeBiGram t = TreeBiGram root [mother t] : nub (tBG t)
  where
    tBG t | isLeaf t = []
          | otherwise =
              let ds = daughters t
              in
                TreeBiGram (r $ mother t) (map mother ds) :
                (ds >>= tBG)


