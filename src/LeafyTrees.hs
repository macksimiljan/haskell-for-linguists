{-# LANGUAGE
           FlexibleInstances,
           MultiParamTypeClasses,
           FunctionalDependencies
#-}

module LeafyTrees
(

  Tree,LeafyTree,
  -- $trees
  TreeLike(isLeaf,mother,daughters,mkTree,mapTree,unfoldTree,depth,prune,foldTree,filterTree)
  )
where

import Data.Maybe (mapMaybe)

-- | We began the class with this definition of an unordered tree.  It
-- identifies leaves with daughterless nodes.
data Tree node = Node node [Tree node]
               deriving (Eq,Ord,Show)

-- | It is natural to want to treat nodes without daughters as
-- different from leaves.  A 'LeafyTree' allows us to do just this.
-- In a 'LeafyTree', leaves are marked as such.
data LeafyTree leaf node = Leaf leaf
                         | LNode node [LeafyTree leaf node]
                         deriving (Eq,Ord)
-- $trees
--
-- A 'LeafyTree' is a datatype that forces leaf symbols to be at
-- leaves, and nonterminal symbols to be at internal nodes.  We could
-- use our 'Tree' datatype to discriminate between leaf and
-- nonterminal symbols, and view 'LeafyTree' as an abbreviation.
--
-- > type AltLeafyTree leaf node = Tree (Either leaf node)
--
-- Note that every 'LeafyTree' can be represented as an
-- 'AltLeafyTree':
--
-- > leafy_to_alt :: LeafyTree leaf node -> AltLeafyTree leaf node
-- > leafy_to_alt (Leaf l) = Node (Left l) []
-- > leafy_to_alt (LNode n ts) = Node (Right n) (fmap leafy_to_alt ts)
--
-- Of course, 'AltLeafyTree' /doesn't/ force leaf symbols to be at
-- leaves and node symbols to be at internal nodes.  Thus there is
-- "junk" in 'AltLeafyTree' that isn't present in 'LeafyTree'.  This
-- can be seen by noting that while every 'LeafyTree' is also an
-- 'AltLeafyTree', not every 'AltLeafyTree' is a 'LeafyTree'.
--
-- > alt_to_leafy :: AltLeafyTree leaf node -> Maybe (LeafyTree leaf node)
-- > alt_to_leafy (Node (Left l) []) = Leaf l
-- > alt_to_leafy (Node (Right n) ts) = traverse alt_to_leafy ts >>= LNode n
-- > alt_to_leafy (Node (Left l) ts) = undefined
--
-- We have that @leafy_to_alt@ is a right inverse of @alt_to_leafy@,
-- but not a left inverse (as @alt_to_leafy@ is not even defined
-- everywhere).
--
-- prop> alt_to_leafy (leafy_to_alt t) = Just t

-- | It would be a waste to write two versions of every function on
-- trees, one for 'Tree' and one for 'LeafyTree'.  We avoid this by
-- defining a typeclass, which asserts that a type @tree@ is
-- 'TreeLike' (in relation to another type @symbol@).  We will then
-- program just one version of a function, that will work on /any/
-- 'TreeLike' type.
class TreeLike tree symbol | tree->symbol where
  {-# MINIMAL mother,daughters,mkTree #-}
  
  -- | 'mother' returns the symbol at the root of the tree
  mother :: tree -> symbol
  -- | 'daughters' returns the list of daughters of the root of the tree
  daughters :: tree -> [tree]
  -- | 'mkTree' requires a symbol for the root, and a list of its
  -- desired daughters, and then constructs a tree
  mkTree :: symbol -> [tree] -> tree
  -- | 'isLeaf' says whether a tree is a leaf.  By default, this can
  -- be checked by looking at whether the root has any daughters.
  isLeaf :: tree -> Bool
  isLeaf = null . daughters
  -- | If we can translate @symbols@ to @symbols'@, we can transform
  -- @trees@ over @symbols@ to @trees'@ over @symbols'@
  mapTree :: (TreeLike tree' symbol')
          => (symbol -> symbol') -> tree -> tree'
  mapTree f t = mkTree (f $ mother t) $ map (mapTree f) $ daughters t
  -- | If we are given a starting symbol, and a way of creating a list
  -- of symbols from a given one, we can use this information to
  -- construct a tree.
  unfoldTree :: (symbol -> [symbol]) -> symbol -> tree
  unfoldTree f a = mkTree a $ map (unfoldTree f) $ f a
  -- | The 'depth' of a tree is one more than the maximal depth of its
  -- daughters.
  depth :: (Enum i, Ord i) => tree -> i
  depth = succ . maximum . (toEnum 0 :) . map depth . daughters
  -- | We might want to trim an overgrown tree, by chopping off all of
  -- its branches past a certain length.  
  prune :: Int -> tree -> Maybe tree
  prune 1 t = return $ mkTree (mother t) []
  prune n t | n < 1 = Nothing
            | otherwise =
              do
                ds <- prune (n-1) `traverse` daughters t
                return $ mkTree (mother t) ds
  -- | 'foldTree' transforms a @tree@ into some other object
  -- bottom-up.  Starting at the leaves, it uses its first argument, a
  -- function @f@, to transform their @symbol@ into an @a@.  Then, it
  -- works its way up the tree, turning internal nodes all of whose
  -- daughters have been turned into @a@s into an @a@ by again using
  -- the function @f@.
  foldTree :: (symbol -> [a] -> a) -> tree -> a
  foldTree f = foldTreeHelper []
    where
      foldTreeHelper a t = f (mother t) $ map (foldTreeHelper a) $ daughters t
  -- | 'filterTree' takes a predicate of nodes and a tree @t@, and returns
  -- the subtree (qua graph) of @t@ which can be reached from the root
  -- without traversing any nodes which the predicate is false of.
  --
  -- Intuitively, 'filterTree' chops off all subtrees of a tree whose roots
  -- the predicate is false of.  If the nodes of the tree are such that
  -- the predicate is guaranteed to be monotone decreasing (where
  -- @'False' <= 'True'@ and mothers precede daughters), then @'filterTree' p
  -- t@ eliminates all non-@p@ nodes from @t@, while preserving
  -- dominance and precedence relations.
  filterTree :: TreeLike tree symbol
             => (symbol -> Bool) -- ^ The predicate @p@ which tests
                                 -- whether a node (and associated
                                 -- subtree) should be filtered or not
             -> tree -- ^ The tree to filter
             -> Maybe tree -- ^ 'Nothing' if the root is filtered, and
                           -- @'Just' t@ if the root is not filtered
  filterTree p t | p b = Just $ mkTree b $ mapMaybe (filterTree p) $ daughters t
                 | otherwise = Nothing
    where
      b = mother t


instance TreeLike (Tree node) node where
  mother (Node b _) = b

  daughters (Node _ bs) = bs

  mkTree = Node

instance TreeLike (LeafyTree leaf node) (Either leaf node) where
  -- | While there is a default definition for 'isLeaf' in the
  -- 'TreeLike' class, it is not the correct one for 'LeafyTree's,
  -- where we allow non-leaf nodes without any children.
  isLeaf (Leaf _) = True
  isLeaf _ = False
  
  mother (Leaf l) = Left l
  mother (LNode b _) = Right b

  daughters (Leaf _) = []
  daughters (LNode _ bs) = bs

  mkTree (Left l) _ = Leaf l
  mkTree (Right b) bs = LNode b bs

instance (Show node,Show leaf) => Show (LeafyTree node leaf) where
  show = showHelper 0
    where
      showHelper lvl (Leaf l) = show l
      showHelper lvl (LNode t ts) =
        show t
        ++ foldr (\s ss -> "\n" ++ s ++ ss) "" (map (((print_sep lvl ++ " |- ") ++ ) . showHelper (lvl + 1)) ts)
      print_sep lvl = iterate (" " ++) "" !! lvl


              

