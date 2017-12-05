{-# LANGUAGE MultiParamTypeClasses,GADTs #-}
module LeafySearch
  (
    breadthFirst,depthFirst,dfid,
    bestFirst,
    Search(initialState,isFinal,successors,searchSpace,search)
  )
where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe,maybeToList)
import LeafyTrees

  
-- | a breadth first enumeration of the elements of a tree
breadthFirst :: TreeLike tree symbol => tree -> [symbol]
breadthFirst t = bf [t]
  where
    bf (t : ts) = mother t : bf (ts ++ daughters t)
    bf [] = []


-- | a depth first enumeration of the elements of a tree.  For parity
-- with 'breadthFirst', we could have written it using the explicit
-- maintenance of a list data structure to organize the nodes to be
-- visited, as follows:
--
-- > df [] = []
-- > df (Node b bs : ts) = b : df (bs ++ ts)
--
-- As it stands, we could simplify the code by reformulating the
-- 'concat' applied to the result of 'fmap'ping something using the
-- bind operator (@('>>=')@):
--
-- > depthFirst (Node b bs) = b : (bs >>= depthFirst)
depthFirst :: TreeLike tree symbol => tree -> [symbol]
depthFirst t = mother t : concat (fmap depthFirst $ daughters t)


-- | as I have written it, 'dfid' will never terminate.  If the tree
-- is finite (of depth /d/), then after reaching the value @d@ in the
-- list @[0..]@, the input tree will be searched depth first over and
-- over again.  Because trees may be infinite, we cannot simply
-- iterate over @[0 .. 'depth' t ]@.  'depth' must traverse each node
-- of the tree, to verify that it has indeed found the longest path
-- from root to leaf.  This takes a while if the tree is infinite.  A
-- solution would involve checking at each iteration whether the tree
-- is at least as deep as we will be traversing; if not, we stop.
dfid :: TreeLike tree symbol => tree -> [symbol]
dfid t = concat $ fmap depthFirst $ mapMaybe (flip prune t) [0..]

-- | a best first enumeration is one where it is the "best" node which
-- is explored next.  This differs from the previous strategies, which
-- used purely tree-geometric considerations to determine the node to
-- be explored next.  It requires that we be able to compare the nodes
-- in some meaningful way; I have indicated this by attaching the
-- class constraint that items of type @a@ support the 'Ord'
-- operations (in particular, 'compare').
--
-- The enumeration itself is achieved, as was breadth first search, by
-- maintaining a list of nodes which can be visited.  The first
-- element on the list is the "best".  The remaining elements on the
-- list @ts@ are ranked in order of "bestness".  But now we must
-- consider as well the daughters of the new tree @bs@, when selecting
-- the new "best" node.  We do this by sorting the list @bs ++ ts@
-- according to how good their respective mother nodes are.  This is
-- presumably not the most efficient strategy, as this problem has
-- particular features that this solution does not take into account,
-- namely:
--
-- 1. @ts@ is already sorted
--
-- 2. @ts@ is generally (much) longer than @bs@
--
-- An alternative method for ordering @bs ++ ts@ which takes
-- advantage of properties 1 and 2 might be:
--
-- > foldr (insertBy (compare `on` mother)) ts bs
bestFirst :: TreeLike tree symbol
          => (symbol -> symbol -> Ordering)
          -> tree -> [symbol]
bestFirst compare t = best [t]
  where
    best (t : ts) = mother t : best (sortBy (compare `on` mother) (daughters t ++ ts))
    best [] = []

class Eq info => Search state info where
  initialState :: info -> state
  isFinal :: info -> state -> Bool
  successors :: info -> state -> [state]
  searchSpace :: (TreeLike tree state',state ~ state') => info -> tree
  searchSpace i = unfoldTree (successors i) (initialState i)
  search :: (TreeLike tree state',state ~ state')
         => info -- ^ relevant information
         -> (tree -> [state]) -- ^ the chosen exploration strategy
         -> [state]
  search i strategy = filter (isFinal i) $ strategy $ searchSpace i

