{-# LANGUAGE TupleSections #-}
module IntersectionParser
  (
    -- $intersectionParsing
    SituatedNode(Sit,left,cat,right),
    situate,
    makeContiguousDaughters,
    makeRules,
    makeRoots,
    makeLeaves,
    intParse
  )
where

import Control.Monad (guard)
import Data.List (partition)
import Trees
import TreeGrams

-- $intersectionParsing
--
-- The aspect of tree bigram grammars that allows them to be parsed
-- /efficiently/ is the combination of
--
-- 1. for every tree bigram grammar /g/ and sentence /w/, there is
-- another that assigns to /w/ exactly the same structures as does
-- /g/, but does not generate any other string, and
-- 2. this 'intersection' grammar can be constructed easily from the
-- original grammar.
--
-- Thus, while we could simply use an enumeration based parser, by
-- identifying and taking advantage of the particularities of this
-- restricted class of grammars, we are able to come up with a better
-- conception of how to parse.



-- | A node in the intersection grammar represents not only the
-- /category/ of a phrase, but also /where in the input string/ it is.
-- Our grammars derive trees, and trees relate in a very simple way to
-- their yields: every node in the tree has as its yield a contiguous
-- subsequence of the yield of the whole tree.  We can thus indicate
-- the part of the input string that a node contributes to by a pair
-- of integers, representing the 'left' and 'right' boundary positions
-- in the string.
data SituatedNode a = Sit {left :: Int, cat :: a, right :: Int} deriving (Eq,Ord)
instance Show a => Show (SituatedNode a) where
  show (Sit l c r) = "[" ++ show l ++ "," ++ show c ++ "," ++ show r ++ "]"

-- | It is convenient to be able to pass the arguments to the 'Sit'
-- constructor in a different order.
situate :: Int -- ^ The left boundary
        -> Int -- ^ The right boundary
        -> a -- ^ The node label
        -> SituatedNode a
situate i o a = Sit i a o

-- | If @ds@ is a list of daughters of some situated node @a@, and @a@
-- has left boundary of @i@, and right boundary of @j@, then we know
-- that the boundaries of the daughters must
--
-- 1. line up with each other, in that the right boundary of each
-- daughter must be the same as the left boundary of its neighbor.
-- 2. line up with their mother, in that the left boundary of the
-- first daughter must be the same as the left boundary of the mother,
-- and the right boundary of the /last/ daughter must be the same as
-- the right boundary of the mother.
--
-- 'makeContiguousDaughters' gives a list of all ways of situating the
-- list of daughters subject to these constraints.
makeContiguousDaughters :: Int -- ^ The left boundary of the mother
                        -> Int -- ^ The right boundary of the mother
                        -> [a] -- ^ The list of daughter node labels
                        -> [[SituatedNode a]]
makeContiguousDaughters i j [] = do
  return []               -- if there are no daughters, then there is
                          -- only one way of situating the list of
                          -- daughters (the empty list)
makeContiguousDaughters i j (b:bs) = do
  k <- [i..j]             -- otherwise, for every integer @k@ between
                          -- @i@ and @j@,
  ds <- makeContiguousDaughters k j bs
                          -- and every way @ds@ of making the
                          -- remaining daughters contiguous from @k@
                          -- to @j@
  return (situate i k b : ds)
                          -- then @([i,b,k] : ds)@ is a way of making
                          -- all the daughters contiguous

-- | Given a list of 'TreeBiGram's, we situate them in all possible
-- ways.
makeRules :: [TreeBiGram a] -- ^ The list of 'TreeBiGram's
          -> Int -- ^ The length of the input sentence
          -> [TreeBiGram (SituatedNode a)]
makeRules oldRules n = do
  (r,ds) <- oldRules      -- for every rule @(r,ds)@
  i <- [0..n]             -- every integer @i@ between zero and @n@
  j <- [i..n]             -- every integer @j@ between @i@ and @n@
  ss <- makeContiguousDaughters i j $ fmap unR ds
                          -- and every way @ss@ of making the
                          -- daughters @ds@ contiguous between @i@ and
                          -- @j@,
  return (R $ situate i j $ unR r,fmap R ss)
                          -- then the rule @([i,r,j], ss)@ is possible

-- | A root/starting bigram is special, in that it must span the
-- entire string.  Also, we do not situate the 'Root' symbol, as it is
-- just telling us which nodes can be roots, and is not a part of the
-- tree we care about.
makeRoots :: [TreeBiGram a] -- ^ The list of starting bigrams
          -> Int -- ^ The length of the input sentence
          -> [TreeBiGram (SituatedNode a)]
makeRoots oldRoots n = do
  (Root,ds) <- oldRoots      -- for every rule @(Root,ds)@
  ss <- makeContiguousDaughters 0 n $ fmap unR ds
                             -- and every way of making the daughters
                             -- (which should really just be a single
                             -- node) span the entire string
  return (Root, fmap R ss)
                             -- then @(Root,ss)@ is possible

-- | Leaf bigrams are also special, in that they represent parts of
-- the string we are trying to parse.  Thus, we do not situate them in
-- all possible ways, but rather only in ways that correctly represent
-- which parts of the input string they are identical to.
makeLeaves :: Eq a =>
              [TreeBiGram a] -- ^ The list of leaves
           -> [a] -- ^ The input sentence
           -> [TreeBiGram (SituatedNode a)]
makeLeaves oldLeaves w = do
  i <- [0..(length w)-1]     -- for every integer @i@ representing a
                             -- word in the sentence
  (R l,[]) <- oldLeaves      -- and for every leaf rule @(leaf,[])@
  guard (w !! i == l)        -- if the leaf is actually the @i@th word
                             -- in the sentence
  return (R $ situate i (succ i) l,[])
                             -- then @([i,leaf,i+1],[])@ is possible

-- | Intersection parsing is just a matter of situating all bigrams in
-- the appropriate ways.
intParse :: Eq a =>
            [TreeBiGram a] -- ^ The grammar
         -> [a] -- ^ The input sentence
         -> [TreeBiGram (SituatedNode a)] -- ^ The intersection grammar
intParse g w = makeRoots roots n ++ makeRules nonleaves n ++ makeLeaves leaves w
  where
    (roots,others) = partition (isRoot . fst) g
                           -- we first separate out the root rules from the others
    (leaves,nonleaves) = partition (null . snd) others
                           -- and then the leaf rules
    n = length w


