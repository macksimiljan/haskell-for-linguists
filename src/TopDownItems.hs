{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,FlexibleContexts,GADTs #-}
module TopDownItems
  (
    -- * Parsing
    -- ** Abbreviations
    -- $abbreviations1
    Sentence,
    WordOrSymbol,
    Rule,
    Position,
    Predictions,
    History,
    Grammar,
    -- ** Items
    TopDownItem(TopDownItem,position,predictions,history),
    -- ** Operations
    -- $TopDownOperations
    scan,
    predict,
    -- * Parsing as Search
    -- $parsingAsSearch
    Info_TD(Info_TD,unInfo_TD),
    recognizer,
    -- * Assembling trees from histories
    -- $reConstructingTrees
    -- ** Constructing Parse Trees
    getArgs,buildParseTree,parser,
    -- ** Interpreting Parse Trees
    interpretRuleAsTree,
    parse2Trees
  )
where

import Data.Either (rights,isLeft)
import Data.Maybe (mapMaybe,maybeToList)
import LeafyTrees
import LeafyTreeGrams
import LeafySearch

-- $abbreviations1
--
-- We introduce some notation for types that we will use often.  This
-- is intended to make it easier to read the types of the functions we
-- will define.
type Sentence w = [w]
type WordOrSymbol w s = Either w s
type Rule w s = TreeBiGram (WordOrSymbol w s)
type Position = Int
type Predictions w s = [Rooted (WordOrSymbol w s)]
type History w s = [Rule w s]
type Grammar w s = [Rule w s]

-- | A 'TopDownItem' records the information a parser needs to keep
-- track of what we have done thus far and what is left to do
data TopDownItem words sym = TopDownItem {position :: Position, -- ^ the 'position' records how much of the input we have successfully recognized
                                          predictions :: Predictions words sym, -- ^ the 'predictions' record what we expect we will see
                                          history :: History words sym -- ^ the 'history' records how this item was constructed
                                         }
                           deriving (Eq,Ord,Show)

-- $abbreviations2
--
-- We introduce some more notation, both for types, and for their manipulation
type Item w s = TopDownItem w s
-- | 'mkItem' is shorter than 'TopDownItem'
mkItem :: Position -> Predictions w s -> History w s -> Item w s
mkItem = TopDownItem
-- | 'buildHistory' assembles a list of bigrams used, from first to last
buildHistory :: Rule w s -> History w s -> History w s
buildHistory rule hist = hist ++ [rule]
-- | 'updatePredictions' specifies how to turn the right hand side of
-- a rule into predictions, and how to add that to our current
-- predictions.
updatePredictions :: [WordOrSymbol w s] -- ^ The right hand side of some 'Rule'
                  -> Predictions w s -- ^ The old predictions
                  -> Predictions w s
updatePredictions ws preds = makePredictions ws ++ preds
  where
    makePredictions = map r

-- $TopDownOperations
--
-- Our top down parser uses two operations to construct items: /scan/
-- and /predict/.  /Scan/ matches observable predictions with actual
-- data, while /predict/ makes predictions about how something of a
-- particular kind might have come to be.


-- | 'scan' is possible when the next prediction is for a word.  If
-- the predicted word is the same as the next word in the sentence, we
-- succeed.
scan :: Eq words
     => Sentence words -- ^ The input sentence
     -> Item words sym -- ^ The current item
     -> Maybe (Item words sym) -- ^ Might give us a new item
scan w = scan_w
  where
    n = length w
    scan_w (TopDownItem pos (a : bs) his) | not (isRoot a)
                                            && pos < n
                                            && either (\a -> w !! pos == a) (const False) (unR a) =
                                              Just (mkItem (succ pos) bs his)
                                          | otherwise = Nothing
    scan_w _ = Nothing

-- | @'predict' r@ is possible when the next prediction is for a
-- non-terminal which is identical to the left hand side of the bigram
-- rule.  In that case, we refine this prediction by replacing it with
-- predictions for the right hand side of the bigram.
predict :: (Eq sym,Eq words)
        => Rule words sym -- ^ The rule we want to use
        -> Item words sym -- ^ Our current item
        -> Maybe (Item words sym) -- ^ Might give us a new item
predict r@(TreeBiGram x xs) (TopDownItem pos (b:bs) his) | x == b = Just $ mkItem pos (updatePredictions xs bs) (buildHistory r his)
                                                         | otherwise = Nothing
predict _ _ = Nothing

-- $parsingAsSearch
--
-- We wish to view parsing as a search problem.  Accordingly, we want
-- to construct an instance of the 'Search' class which makes use of
-- our top down parser operations /scan/ and /predict/ to construct
-- search states.  Then we can use a search strategy to explore the
-- search space, looking for a final state, which indicates that the
-- sentence in question is grammatical.  To instantiate the 'Search'
-- class we need to specify the type of state (in our case, it will be
-- a 'TopDownItem'), as well as some extra information.

-- | The additional information we need to construct our search space
-- is a 'Grammar' and a 'Sentence'; we need information about the
-- 'Grammar' to make predictions, and information about the 'Sentence'
-- to make contact with the data.
newtype Info_TD words sym = Info_TD {unInfo_TD :: (Grammar words sym,Sentence words)} deriving (Eq,Ord,Show)

instance (Eq words,Eq sym,words ~ words',sym ~ sym') => Search (TopDownItem words' sym') (Info_TD words sym) where
  initialState _ = TopDownItem 0 [root] []
  isFinal (Info_TD (_,w)) (TopDownItem pos pred _) | null pred = length w == pos
                                                   | otherwise = False
  successors (Info_TD (_,w)) item@(TopDownItem _ (b : _) _) | not (isRoot b)
                                                              && isLeft (unR b) = maybeToList $ scan w item
  successors (Info_TD (g,_)) item = mapMaybe (flip predict item) g

-- | We can immediately define a recognizer for a grammar in terms of
-- search: a grammar recognizes a sentence just in case there is a
-- final state in the search tree constructed given the grammar and
-- the sentence (i.e. just in case the list of final states is not
-- empty).  I have chosen a 'breadthFirst' search strategy somewhat
-- arbitrarily.  Because 'search' works for any 'TreeLike' type, we
-- must specify what kind of tree we want the search space to be.  One
-- way to do this would be to explicitly annotate the type of
-- 'search'.  However, its type is long and tedious to write.
-- Instead, I annotate 'breadthFirst' with its type, specifying that I
-- want it to work on the 'TreeLike' type 'Tree', from which Haskell
-- infers that 'search' should construct a search tree of that type.
recognizer :: (Eq words,Eq sym) => Grammar words sym -> Sentence words -> Bool
recognizer g w = not $ null $ search (Info_TD (g,w)) (breadthFirst :: Tree (TopDownItem words sym) -> [TopDownItem words sym])


-- $reConstructingTrees
--
-- In order to construct a parser, we need to find a way of turning a
-- parse history into a syntactic tree.  We will do this in two steps.
-- First, we will turn a history into an abstract __parse tree__,
-- which records which bigrams were used to create predictions that
-- later bigrams refined.  Then we will turn abstract parse trees into
-- syntactic structures by interpreting bigrams as functions which
-- construct trees.

-- | 'getArgs' takes a bigram, and a list of trees as its arguments.
-- It selects one tree from the list for each non-terminal symbol on
-- the right hand side of the bigram.  It then constructs a tree whose
-- root is the bigram, and whose daughters are the trees just
-- selected.  It returns a list of trees whose first element is the
-- tree just constructed, and whose remaining elements are those which
-- are left over in the input list.
getArgs :: Rule words sym -> [Tree (Rule words sym)] -> [Tree (Rule words sym)]
getArgs bg stack | isRoot $ lhs bg = stack
getArgs rule stack = mkTree rule (take n stack) : drop n stack
  where
    n = length $ rights $ rhs rule

-- | 'buildParseTree' constructs a (hopefully singleton) list of parse
-- trees from a parse history by calling 'getArgs' over and over on
-- each element of the history
buildParseTree :: History words sym -> [Tree (Rule words sym)]
buildParseTree = foldr getArgs []

-- | A 'parser' then goes through the search space looking for final
-- states, then extracts their history and constructs an abstract
-- parse tree.
parser :: (Eq words, Eq sym)
       => Grammar words sym
       -> Sentence words
       -> [Tree (Rule words sym)]
parser g w = map (head . buildParseTree . history) $ search (Info_TD (g,w)) (breadthFirst :: Tree a -> [a])


-- | Given a bigram of the form @(A,BCdEf)@, where @A@, @B@, @C@, and
-- @E@ are non-terminals, and @d@ and @f@ are terminals, we interpret
-- it as a /function/ which takes three trees @tB@, @tC@ and @tE@ as
-- arguments, and returns the tree with root node @A@, and with
-- daughters the trees @tB@, @tC@, @'Leaf' d@, @tE@ and @'Leaf' f@ in
-- that order.
interpretRuleAsTree :: (TreeLike tree (WordOrSymbol words sym))
                    => Rule words sym
                    -> [tree]
                    -> tree
interpretRuleAsTree rule ts = mkTree (unR $ lhs rule) $ fst $ foldr replacer ([],reverse ts) $ rhs rule
  where
    replacer l@(Left _) (children,ts) = (mkTree l [] : children,ts)
    replacer r@(Right _) (children,t:ts) = (t : children,ts)

-- | What we might think of as a parser, something that to an input
-- sentence associates the syntactic tree that we expect, can be
-- obtained from our 'parser' function above by interpreting each node
-- in the abstract parse tree as a function for constructing a
-- syntactic tree.
parse2Trees :: (Eq words, Eq sym, TreeLike tree (WordOrSymbol words sym))
            => Grammar words sym
            -> Sentence words
            -> [tree]
parse2Trees g = map (foldTree interpretRuleAsTree) . parser g

