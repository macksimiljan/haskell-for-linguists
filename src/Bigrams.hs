{- | This module introduces our first notion of a grammar; a way of
   specifying a set of objects as being 'grammatical', and by
   extension, everything else as being 'ungrammatical'.  -}

module Bigrams
(
  -- * Bounded Alphabets
  --
  -- $bounding
  Bounds,
  makeBounded,
  -- * Bigrams
  --
  -- $bigrams
  Bigram,
  -- ** Bigram extraction
  --
  -- $bilearning
  text2Grams,text2Grams',
  -- ** Bigram checking
  --
  -- $bichecking
  isLicensed,isLicensed',
  -- $biunchecking
  isNotProhibited,isNotProhibited',
  -- * N-grams
  --
  -- $ngrams
  NGram,
  zipN,
  textNGrams,textNGrams',
  licensedNGram
  )
where

-- 'catMaybes' has type @['Maybe' a] -> [a]@.  It removes 'Nothing'
-- from a list, and turns a @'Just' a@ into an @a@.
import Data.Maybe (catMaybes)

-- $bounding
--
-- We would like to specify which symbols can be at the beginning, and
-- which at the end, of words.  In order to do this we need to /reify/
-- the notion of word-beginning and word-end.  We do this by defining
-- a new type (function), one which takes a type of symbols, and
-- returns the type of those symbols with boundaries.

-- | In order to include information about which letters may begin a
-- word, and which may end a word, we include two new symbols 'Start'
-- and 'Finish' which represent word boundaries.  Something of type
-- @'Bounds' a@ is either itself an @a@ (but written as @'B' a@) or is
-- a boundary symbol 'Start' or 'Finish'.
data Bounds a = Start | Finish | B a deriving (Eq,Ord)

-- | Anything of type @'Bounds' a@ has an upper bound, 'Finish', and a
-- lower bound, 'Start'.
instance Bounded (Bounds a) where
  maxBound = Finish
  minBound = Start
  
-- | If I were to ask Haskell to derive a 'Show' instance for 'Bounds'
-- automatically, it would print @'B' a@ as "B a".  I want it to
-- simply ignore the 'B', so I have to tell it what to do myself.
instance Show a => Show (Bounds a) where
  show (B a) = show a
  show Start = "Start"
  show Finish = "Finish"

-- | As 'Bounds' stores information, it can be viewed as a 'Functor'.
instance Functor Bounds where
  fmap f (B a) = B (f a)
  fmap f Start = Start
  fmap f Finish = Finish

-- | Given a list of objects, we can bound the list by placing a
-- 'Start' symbol in front, a 'Finish' symbol in back, and prefixing a
-- 'B' on all of the list items.
makeBounded :: [a] -> [Bounds a]
makeBounded t = [Start] ++ fmap B t ++ [Finish]

-- $bigrams
--
-- A bigram grammar specifies a set of pairs of symbols.  We can
-- understand a pair of symbols as specifying whether it is possible
-- or not to have that pair of symbols be adjacent to one another (in
-- that order) in a string.  We will investigate how to /learn/ a
-- bigram grammar from data, and how to /use/ a bigram grammar to
-- decide whether a string is grammatical or not.

-- | A Bigram is just a pair of Bounded items.  We introduce this
-- definition as a /type synonym/.  A type synonym does not define
-- anything new, it just introduces an abbreviation for an already
-- existing type.
type Bigram a = (Bounds a,Bounds a)

-- $bilearning
--
-- The problem of grammar learning in the context of bigrams is very
-- straightforward.  The problem is, in brief, obtaining a smallest
-- bigram grammar that licenses all words in a corpus.  A grammar is
-- /smaller/ than another just in case it licenses a subset of the
-- other's words.
--
-- There are two natural ideas that we can employ to define the same
-- learning algorithm.

-- | The first idea begins with the observation that the bigrams which
-- occur in a given word are exactly those which obtain if we pair up
-- each symbol with the symbol which follows it.  We achieve this by
-- aligining two copies of the same list, and then shifting one back
-- by one symbol.
text2Grams :: [a] -> [Bigram a]
text2Grams t = zip t' (tail t')
  where
    t' = makeBounded t

-- | The second idea is more algorithmic in nature: it is that the way
-- of determining which bigrams occur in a word is to record each pair
-- of symbols in the word.
text2Grams' :: [a] -> [Bigram a]
text2Grams' = t2g . makeBounded
  where
    t2g (b:bs@(c:cs)) = (b,c):(t2g bs)
    t2g _ = []

-- $bichecking
--
-- We can use (bigram) grammars as /classifiers/; as ways of grouping
-- the objects we are talking about (here: strings) into distinct
-- /classes/.  The most common classes in linguistic contexts are
-- /grammatical/ and /ungrammatical/ (or equivalently /good/-/bad/,
-- /acceptable/-/unacceptable/, or any other 2-way classification).
-- This, i.e. when we are attempting to decide whether an object
-- belongs to one of a set of two classes, is commonly called the
-- __recognition problem__ for a grammar.
--
-- We think of a bigram grammar as specifying the set of /allowed/
-- bigrams.  Abstractly, to solve the recognition problem for bigram
-- grammars, we want to verify that all bigrams in a string are
-- allowed by the grammar.

-- | An intuitive way of approaching a solution to the recognition
-- problem for bigram grammars is to simply collect all bigrams in a
-- string, and then to verify that they are allowed by the grammar.
-- This approach allows us to reuse our previous algorithms for
-- collecting bigrams.
isLicensed :: Eq a => [Bigram a] -> [a] -> Bool
isLicensed g = all (`elem` g) . text2Grams

-- | An alternative is to walk through the string one letter at a
-- time, and verify that the subsequent letter is a possible
-- next-letter according to the grammar.
isLicensed' :: Eq a => [Bigram a] -> [a] -> Bool
isLicensed' g = lic . makeBounded
  where
    lic (c:cs@(d:ds)) = elem (c,d) g && lic cs
    lic _ = True


-- $biunchecking
--
-- Alternatively, we may think of a bigram grammar as specifying the
-- /prohibited/ bigrams (and thus as specifying the permitted ones
-- indirectly as "everything else").  Again, the general approach to
-- checking that a sentence is not prohibited is to verify that none
-- of its bigrams are illegal.

-- | The simplest way to implement this is to simply write down what
-- we want to say; first we collect the bigrams in the word, and then
-- we verify that none of them are prohibited by the grammar.
isNotProhibited :: Eq a => [Bigram a] -> [a] -> Bool
isNotProhibited g = all (`notElem` g) . text2Grams

-- | An alternative is to walk through the string letter by letter,
-- each time verifying that the next letter is not an illegal one
-- according to the grammar.
isNotProhibited' :: Eq a => [Bigram a] -> [a] -> Bool
isNotProhibited' b = unlic . makeBounded
  where
    unlic (c:cs@(d:ds)) = notElem (c,d) b && unlic cs
    unlic _ = True

-- $ngrams
--
-- Generalizing from the case of bigrams, we see that we could just as
-- easily look at /trigrams/, or /4-grams/, or ...  While we could
-- implement types and functions for each kind of /n-gram/, these
-- would invariably be doing "the same thing", and we would miss out
-- on a clean statement of the underlying generalization.

-- | An n-gram is a list of Bounded items of length @n@.  A 2-gram is
-- a list of length 2, which is isomorphic to a pair (i.e. to a
-- 'Bigram').  
type NGram a = [Bounds a]

fork :: (a -> b) -> (a -> c) -> a -> (b,c)
fork f g x = (f x, g x)

safeHead :: [a] -> Maybe a
safeHead (b:_) = Just b
safeHead _ = Nothing

-- | We would like to take the list of heads of each sentence as an
-- /n-gram/, and then continue looking for n-grams amongs the list of
-- tails of each sentence.  There is some minor complication in that
-- we need to know when we have run out of elements of a list.  We
-- achieve this by using a /safe/ version of the @head@ operation,
-- which returns 'Nothing' if the list in question is empty.  Then we
-- can check if 'Nothing' is in the list of heads we have just
-- collected.  If it is, we know that one of the lists is empty, and
-- so we should stop trying to zip them together.  Otherwise, we carry
-- on.
zipN l = if Nothing `elem` hd then []
              else (catMaybes hd) : zipN (fmap tail l)
  where
    hd = fmap safeHead l

-- | The essential idea is that we want to zip together @n@ copies of
-- the input sentence, each one shifted one symbol back.  (This is
-- thus a generalization of the @text2Grams@ function.)  We do this by
-- first copying the sentence @n@ times.  Then, we would like to shift
-- each copy back by one step per copy (so the original doesn't get
-- shifted at all, the first copy gets shifted back by one step, the
-- second by two, etc).  "Shifting back by one step" is implemented
-- via the @tail@ operation.  We make a list of functions which just
-- apply the @tail@ operation some number of times, such that the
-- first function doesn't apply it at all, the second function applies
-- it once, the third twice, the fourth thrice, etc, and we zip this
-- function with the list of copies of the sentence.  Then each
-- sentence is associated with a function that will shift it back the
-- appropriate number of symbols.  We then apply each function to the
-- sentence it is associated with, to get the desired list of shifted
-- copies of the original sentence.  Then we can simply call
-- @zipN@ to take care of the rest.
textNGrams :: Eq a => Int -> [a] -> [NGram a]
textNGrams n = zipN . fmap (uncurry ($)) . zip tailOp . replicate n . makeBounded
  where
    tailOp = iterate (tail . ) id

-- | Alternatively, we can simply walk through the list, each time
-- taking the next @n@ segments as an n-gram.
textNGrams' :: Eq a => Int -> [a] -> [NGram a]
textNGrams' n = getN . makeBounded
  where
    getN bs | length nbs == n = nbs : getN (tail bs)
            | otherwise = []
      where
        nbs = take n bs

-- | Checking whether a sentence is licensed by an n-gram grammar then
-- proceeds exactly as in the bigram case, except that we need to
-- determine what the @n@ in question is.  We can do this by looking
-- at how long the n-grams of the grammar are.
licensedNGram :: Eq a => [NGram a] -> [a] -> Bool
licensedNGram g = and . fmap (`elem` g) . textNGrams n
  where
    n = length $ head g


