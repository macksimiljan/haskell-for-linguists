{-# LANGUAGE TupleSections #-}

module EnumParser
  (
    Enumerable,
    -- $infiniteTypes
    interleave,interleaveList,
    enumParser
  )
where
import Trees
import TreeGrams
import Bigrams

-- | A type is 'Enumerable' just in case all of its inhabitants can be
-- put into a list.  (Ideally without repetitions, but all that is
-- required is that each inhabitant occurs at least once in the list.)
class Enumerable a where
  enumerate :: [a]
-- prop> a `elem` enumerate == True

-- | The unit type is trivially enumerable, as it has just one
-- inhabitant.
instance Enumerable () where
  enumerate = [()]

-- | Similarly, 'Bool' is enumerable, and I choose somewhat
-- arbitrarily the order 'False' before 'True'.
instance Enumerable Bool where
  enumerate = [False,True]

-- | Characters are enumerable, as they are finite in number.  Indeed,
-- the classes 'Enum' and 'Bounded' suffice for enumerability.
instance Enumerable Char where
  enumerate = [minBound..maxBound]

-- $infiniteTypes
--
-- To enumerate types with infinitely many inhabitants, it is helpful
-- to have a way to __fairly__ merge (even infinite) lists together.

-- | The operation 'interleave' creates a new list out of two old ones
-- by alternating the elements of its input lists.
interleave :: [a] -> [a] -> [a]
interleave (b:bs) cs = b : interleave cs bs
interleave [] cs = cs

-- | 'interleaveList' takes a perhaps infinite list of lists, and
-- interleaves them together.  It is crucial that each element of each
-- list occurs at some finite point in the output list.
interleaveList :: [[a]] -> [a]
interleaveList (b:bs) = interleave b $ interleaveList bs
interleaveList [] = []

-- | The 'Integer's are enumerable, by interleaving the positive and
-- negative ones.  The number @i < 0@ is in the list at position @2 *
-- abs i@, and the number @i > 0@ at position @2 * i - 1@.  (0 is of
-- course at position 0.)
instance Enumerable Integer where
  enumerate = 0 : interleave [1,2..] [-1,-2..]

-- | The union of two 'Enumerable' types is itself 'Enumerable', in
-- much the same way as was the 'Integer's: the two types are simply
-- 'interleave'd (with appropriate tags ('Left' and 'Right')
-- indicating to which of the two types the item in question belongs.)
instance (Enumerable a,Enumerable b) => Enumerable (Either a b) where
  enumerate = interleave (fmap Left enumerate) (fmap Right enumerate)

-- | The cross-product of two 'Enumerable' types is also 'Enumerable'.
-- For each element in the type @a@, we construct a list where that
-- element is paired with every element in @b@.  Then we 'interleave'
-- all of those lists together.
instance (Enumerable a,Enumerable b) => Enumerable (a,b) where
  enumerate = interleaveList $ foldr (\a cs -> fmap (a,) enumerate : cs) [] enumerate

-- | The type of lists whose elements are 'Enumerable' is
-- 'Enumerable'.  We 'interleave' the lists of length 0 with those of
-- length 1 with those of length 2 ...  The lists of length n+1 are
-- constructed out of the lists of length n in much the same way as we
-- enumerated the cross-product of enumerable types; we construct one
-- list consisting of all the lists of length n with a new element
-- consed onto them for each inhabitant of the enumerable type.  Then
-- we interleave these lists.
instance Enumerable a => Enumerable [a] where
  enumerate = [] : (interleaveList $ fmap makeList [1..])
    where
      makeList 0 = [[]]
      makeList n = interleaveList $ fmap (\bs -> fmap (: bs) enumerate) $ makeList (n-1)

-- | The type of 'Tree' over an 'Enumerable' type can be enumerated.  We do this by
--
-- 1. enumerating all elements (possible mother nodes)
-- 2. enumerating all possible lists of trees (possible daughters)
-- 3. adding each possible mother node to all possible daughters
-- 4. interleaving the resulting lists.
instance Enumerable a => Enumerable (Tree a) where
  enumerate = interleaveList $ fmap (\a -> fmap (Node a) enumerate) enumerate

-- | A first parsing program can be defined if we know that the set of
-- possible structures is 'Enumerable'.  In that case, we can simply
-- 'enumerate' all the possible structures, and then check them one by
-- one to see if they generate the sentence we are interested in.
-- Note that this procedure does not make /any assumptions at all/
-- about what grammars are, or what structures are, or how sentences
-- are related to structures.  In other words, since we require it to
-- work in such a wide variety of conditions, it cannot take advantage
-- of whatever regularities might exist in a special case we might be
-- interested in.
enumParser :: (Enumerable structures,Eq observable) =>
  (structures -> observable) -- ^ We need a way of making structures observable,
  -> (structures -> Bool) -- ^ and a way of identifying the well-formed structures.
  -> observable -- ^ Then given an observation,
  -> [structures] -- ^ we can enumerate the well-formed structures that could have given rise to it.
enumParser yield isGrammatical sentence = filter isGrammatical $ filter ((==) sentence . yield) $ enumerate

