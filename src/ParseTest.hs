{-# LANGUAGE FlexibleContexts #-}

module ParseTest
  (
    -- * Language and Grammar
    -- $constructions
    -- ** Basic Structures
    ivp,tvp,cvp,pp,name,
    -- ** Modification
    -- $modification
    noun_modifier, dp,
    -- ** Movement
    -- $movement
    nptrace,gap_ivp,gap_tvp,tvp_gap,gap_cvp,cvp_gap,
    -- $dischargingTraces
    rel,
    -- ** Adding Words
    -- $lexicalizedConstructions
    john, mary, theTiger, theLion, slept, bit, roared, thinks,
    thatXScratched, thatXBit, johnBitTheTiger, theTigerSlept,
    maryThinksThatTheTigerThatJohnBitRoared, theLionThatMaryScratchedSlept,
    theLionThatTheTigerThatMaryBitScratchedRoared,
    -- * Grammars and their Use
    g, parse, recog,
    -- ** A test suite
    test1, test2
  )
where
import TopDownItems
import LeafySearch
import Data.Maybe
import LeafyTrees
import LeafyTreeGrams
import Data.List (nub,union)

-- $constructions
--
-- It is convenient to define abbreviations to allow for easier tree
-- construction.

-- | 'ivp' is the structure that an intransitive verb occurs in: it
-- needs a subject, and an intransitive verb.  The subject is the left
-- sister of a "VP", and this "VP" contains just a verb.
ivp :: String -> LeafyTree String String -> LeafyTree String String
ivp v s = mkTree (Right "S")
            [s,
             mkTree (Right "VP")
             [mkTree (Right "Vi")
               [mkTree (Left v) []]
             ]
            ]
-- | 'tvp' is the structure that a transitive verb occurs in: it needs
-- a subject, an object, and a transitive verb.  The subject is the
-- left sister of a "VP", and this "VP" contains both a verb and an
-- object, in that order.
tvp :: String
    -> LeafyTree String String
    -> LeafyTree String String
    -> LeafyTree String String
tvp v s o = mkTree (Right "S")
            [s,
             mkTree (Right "VP")
              [mkTree (Right "Vt")
                [mkTree (Left v) [],
                 o
                ]
              ]
            ]
-- | 'cvp' is the structure that a sentential complement taking verb
-- occurs in: it needs a subject, a sentential complement, and a
-- transitive verb.  The subject is the left sister of a "VP", and
-- this "VP" contains both a verb and a /that/, which is sister to the
-- sentential complement.
cvp :: String
    -> LeafyTree String String
    -> LeafyTree String String
    -> LeafyTree String String
cvp v s c = mkTree (Right "S")
            [s,
             mkTree (Right "VP")
              [mkTree (Right "Vc")
                [mkTree (Left v) [],
                 mkTree (Right "S'")
                  [mkTree (Left "that") [],
                   c
                  ]
                ]
              ]
            ]
-- | A 'pp' consists of a "PP" which dominates a preposition and a noun phrase
pp :: String -> LeafyTree String String -> LeafyTree String String
pp p np = mkTree (Right "PP")
          [mkTree (Left p) [],
            np
          ]

-- | Proper names are "NP"s
name :: String -> LeafyTree String String
name n = mkTree (Right "NP")
         [mkTree (Left n) []]


-- $modification
--
-- certain expressions can be added /ad libitum/ to others; we call
-- these __adjuncts__.

-- | Nouns are good targets of modification.  A nominal modifier is
-- something that attaches to a nominal, returning a nominal ("N'").
noun_modifier :: LeafyTree String String -> LeafyTree String String -> LeafyTree String String
noun_modifier xp n = mkTree (Right "N'")
                     [n,
                      xp
                     ]
-- | The syntactic treatment of a DP (NP) below, borrowing from the
-- Tree Adjoining Grammar literature, reifies the possibility of
-- having adjuncts (either PP adjuncts or relative clauses), which are
-- themselves functions from trees to trees, by lifting the type of a
-- DP to take such a function (here @f@) as an argument.  This permits
-- the DP to specify where the adjunct may attach (here: above the @N@
-- but below the @NP@).
dp :: String
   -> String
   -> (LeafyTree String String -> LeafyTree String String)
   -> LeafyTree String String
dp d n f = mkTree (Right "NP")
           [mkTree (Right "Det")
             [mkTree (Left d) [],
              f (mkTree (Right "N")
                  [mkTree (Left n) []])
             ]
           ]

-- $movement
--
-- We allow for movement by keeping track of the extraction path.
-- Following the literature on Generalized Phrase Structure Grammar, a
-- category @XP[YP]@ represents an @XP@ that contains (somewhere) a
-- @YP@ trace.

-- | We begin with a (NP) trace itself, which, categorically speaking,
-- is a @NP@ that, exclusively, contains a @NP@ trace.
nptrace = mkTree (Right "NP[NP]") []

-- | An intransitive sentence whose subject is a trace is a sentence
-- which contains a np trace (@S[NP]@).
gap_ivp :: String -> LeafyTree String String
gap_ivp v = mkTree (Right "S[NP]")
            [nptrace,
             mkTree (Right "VP")
              [mkTree (Right "Vi")
                [mkTree (Left v) []]
              ]
            ]
-- | Similarly, a transitive sentence whose subject is a trace is a
-- sentence containing a np trace (@S[NP]@).
gap_tvp :: String
        -> LeafyTree String String
        -> LeafyTree String String
gap_tvp v o = mkTree (Right "S[NP]")
              [nptrace,
               mkTree (Right "VP")
                [mkTree (Right "Vt")
                  [mkTree (Left v) [],
                   o
                  ]
                ]
              ]
-- | But transitive sentences can contain traces as well in the object
-- position.
tvp_gap :: String
        -> LeafyTree String String
        -> LeafyTree String String
tvp_gap v s = mkTree (Right "S[NP]")
              [s,
               mkTree (Right "VP[NP]")
                [mkTree (Right "Vt")
                  [mkTree (Left v) [],
                   nptrace
                  ]
                ]
              ]
-- | Sentential complement taking verbs may have subject np traces.
gap_cvp :: String
        -> LeafyTree String String
        -> LeafyTree String String
gap_cvp v c = mkTree (Right "S[NP]")
              [nptrace,
               mkTree (Right "VP")
                [mkTree (Right "Vc")
                  [mkTree (Left v) [],
                   mkTree (Right "S'")
                    [mkTree (Left "that") [],
                     c
                    ]
                  ]
                ]
              ]
-- | Although transitive, sentential complement taking verbs cannot
-- have np traces in their object position, as their objects are not
-- NPs.  But their objects might be sentences which themselves contain
-- NP traces.  In this case, the matrix sentence contains a np trace.
cvp_gap :: String
        -> LeafyTree String String
        -> LeafyTree String String
        -> LeafyTree String String
cvp_gap v s c = mkTree (Right "S[NP]")
                [s,
                 mkTree (Right "VP[NP]")
                  [mkTree (Right "Vc")
                    [mkTree (Left v) [],
                     mkTree (Right "S'[NP]")
                      [mkTree (Left "that") [],
                       c
                      ]
                    ]
                  ]
                ]

-- $dischargingTraces
--
-- Keeping track of traces in this way allows us to decide when we
-- might have found a moving expression.

-- | Relative clauses are analyzed as involving movement, sometimes of
-- a silent operator.  We can simply say that a relative clause needs
-- to involve a trace, without being forced to say that there was
-- something definite that moved.
rel :: LeafyTree String String -> LeafyTree String String
rel sgap = mkTree (Right "S'[NP]")
           [mkTree (Left "that") [],
             sgap
           ]

-- $lexicalizedConstructions
--
-- Using these basic sentence frames, we can define the actual trees
-- we want by filling them with words.
john = name "John"
mary = name "Mary"
theTiger = dp "the" "tiger"
theLion = dp "the" "lion"
slept = ivp "slept"
bit = tvp "bit"
roared = ivp "roared"
thinks = cvp "thinks"
thatXScratched = noun_modifier . rel . tvp_gap "scratched"
thatXBit = noun_modifier . rel . tvp_gap "bit"
johnBitTheTiger = bit john $ theTiger id
theTigerSlept = slept $ theTiger id
maryThinksThatTheTigerThatJohnBitRoared = thinks mary $ roared $ theTiger $ thatXBit john
theLionThatMaryScratchedSlept = slept $ theLion $ thatXScratched mary
theLionThatTheTigerThatMaryBitScratchedRoared = roared $ theLion $ thatXScratched $ theTiger $ thatXBit mary

-- | Our grammar is just the collection of the bigrams from the sentences above:
g :: Grammar String String
g = []
    `union` treeBiGram johnBitTheTiger
    `union` treeBiGram theTigerSlept
    `union` treeBiGram maryThinksThatTheTigerThatJohnBitRoared
    `union` treeBiGram theLionThatMaryScratchedSlept

-- | We can specialize a parser for this grammar.  It is fun to use
-- the 'words' function from the __Prelude__ as well, which breaks a
-- string into a list of the words it contains.
parse :: String -> [LeafyTree String String]
parse = parse2Trees g . words


-- | Similarly, we can specialize a recognizer for this grammar, using
-- the 'words' function to allow us to write a sentence, instead of a
-- list of words.
recog :: String -> Bool
recog = recognizer g . words

-- | This is a grammatical sentence (involving center embedding).
-- Let's see what happens when we try to parse it.
test1 = parse "the lion that the tiger that Mary bit scratched roared"
-- | This is an ungrammatical sentence (although sometimes people
-- report it as being acceptable; it is called a 'grammatical
-- illusion').  Let's see what happens when we try to parse it.
test2 = parse "the lion that the tiger that Mary bit scratched John"

