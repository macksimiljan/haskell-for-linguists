-- | Def: Buchstabe (symbol)
data Symbol = A | B | C deriving(Show)

-- | Def 1: Wort (word)
data Word_1 = EmptyWord | Word_1 Symbol Word_1
instance Show Word_1 where
    show EmptyWord    = "#"
    show (Word_1 s w) = show(s) ++ ":" ++ show(w)

-- | Laenge eines Wortes
wordLength :: Word_1 -> Int
wordLength EmptyWord    = 0
wordLength (Word_1 s w) = 1 + wordLength w

-- | Verkette zwei Woerter
wordConcat :: Word_1 -> Word_1 -> Word_1
wordConcat EmptyWord v      = v
wordConcat (Word_1 s1 w1) v = Word_1 s1 (wordConcat w1 v)

-- | Def 2: Wort
data Word_2 = EmptyWord_2 | Word_2 Word_2 Symbol
instance Show Word_2 where
    show EmptyWord_2   = "#"
    show (Word_2 w s)  = show(w) ++ ":" ++ show(s)

