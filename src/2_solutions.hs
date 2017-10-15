-- | Def: Buchstabe (symbol)
data Symbol = A | B | C deriving(Show)

-- | Def 1: Wort (word)
data Word_1 = EmptyWord | Word_1 Symbol Word_1
instance Show Word_1 where
    show EmptyWord    = "#"
    show (Word_1 s w) = show(s) ++ ":" ++ show(w)

-- | Def 2: Wort
data Word_2 = EmptyWord | Word_2 Word_2 Symbol
instance Show Word_2 where
    show EmptyWord    = "#"
    show (Word_2 w s) = show(w) ++ ":" ++ show(s)

