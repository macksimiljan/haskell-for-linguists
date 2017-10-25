classes: Eq, Functor
currying

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
    
data Maybe a = Just a | Nothing
  deriving (Eq, Ord)    
```

Sprache _L_
Bigram-Model i) zur Gültigkeit von Wörtern, ii) zum Auflisten aller Bigramme in einem Wort
