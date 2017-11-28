# Tutorium 7: Leafy-Trees & Top-Down-Parsing

## Warm-up

__Aufgabe:__ Mache dir klar, was der Sinn von `Maybe` ist und verstehe die Implementierung von `famp` 
für diese Klasse: 

```haskell
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing
```

__Aufgabe:__ Die Café-Bewertungen von letzter Woche ergänzen wir wie folgt: Es soll nun möglich sein, nach einem zweiten Besuch im jeweiligen Café die Bewertungen zu verändern.
Dafür leite den Datentyp `Cafe` von der Klasse `Functor` ab. Somit ist es nun beispielsweise möglich, die Bewertungen für Eis, Kaffee und
Croissant jeweils um eine Schulnote nach oben zu setzen.

## Leafy-Trees & Leafy-Search

__Aufgabe:__ Vergleiche die beiden folgenden Definitionen einer Baumstruktur:
```haskell
data Tree node = Node node [Tree node]

data LeafyTree leaf node = Leaf leaf
                         | LNode node [LeafyTree leaf node]
```

__Aufgabe:__ Welchen Zweck erfüllt die folgende Klasse?
```haskell
class TreeLike tree symbol | tree->symbol where
  
  -- | 'mother' returns the symbol at the root of the tree
  mother :: tree -> symbol
  -- | 'daughters' returns the list of daughters of the root of the tree
  daughters :: tree -> [tree]
  -- | 'mkTree' requires a symbol for the root, and a list of its
  -- desired daughters, and then constructs a tree
  mkTree :: symbol -> [tree] -> tree
```

__Hinweis:__ Um die Klassendefinition zu verstehen, ist vielleicht [dieser](https://stackoverflow.com/questions/2675655/whats-the-for-in-a-haskell-class-definition)
Stackoverflow-Post hilfreich.
