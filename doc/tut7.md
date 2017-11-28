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

__Aufgabe:__ Speichere jeden Teilbaum aus der Baumstruktur für "Hans lacht." in einer Variablen ab:
1. für Bäume vom Typ `Tree`,
2. für Bäume vom Typ `LeafyTree`.
Importiere dafür das Modul `LeafyTrees`. Erstelle die Teilbäume mit `mkTree :: symbol -> [tree] -> tree`. Die Blätter müssen mit Typ annotiert sein. Beispiel:
```haskell
hans = mkTree "Hans" [] :: Tree String

hans' = mkTree (Left "Hans") [] :: LeafyTree String String
```

__Hinweis:__ Verwende bei Leafy-Trees `Left` für Terminale, `Right` für Nicht-Terminale. Hintergrund:
1. [Data.Either](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Either.html)
2. `instance TreeLike (LeafyTree leaf node) (Either leaf node) where`
