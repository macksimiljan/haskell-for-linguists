# Tutorium 7: Leafy-Trees

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

## Leafy-Trees

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

__Aufgabe:__ Zur Vereinfachung sind im Modul `ParseTest` u.a. folgende Funktionen definiert:
```haskell
-- | Proper names are "NP"s
name :: String -> LeafyTree String String

-- | 'ivp' is the structure that an intransitive verb occurs in: it
-- needs a subject, and an intransitive verb.  The subject is the left
-- sister of a "VP", and this "VP" contains just a verb.
ivp :: String -> LeafyTree String String -> LeafyTree String String

-- | 'tvp' is the structure that a transitive verb occurs in: it needs
-- a subject, an object, and a transitive verb.  The subject is the
-- left sister of a "VP", and this "VP" contains both a verb and an
-- object, in that order.
tvp :: String
    -> LeafyTree String String
    -> LeafyTree String String
    -> LeafyTree String String   
```
Repräsentiere die Sätze "Arthur slept." und "Arthur liked Mary." nun mit Haskell.

__Aufgabe:__ Unser nächstes Ziel ist der Satz "Arthur liked Mary that slept." Für die Analyse ist das Konzept der Bewegung relevant. 
Dabei bedeutet _XP[YP]_, dass eine _YP_ aus einer _XP_ herausbewegt wurde.
1. Erstelle eine intransitive VP mit herausbewegtem Subjekt für das Verb _slept_:
```haskell
-- | An intransitive sentence whose subject is a trace is a sentence
-- which contains a np trace (@S[NP]@).
gap_ivp :: String -> LeafyTree String String
```
2. Daraus wird nun der Relativsatz "that slept":
```haskell
-- | Relative clauses are analyzed as involving movement, sometimes of
-- a silent operator.  We can simply say that a relative clause needs
-- to involve a trace, without being forced to say that there was
-- something definite that moved.
rel :: LeafyTree String String -> LeafyTree String String
```
3. Nächster Schritt in der Derivation ist das Erstellen von "Mary that slept". Gibt es hier nun ein Problem? Überlege dir, wie Derivationen 'vorangetrieben' werden. Ist `noun_modifier` die Lösung?
```haskell
-- | Nouns are good targets of modification.  A nominal modifier is
-- something that attaches to a nominal, returning a nominal ("N'").
noun_modifier :: LeafyTree String String -> LeafyTree String String -> LeafyTree String String
noun_modifier xp n = mkTree (Right "N'")
                     [n,
                      xp
                     ]
```
