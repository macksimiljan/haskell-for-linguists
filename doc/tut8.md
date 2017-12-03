# Tutorium 8: Module & Haddock

![Haddock](https://upload.wikimedia.org/wikipedia/commons/6/69/Melanogrammus_aeglefinus1.jpg)

## Naming Conventions

Was wird hier jeweils benannt (Variable, Funktion, Typ, Modul, Datei)? Ist die Benennung sinnvoll/ korrekt?

1. `var`
2. `i`
3. `firstName`
4. `first_name`
5. `Name`
6. `filterByName`
7. `Trees.hs`
8. `trees`
9. `type`
10. `10names`
11. `tenNames`

## Module
Wir können unseren Programmcode in [Modulen](http://learnyouahaskell.com/modules) organisieren.

> A Haskell module is a collection of related functions, types and typeclasses.

> A Haskell program is a collection of modules
where the main module loads up the other modules and then uses the functions defined in them to do something.

__Aufgabe:__ Warum ist das sinnvoll?

__Beispiel:__ [Data.Set](https://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Set.html),
[LeafyTrees](https://home.uni-leipzig.de/gkobele/courses/2017/Parsing/Code/Day10/LeafyTrees.html)

Zur Suche kann [Hoogle](https://www.haskell.org/hoogle/) verwendet werden.

### Module importieren

Module bzw. Funktionen aus Modulen können wie folgt importiert werden:

```haskell
import Data.Set
import Data.Set (intersection)
import Data.Set hiding (intersection)
import qualified Data.Set
import qualified Data.Set as Set
```

__Aufgabe:__ Erstelle eine Haskell-Datei _WordAnalysis.hs_. Das Modul `Data.Char` beinhaltet u.a. die Prädikate
`isLower`, `isUpper`. Teste verschiedene Arten des Modulimports, indem du ein Prädikat `isGermanNoun :: String -> Bool`
schreibst, welches wahr ist, wenn ein Eingabewort mit einem Großbuchstaben beginnt.

### Module erstellen

Um aus einer Haskell-Datei ein Modul zu machen, muss folgendes angegeben werden:

```haskell
module WordAnalysis
(
   -- exported functions separated by comma
) where

-- module to be imported
-- Prelude is implicitly imported

isGermanNoun :: String -> Bool
-- to be implemented
```

__Aufgabe:__ Wandle den Code von _WordAnalysis.hs_ in ein Modul um. Importiere das Modul in _WordAnalysisTest.hs_
(was auch wieder ein Modul sein könnte) und teste hier die Funktionsweise von `isGermanNoun`.

__Aufgabe:__ Schreibe eine Funktion, die die ("deutschen") Plosive in einem Eingabewort zählt.
Lege dazu in `WordAnalysis` die Funktion `numberPlosives :: String -> Int` an.

## Haddock

Um eine hübsche Dokumentation von annotiertem Quellcode zu erhalten, kann [Haddock](https://www.haskell.org/haddock/)
genutzt werden.

__Aufgabe:__ Stelle sicher, dass Haddock installiert.
```
$ haddock --version

Haddock version 2.16.1, (c) Simon Marlow 2006
Ported to use the GHC API by David Waern 2006-2008
```

### Ohne Annotationen

Dann kannst du mit `$ haddock  -o haddock --html WordAnalysis.hs` die Dokumentation erzeugen.
`-o` gibt den Pfad zum Ausgabeordner an. Öffne hier _index.html_, um die dir Dokumentation anzusehen.

### Mit Annotationen

Eine Übersicht zu den Annotationen gibt es [hier](http://haskell-haddock.readthedocs.io/en/latest/markup.html).
Einige Beispiele:

* `-- | I'm a function comment.`
* `-- ^ I'm an argument comment.`
* `{-| I'm a module comment. -}`
* `-- >>> I'm an example.`
* `-- * I'm a section.`

__Aufgabe:__ Versehe dein Modul `WordAnalysis` mit Annotationen.









