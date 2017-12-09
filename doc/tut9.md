# Tutorium 9: LeafyTrees & Top-Down-Parsing

Gegeben sei die folgende CFG:
```
S   -->  NP VP
NP  -->  PN
NP  -->  Det N
VP  -->  Vi
VP  -->  Vt NP
Det -->  a | the
N   -->  car | truck | anvil
PN  -->  Bugs | Daffy
Vi  -->  fell over
Vt  --> hit
```

__Aufgabe:__ Erstelle für jeden der folgenden Trainingssätze einen LeafyTree:
* _Daffy fell over._
* _The anvil hit Daffy._
* _A truck fell over._
* _A truck hit Bugs._

__Hinweis:__ Modul `ParseTest`.

__Aufgabe:__ Berechne für jeden Trainingssatz seine Baum-Bigramme. 
Zur besseren Darstellung der Bigramme lege ein Modul `Utilities` an
und implementiere dort eine Funktion `grammar2Str :: (Show a) => [TreeBiGram a] -> String`,
welche eine Liste von Bigrammen auf eine gut lesbaren String abbildet.
Der Aufruf in der Konsole erfolgt dann über:
```
>  putStr $ grammar2Str bigrams_daffyFellOver
Nothing	-->	Right "S"	
Just (Right "S")	-->	Right "NP"	Right "VP"	
Just (Right "NP")	-->	Left "Daffy"	
Just (Right "VP")	-->	Right "Vi"	
Just (Right "Vi")	-->	Left "fell over"	
```

__Hinweis:__ Modul `LeafyTreeGrams`.

__Aufgabe:__ Suche im Baum zu _The anvil hit Daffy._ das Blatt _the_.
Nutze dazu
1. Tiefensuche,
2. Breitensuche,
3. Beste-First-Suche.
Nach wie vielen Schritten wird das Blatt gefunden? Die Schritten können z.B.
nach folgendem Schema berechnet werden:
```
> length $ takeWhile (\x -> x /= 3) [42, 21, 0, 3, 10]
2
```

__Hinweis:__ Modul `LeafySearch`.

__Aufgabe:__ Erstelle je eine Variable vom Typ
* `Sentence`
* `WordOrSymbol`
* `Rule`
* `Position`
* `Predictions`
* `History`
* `Grammar`

Überprüfe, ob die Variablen auch tatsächlich den geforderten Typ haben. Können zwei Variablen
vom Typ `History` bzw. `Grammar` identisch sein, also kann `myHistory == myGrammar` `True`
zurückgeben?

__Hinweis:__ Modul `TopDownItems`.


__Aufgabe:__ Erstelle eine Grammatik `grammer` basierend auf den Trainingssätzen.

__Aufgabe:__ Erstelle einen Parser `bugsParser` und einen Recognizer `bugsRecognizer` 
analog zu `parse` und `recog` aus `ParseTest`.  Teste, ob die Trainingssätze richtig behandelt werden
und überlege dir Testsätze.


__Aufgabe:__

__b)__ Gegeben folgende Definition von `TopDownItem`
```haskell
-- | A 'TopDownItem' records the information a parser needs to keep
-- track of what we have done thus far and what is left to do
data TopDownItem words sym =
        TopDownItem { position :: Position, -- ^ the 'position' records how much of the input we have successfully recognized
                      predictions :: Predictions words sym, -- ^ the 'predictions' record what we expect we will see
                      history :: History words sym -- ^ the 'history' records how this item was constructed
                     }
                     deriving (Eq,Ord,Show)
```
warum wird kurz danach folgendes implementiert?
```haskell
type Item w s = TopDownItem w s
mkItem :: Position -> Predictions w s -> History w s -> Item w s
mkItem = TopDownItem
```

__c):__ Was macht `buildHistory`?
```haskell
buildHistory :: Rule w s -> History w s -> History w s
buildHistory rule hist = hist ++ [rule]
```

__d):__ Gegeben
```haskell
r :: a -> Rooted a
r = Just
```
was macht `updatePredictions`?
```haskell
updatePredictions :: [WordOrSymbol w s] -- ^ The right hand side of some 'Rule'
                  -> Predictions w s -- ^ The old predictions
                  -> Predictions w s
updatePredictions ws preds = makePredictions ws ++ preds
  where
    makePredictions = map r
```


