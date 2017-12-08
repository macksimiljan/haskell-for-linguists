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

__Aufgabe:__ Erstelle für jeden der folgenden Testsätze einen LeafyTree:
* _Daffy fell over._
* _The anvil hit Daffy._
* _A truck fell over._
* _A truck hit Bugs._

__Hinweis:__ Modul `ParseTest`.

__Aufgabe:__ Berechne für jeden Testsatz seine Baum-Bigramme. 
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
wie folgt berechnet werden:
```
> length $ takeWhile (\x -> x > 3) [42, 21, 0, 3, 10]
2
```

__Hinweis:__ Modul `LeafySearch`.

__Aufgabe:__

__Hinweis:__ Modul `TopDownItems`.