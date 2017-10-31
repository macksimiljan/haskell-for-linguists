# Tutorium 4: Haskell-Übung

## Warm-up

__Aufgabe:__ Welche Klassen und Typen kennst du in Haskell? Wie sind sie miteinander verbunden?

__Aufgabe:__ Erkläre, was man a) unter Lambda-Notation und b) unter _Currying_ von Funktionen versteht.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
    
data Maybe a = Just a | Nothing
  deriving (Eq, Ord)    
```

## Klassenbewusstsein

Gegeben sei folgender neuer Listendatentyp für ganze Zahlen:
```haskell
data ComparableList = EmptyList | ComparableList Int ComparableList deriving (Show)
```
Diese Listen sollen anhand ihrer Länge vergleichbar sein.

__Aufgabe:__ Definiere die Funktion `lengthCL :: ComparableList -> Int`, welche die Länge bestimmt.
Teste deine Funktion.

__Aufgabe:__ Erstelle zwei Variablen `list1` und `list2` vom Typ `ComparableList`. Warum wirft `list1 == list2`
einen Fehler? Ändere die Typdefinition entsprechend, um diese Operation zur ermöglichen.

__Aufgabe:__ Nun soll folgende Operationen ermöglicht werden: `list1 >= list2` und `min list1 list2`.
Was muss nun also noch zusätzlich implementiert (/dem Computer mitgeteilt werden)?

__Aufgabe:__ Vom Listentyp `[a]` kennen wir folgende Operation: `map (\x -> 2*x) [1,2,3]`. Was tut sie?
Warum funktioniert nicht `map (\x -> 2*x) list1`?

__Aufgabe:__ Implementiere eine Funktion `mapCL`, welche es ermöglicht, die Elemente in einer List vom Type 
`ComparableList` mithilfe einer gegebenen Funktion `f` zu ändern. Es soll somit folgender Aufruf möglich sein:
`mapCL (\x -> 2*x) list1`.


## Wörter
__Aufgabe:__ Versuche zu verstehen, was der folgende Code macht. Es geht allein um
die Lesekompetenz von Code.

```haskell
-- | Buchstabe
data Symbol = A | B | C deriving(Show)

-- | Wort
data Wort = LeeresWort | Wort Symbol Wort
instance Show Wort where
    show LeeresWort    = "#"
    show (Wort s w) = show(s) ++ ":" ++ show(w)
```    

__Aufgabe:__ Kopiere den Code in eine Datei und speichere diese z.B. unter `wort.hs` ab. Navigiere zu dem Ordner, wo du
die Datei gespeichert hast, öffne hier die Haskell-Konsole mit `$ ghci` und lade die Datei mit `> :load word.hs`. 
Interagiere mit dem Code, beispielsweise durch
   * `> :type A`
   * `> :type Wort`
   * `> :type leereswort`
   * `> :type LeeresWort`
   * `> Word A LeeresWort`
   
## Bäume

Ein Baum wird wie folgt definiert:

```haskell
data Tree a = Node a [Tree a] deriving Show
```

__Aufgabe:__ Erstelle einen Baum mit mindestens drei Knoten.

__Aufgabe:__ Schreibe eine Funktion `isLeaf`, die `True` zurück gibt, wenn der Eingabe-Baum ein Blatt ist,
ansonsten `False`.

__Aufgabe:__ Schreibe eine Funktion `mother`, welche den Wurzelknoten eines Baums ausgibt.

__Aufgabe:__ Schreibe eine Funktion `daugthers`, welche die Töchter des Wurzelknotens eines Baums ausgibt.

__Aufgabe:__ Gehe durch den Quellcode von _src/basic_tress.hs_ von Greg Kobele.

Sprache _L_
Bigram-Model i) zur Gültigkeit von Wörtern, ii) zum Auflisten aller Bigramme in einem Wort
