# Tutorium 3: Haskell-Übung

## Warm-up

__Aufgabe:__ Mache dich bereit zum Programmieren, z.B. indem du deinen Editor öffnest und GHCI startest.
Mit welchem Befehl wird a) eine Haskell-Datei geladen, b) diese Datei erneut geladen und c) GHCI beendet?

## Fehlersuche

__Aufgabe:__ Berichtige die Fehler im folgenden Code:

```haskell
STrangesUM = + a length xs
    where
      a = 37
     xs = [1, 2, 3, 4, 5]
```

__Aufgabe:__ Um welchen Datentyp handelt es sich hier?

## Deep Thought

__Aufgabe:__ Erstelle eine Variable `theAnswer` mit dem Wert 42.

__Aufgabe:__ Schreibe eine Funktion `deepThought`,
welche als Eingabe eine Frage bekommt und als Ausgabe hat _The answer to your ultimate question is: <theAnswer>_, wobei
_<theAnswer>_ für den Wert der Konstanten steht. Stößt du auf Probleme?

__Aufgabe:__ Definiere ebenfalls den Typ der Funktion.

## Das Allerletzte

__Aufgabe:__ Schreibe eine Funktion `finalInduc`, welche induktiv das letzte Element einer Liste bestimmt.

__Aufgabe:__ Schreibe eine Funktion `finalTake`, welche mittels der Funktion `take` das letzte Listenelement zurückgibt.

__Aufgabe:__ Teste beide Funktionen mit `[1, 2, 3, 4]`, `[ [ ] ]` `[ ]`.


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