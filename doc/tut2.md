# Tutorium 2: Wörter

## Warm-up

Gegeben sei ein Alphabet Σ = {a}.

* Was ist ein Buchstabe/ Zeichen?
* Welche Wort-/ Zeichenketten-Definitionen wurden in der Vorlesung vorgestellt?
* Nach welcher Definition sind folgende Werte Worte:
    1. `a:a:#`
    2. `#:a:a:`
    3. `a:a:a`
    3. `[]`
    4. `aaa`
    4. `a`
    5. `#:a:a:#`
    6. `a:#:#`
    7. `(a+a)`
    8. `[] + (a + [])`
    
* Was ist der Definitions- und Wertebereich von `laenge` und `verkette`?
* In wie weit beeinflusst die Wahl einer Wort-Definition die Definition von `laenge` und `verkette`?

## Wort-Implementierung in Haskell

Mithilfe von `:type <arg>` lässt sich überprüfen, welcher Datentyp ein Objekt hat.
Der Datentyp einer Funktion ist ihr Definitions- und Wertebereich.
Ein Platzhalter/ Eine Variable für einen Datentyp wird oft mit `a` bezeichnet.
Die Funktion `show` gibt eine lesbare Repräsentation eines Objekts zurück.

__Aufgabe:__ Überprüfe mit der Konsole, welchen Datentyp die Funktion `show` hat.

Implementierungen von Datentypen (also auch _Buchstabe_ und _Wort_) werden 
realisiert durch `data <Name> = <Def>`. Zur lesbare Anzeige eines Datentypes in der Konsole z.B.
muss dieser zusätzlich die Funktion `show` implementieren.

__Aufgabe:__ Versuche zu verstehen, was der folgende Code macht. Es geht allein um
die Lesekompetenz von Code.

```haskell
-- | Def: Buchstabe (symbol)
data Symbol = A | B | C deriving(Show)

-- | Def 1: Wort (word)
data Word_1 = EmptyWord | Word_1 Symbol Word_1
instance Show Word_1 where
    show EmptyWord    = "#"
    show (Word_1 s w) = show(s) ++ ":" ++ show(w)
```    

__Aufgabe:__ Kopiere den Code in eine Datei und speichere diese z.B. unter `word.hs` ab. Navigiere zu dem Ordner, wo du
die Datei gespeichert hast, öffne hier die Haskell-Konsole mit `$ ghci` und lade die Datei mit `> :load word.hs`. 
Interagiere mit dem Code, beispielsweise durch
   * `> :type A`
   * `> :type Word_1`
   * `> :type emptyword`
   * `> :type EmptyWord`
   * `> Word_1 A EmptyWord`
   
Erweitere den Code so, dass du deinen Vornamen als Wort ausgeben kannst. Die GHCI-Konsole verlässt du mit `:quit`.

## Listen in Haskell

__Aufgabe:__ Wie ist in Haskell induktiv eine Liste definiert? Warum ist eine Liste und eine Zeichenkette das gleiche?

__Aufgabe:__ Definiere eine Funktion `product`, welche das Produkt aller Zahlen einer Liste berechnet.

__Aufgabe:__ Bestimme anhand der Ausgabewerte für mind. das jeweils gegebene Argument die Bedeutung folgender Funktionen/ Befehle:
   * `1 : []`
   * `[2..4]`
   * `[x^2 | x <- [1, 2, 3]]`
   * `head [1, 2, 3]`
   * `[1, 2, 3] !! 1`
   * `take 2 [1, 2, 3]`
   * `[1, 2, 3] ++ [4, 5, 6]`
   * `tail [1, 2, 3]`
   * `drop 2 [1, 2, 3]`
   * `length [1, 2, 3]`
   * `and [True, False, True]`
