# Tutorium 11: Main & Files

## Warm-up

__Aufgabe:__ Was war der bisherige Workflow, um Haskell-Code auszuführen?

## Main

* Ziel: klar definierte Abfolge von Schritten mit genau einem Einstieg (_entry point_)
* realisiert durch: _main_ Funktion, eine "I/O-Methode"
* Vorteil: erlaubt Ein- und Ausgabe
* Workflow:
    1. Erstelle Source-Datei
    2. Kompilieren mit `$ ghc Main.hs`
    3. Ausführen der Executable über `$ ./Main` 

> Haskell runtime evaluates main (it's really just an expression). The trick is that main evaluates not to a simple value but to an action. The runtime then executes this action. 
> So the program itself has no side effects, but the action does. And this so called IO action is invoked after the Haskell program has run, so we don't break the functional nature of the program.  [Milewski](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/3-pure-functions-laziness-io)

```haskell
main = do
    -- read data
    -- process data
    -- save results
```

### Ausgabe

```haskell
putStrLn "Hello World"
print 42
```

### Eingabe

```haskell
str <- getLine
src <- readFile "myFileName"
```

## Übung: Körpergröße

__Aufgabe:__ Wir haben folgende Liste von Messwerten zur Körpergröße.
Die erste Spalte identifiziert eindeutig eine Reihe/ gemessene Person über die ersten beiden
Buchstaben von Vor- und Nachname. In der zweiten Spalte steht die Körpergröße in Zentimeter.

```
AnMu,176
BoJo,200
ClDo,181
DaMu,145
ErKa,180
```
Speichere die Werte in einer Datei `heights.csv`.

__Hinweis:__ Zum Lesen/ Schreiben von CSV-Daten kann das Modul [Cassava](https://hackage.haskell.org/package/cassava-0.5.1.0) verwendet werden.
Dies muss u.U. zuerst installiert werden:
1. Update die Liste der Packages: `$ cabal update`
2. Installiere das Modul: `$ cabal install cassava`

Beispiel zur Verwendung:
```
> encode [("John" :: Text, 27 :: Int), ("Jane", 28)]
"John,27\r\nJane,28\r\n"

> decode NoHeader "John,27\r\nJane,28\r\n" :: Either String (Vector (Text, Int))
Right [("John",27),("Jane",28)]
```

__Aufgabe:__ Erstelle ein Modul `MyCsvParser`, welches eine Funktion `parseHeights :: ByteString -> Either String (Vector HeightRecord)` enthält,
d.h. diese Funktion erhält den Output von `readFile` und wandelt diesen in eine Liste von Datensätzen um.
Überlege dir eine geeignete Definition für den (neuen) Datentyp `HeightRecord`.

__Hinweis:__ Um einen CSV-Datensatz eines bestimmten Datentyps zu erstellen, orientiere dich an folgendem Beispiel:
```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Text    (Text)
import GHC.Generics (Generic)

data Person = Person { name :: !Text , salary :: !Int }
    deriving (Generic, Show)

instance FromRecord Person
instance ToRecord Person
```

__Hinweise:__ Die beiden Ausrufezeichen im obigen Hinweis können auch weggelassen werden. 
Zu ihrer Bedeutung siehe [hier](https://stackoverflow.com/questions/993112/what-does-the-exclamation-mark-mean-in-a-haskell-declaration).


__Aufgabe:__ Bestimme die minimale Körpergröße und gebe diese auf der Konsole aus.
Schreibe dafür folgende Funktion, die eine Liste der Körpergrößen zurück gibt:

`justHeightValues :: Either String (Vector HeightRecord) -> Vector Int`



__Hinweis:__ Um den Wert einer Variablen vom Typ `Either` zu erhalten, nutze die Funktion `either`
wie [hier](https://stackoverflow.com/questions/19839042/either-right-left-how-to-read-value) beschrieben.


## Übung: Parser

### Grundlage

__Aufgabe:__ Wähle einen trainierten Parser aus einem der bisherigen Tutorien aus und erstelle dafür eine
Datei mit Testdatensätzen, z.B.

```
Hans mag Rosen.
Maria mag keine Erdbeeren.
Mag Maria Erdbeeren?
```

Lasse dir die Testdaten auf der Konsole anzeigen.

__Aufgabe:__ Lasse dir für jeden Testdatensatz anzeigen, ob er grammatisch ist oder nicht.

### Erweiterung

__Aufgabe:__ Erweitere nun deine Testdatensätze um das erwartete und tatsächliche Grammatikalitätsurteil
(letzteres sei mit `False` initialisiert)
sowie eine ID. Zur Vereinfachung codiere `True` als `1` und `False` als `0`. Ein Satz ist nun z.B.
in der Datei angegeben als `1,Hans mag Rosen,1,0`. Schreibe eine Funktion `parseTestSentences`, welche
analog zu `parseHeights` aufgebaut ist.
Zeige die Testsätze auf der Konsole an.

__Aufgabe:__ Zeige auf der Konsole an, wie viele der Testsätze als grammatisch a priori annotiert sind.

__Hilfestellung:__ `Data.Vector` hat die Funktionen `map` und `toList`, `fromList`. Außerdem könnte folgendes
nützlich sein:
```haskell
expectations :: Either String (Vector TestSentence) -> [Bool]
-- to be implemented

either2Vector :: Either a (Vector b) -> Vector b
either2Vector = either (const $ V.fromList []) id

int2Bool :: Int -> Bool
int2Bool 1 = True
int2Bool _ = False

bool2Int :: Bool -> Int
bool2Int True = 1
bool2Int _    = 0
```

__Aufgabe:__ Es soll nun jeder Testdatensatz mit dem Recognizer bewertet werden und das Ergebnis 
jeder Bewertung im Attribut `actual` abgespeichert werden. Das Ergebnis soll in der Entwicklung
auf der Konsole angezeigt werden. Wenn das korrekt funktioniert, schreibe das Ergebnis in eine
neue Datei `test_sentences_recog.csv`.

__Hinweis:__
```haskell
updateActualFields :: Vector TestSentence -> [Bool] -> Vector TestSentence
updateActualFields records values = records V.// (zip [0,1..] updatedRecords)
    where updatedRecords = [ updateActualField record value | (record, value) <- zip (V.toList records) values]

updateActualField :: TestSentence -> Bool -> TestSentence
updateActualField record value = record {actual = bool2Int value}
```

### Nächste Schritte

__Aufgabe:__ Wenn nun auch die Satzstruktur in der Ausgabedatei abgespeichert werden soll,
an welchen Stellen muss der Programmcode geändert werden?

__Aufgabe:__ Wir würden False Positives und Co gezählt werden?

__Aufgabe:__ Welche Methoden gehören eigentlich nicht in das Modul `MyCsvParser` und
in welches andere Modul könnten sie verlegt werden?



## Nice to know


[Milewski](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/3-pure-functions-laziness-io): Haskell functions are sometimes called pure functions.
Here are the fundamental properties of a pure function:  

1. A function returns exactly the same result every time it's called with the same set of arguments.
In other words a function has no state, nor can it access any external state.
Every time you call it, it behaves like a newborn baby with blank memory and no knowledge of the external world.
2. A function has no side effects. Calling a function once is the same as calling it twice and discarding the result of the first call. In fact if you discard the result of any function call, Haskell will spare itself the trouble and will never call the function. No wonder Haskell has a reputation for laziness (more about it later).

**How can a program built from pure functions do anything useful other than heat up the processor during cold winters in Siberia?**

Now, real programs interact with the outside world. They call functions which do IO, as a side effect, and then return some value. In Haskell, functions with side effects are often called actions, to distinguish them from normal Haskell functions (which behave like mathematical functions: they take inputs and return a result, with no side effects).
