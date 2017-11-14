# Tutorium 6: Klassen und Grammatiken

## Warm-up

__Aufgabe:__ Mache dir klar, was der Sinn von `Maybe` ist und verstehe die Implementierung von `famp` 
für diese Klasse: 

```haskell
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing
```

__Aufgabe:__ Wir wollen Cafés testen und erfassen jedes Café in einer wunderschönen Haskell-Variablen vom Typ `Cafe`,
die über _record syntax_ definiert werden soll (siehe unten).
Diese hat folgende Attribute: `name :: String`, `iceCream :: a`, `coffee :: a`, `croissant :: a`. Die letzten drei Attribute
sollen die Bewertung beinhalten, die numerisch oder auch als Text abgegeben werden kann. Definiere den Datentyp `Cafe`
und lege eine Bewertung im Schulnotensystem für das Café _Cafe Albert_ und eine Bewertung in Textformat für das Café
_Cafe Kater_ an. 

__Hinweis:__ Ein Datentyp `Person` kann auf folgende zwei Weisen definiert werden:

```haskell
-- Option 1: bereits bekannt
data Person = Person
  String -- Vorname
  String -- Nachname
  String -- Wohnort
  Int    -- Alter
  
-- Option 2: record syntax, named fields
data Person = Person {
  firstName :: String,
  lastName  :: String,
  city      :: String,
  age       :: Int
}

-- Beispiel-Anwendung:
person = Person {firstName = "Arthur", lastName = "Dent", city = "London", age = 20}

fullName =  firstName person ++ " " ++ lastName person
```

__Aufgabe*:__ Ermögliche eine hübsche Konsolenausgabe für Variablen vom Typ `Cafe`.

__Aufgabe*:__ Die Bewertungen von Cafés sollen verglichen werden. Für diesen Vergleich auf Identität sei der Name des Cafés
nicht relevant. Leite den Datentyp `Cafe` von der Klasse `Eq` ab.

__Aufgabe:__ Es soll nun möglich sein, nach einem zweiten Besuch im jeweiligen Café die Bewertungen zu verändern.
Dafür leite den Datentyp `Cafe` von der Klasse `Functor` ab. Somit ist es nun beispielsweise möglich, die Bewertungen für Eis, Kaffee und
Croissant jeweils um eine Schulnote nach oben zu setzen.

__Aufgabe:__ Was ist der Unterschied zwischen einem _recognizer_ und einem _parser_?

## Intersection-Parsing

Korpus:
 * "Hans mag Erdbeeren."
 * "Hans mag keine Rosen."
 * "Maria mag Rosen."

__Aufgabe:__ Erstelle eine Baum-Bigramm-Grammatik für den Korpus.

__Aufgabe:__ Erstelle diejenige Grammatik, die den Satz "Maria mag Rosen." erzeugt (_intersection grammar_).
Was ist die Struktur dieses Satzes?

__Aufgabe:__ Können wir auch den Satz "Mag Maria Rosen?" parsen?


## Hilfen zum Code
```
> partition (>3) [1,5,2,4,3]
([5,4],[1,2,3])

> (roots,others) = partition (isRoot . fst) grammar
``` 



