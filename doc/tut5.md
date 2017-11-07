# Tutorium 5: Bigramme

## Warum-up
__Aufgabe:__ Erkläre folgenden Ausdruck
```haskell
filter ((==) w . yield) allStructures
```

__Aufgabe:__ Erstelle einen Datentyp `Wochentag`, der die Wochentage, also `Montag`, `Dienstag` etc., enthält.
Folgende Operation soll sinnvoll möglich sein: `min Montag Freitag`. Siehe [hier](http://www.fh-wedel.de/~si/seminare/ws03/Ausarbeitung/2.simple/layout6.htm).

## Bigramme

__Aufgabe:__ Speichere das Modul `Bigrams` aus der Vorlesung. Definiere die Variable `myName` und rufe `makeBounded myName` auf.

__Aufgabe:__ Warum wurde `Bigram` mit dem Key-Term `type` definiert und nicht mit `data` wie z.B. `Tree`?
```haskell
type Bigram a = (Bounds a, Bounds a) 
```

__Aufgabe:__ Als Korpus seien folgende Sätze gegeben: "Hans mag Erdbeeren.", "Hans mag keine Rosen.", "Maria mag Rosen.". Erzeuge eine Grammatik basierend auf den Bigrammen der drei Sätze. Bestünde das Deutsche nur aus diesen sechs Wörtern, wie gut ist unsere Grammatik? Teste dazu die Grammatik mit weiteren Sätzen aus diesem 6-Wort-Deutsch und beurteile somit die Güte der Grammatik als Klassifikator.





## Notes

* Recognition: Ist w in L(G)?
* Parsing: Welche Struktur liegt einem Wort zugrunde?
* Äquivalenz der beiden Fragestellungen
* Parsing-Strategien:
  * Top-Down (Theorie-getrieben)
  * Bottom-uo (Eingabe-getrieben)
* Parsing: Phonetische-Abbildung^-1
* Achtung: Umkehrung einer Funktion oft nicht trivial, s. Primfaktorzerlegung, oder {phi : |= phi} -> länge(phi)
* Baum <=> Struktur
* naiver Ansatz ohne Beschränkungen terminiert nicht, da allStructures unendlich:
```haskell
yield: Structure -> String
grammatical: Structure -> Bool

parse :: String -> [Structure]
parse w = filter grammatical (filter ((==) w yield) allStructures)
```
* Grammatikalität über mögliche Baumbigramme
* parse w = G_w mit G_w Grammatik nur für yield(w)
* Bigramme: VP [laughes], VP[V NP], V[praises], Det[the], N[boy], S[NP VP], NP[John], NP[Det N]

* neuer Ansatz: nicht inkrementell, top-down
* word: (0)-john->(1)-laughs->(2)
* 0S0, 0S1, 1S0, ... 1NP0[John], 0NP1[John], ...
* LÖsche alle Teilbäume mit Terminalzeichen, die die falsche "Stelligkeit" aufweisen, z.B. 
1NP0[John], weil John zwischen 0 und 1 vorkommt, iDetj[the], weil "the" gar nicht im Satz vorkommt (aber 0NP2[0Det1 1N2] ist in der Grammatik).
* bei allen anderen Teilbäumen lösche unsinnige, z.B. 0S10[0NP1 8VP10], 0S5[2NP4 4VP7] (Kohärenzbedingung)
* alle sinnigen Bäume müssen mit S beginnen und die ganze Ketten überdecken
* d.h. nun:
  * $[0S2]
  * 0S2[0NP1 1VP2], 0S2[0NP0 0VP2], ...
  * 0NP1[JOhn]
  * 1VP2[laughs]
* zum Schluss: lösche Zahlen wieder

* Parsing anhand von Schemata (was ist noch zu machen/ wie viel vom Satz ist schon geparst/ was können wir als nächstes tun)
unter Nutzung von Kontrolle (was sollten wir tun) und Datenstrukturen (Speicherung und Zugriff auf Items)

