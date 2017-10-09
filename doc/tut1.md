# Tutorium 1: Einführung

## Vorbereitung: Installation von Haskell
* ganz ohne Installation [hier](http://tryhaskell.org/) Haskell austesten
* oder per [Anleitung](https://docs.haskellstack.org/en/stable/README/) installieren
* und dann die ersten [Hello World!](https://wiki.haskell.org/Haskell_in_5_steps)-Programme schreiben
* zum Programmieren empfiehlt sich ein Editor mit Syntaxhervorhebung für Haskell bzw. eine Entwicklungsumgebung (mit Haskell-Plugin)

## Warm-up
Definiere (informell) folgende Begriffe:
1. Menge, Teilmenge
2. Funktion
3. Definitionsbereich, Wertebereich
4. Stelligkeit einer Funktion
5. polnische Notation
6. Prädikat (Logik)

Welche Datentypen kennst du von Programmiersprachen?

## Buchstaben und Wörter
Definition _Alphabet_:
> Ein Alphabet Σ ist eine endliche Menge von Buchstaben.

Definition _Wort_:
> Ein Wort _w_ ist i) das leere Wort #, oder ii) ein Ausdruck σ:_v_ mit σ ∈ Σ und Wort _v_.
Nichts anderes ist ein Wort.

__Aufgabe:__ Wie wurde 'Verkettung' und 'Länge' definiert? Fasse beides als Funktion auf:
 Von welcher Stelligkeit sind sie und was ist ihr Definitions- und Wertebereich?
 
 __Aufgabe:__ In Anlehnung an [reguläre Ausdrücke](https://de.wikipedia.org/wiki/Regul%C3%A4rer_Ausdruck)
 soll ein Wort als eine Abfolge von Buchstaben aus dem Alphabet verstanden werden, die von '^' und '$' 
 eingeschlossen sind. Ein Wort ist somit _^foobar$_, aber nicht _$foobar^_. Gib eine zu oben analoge
 Definition für 'Wort_regexp' an.
 
 __Aufgabe:__ Sei Σ = {a, n, o, t}. Sind folgende Ausdrücke Wörter nach der Defintion von 'Wort' aus der Vorlesung?
  * a:n:n:a:#
  * o:t:t:o
  * a:n:n:a:#:o:t:t:o:#
  * t:o:n:i:#
  
## Monoid (*)
  
Definition [_Monoid_](https://de.wikipedia.org/wiki/Monoid):
> Ein Monoid ist ein Tripel (_M_, *, _e_) bestehend aus einer Menge _M_, einer zweistelligen Verknüpfung *
> und einem ausgezeichneten Element e ∈ M mit folgenden Eigenschaften:
> * Verknüpfung \* ist eine zweistellige Operation: \* : M × M → M, (a,b) ↦ a * b
> * Assoziativität der Verknüpfung *
> * _e_ ist neutrales Element

Es ist nun zu zeigen, dass bei geeigneter Wahl von _e_ dieses zusammen mit der Menge der Wörter und der Verkettung ein Monoid bildet.

__Aufgabe:__ Ist (ℕ, +, 0) ein Monoid?


## Haskell

* [funktionale Programmiersprache](https://de.wikipedia.org/wiki/Funktionale_Programmierung)
* basiert auf dem [Lambda-Kalkül](https://de.wikipedia.org/wiki/Lambda-Kalk%C3%BCl)
* stark [typisiert](https://de.wikipedia.org/wiki/Typisierung_(Informatik))
* nutzt [lazy evaluation](https://de.wikipedia.org/wiki/Lazy_Evaluation)

### Grundlegendes

Die Konsole wird mit `ghci` (_The Glorious Glasgow Haskell Compilation System_) gestartet und mit `:q` (`:quit`) beendet.
Funktionen einer Datei _my_file.hs_ können über `:l my_file.hs` in der Konsole zur Verfügung gestellt werden. Bei Änderungen in 
der Datei muss diese mit `:r` neu geladen werden.

### Programmieren von Verkettung und Länge

In Haskell wird die Verkettung von Wörtern über `++` ausgedrückt.
Die Länge eines Wortes kann über `length` berechnet werden.
Ein Wort wird als Abfolge von Buchstaben innerhalb von Anführungszeichen eingegeben.

__Aufgabe:__ Schreibe eine Funktion `meineLaenge`, welche die Definition aus der Vorlesung zur Länge eines Wortes implementiert.


