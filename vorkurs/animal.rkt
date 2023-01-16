#lang deinprogramm/sdp/beginner

; Datenanalyse
; Datendefinition -> wir beschreiben die Dinge natürlichsprachlich

; Haustier
; - Katze
; - Hund
; - Schlange

; besser: Haustier _ist eins der Folgenden_:
; - Katze ODER
; - Hund ODER
; - Schlange
(define pet
  (signature (enum "cat" "dog" "snake")))

; Konstruktionsanleitung
#;(define cute? ; sprich: cute-p
    (lambda (pet)
      ...))

; Schablone:
; bei Daten, die Fallunterscheidung sind: Fälle unterschiedlich behandeln
(define cute? ; sprich: cute-p
  (lambda (pet)
    (cond
      ((string=? pet "cat") #t) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "dog") #t)
      ((string=? pet "snake") #f))))

(check-expect (cute? "snake")
              #f)
(check-expect (cute? "cat")
              #t)
(check-expect (cute? "dog")
              #t)

; Digitaluhr: Uhrzeit:
; - Stunde UND
; - Minute
; UND -> zusammengesetzte Daten

(define-record time ; <- Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektoren
  (time-minute natural))
; natural : Signatur für natürliche Zahlen

#;(define make-time-from-msm
    (lambda (msm)
      ...))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

(define time1 (make-time 12 23))
(define time2 (make-time 15 11))

; Minuten seit Mitternacht ausrechnen
(: msm (time -> natural))

(check-expect (msm time1) 743)
(check-expect (msm time2) 911)

; Konstruktionsanleitung
#;(define msm
    (lambda (time)
      ...))

; Schablone -> zusammengesetzte Daten -> müssen alle Bestandteile anschauen
(define msm
  (λ (time) ; <- Lambda-Symbol kann via Menü eingefügt werden (Ctrl-\)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Übung: Minuten seit Mitternacht rein -> Record raus

; hilfreich: quotient, remainder
#;(check-expect (msm->time 5)
                (make-time 0 5))

;; texanischer Highway

; Gürteltier hat folgende Eigenschaften:
; - Gewicht
; - lebendig oder tot

(define-record dillo
  make-dillo
  dillo?
  (dillo-alive? boolean)
  (dillo-weight number))

; Beispiele:
; lebendiges GT, Gewicht 10
(define dillo1 (make-dillo #t 10))
; totes GT, Gewicht 8
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo natural -> dillo))

(check-expect (feed-dillo dillo1 3)
              (make-dillo #t 13))
(check-expect (feed-dillo dillo2 3)
              dillo2)

#;(define feed-dillo
    (lambda (dillo amount)
      (cond
        ((dillo-alive? dillo)
         (make-dillo #t (+ amount
                           (dillo-weight dillo))))
        (else dillo))))

; if -> cond mit genau zwei Fällen: (if <pred> <then> <else>)
(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo)) ; lokale "Variable"
    (if alive?
        (make-dillo #t (+ amount
                          (dillo-weight dillo)))
        dillo)))

; gibt auch noch andere Tiere auf dem Highway

; Papagei hat folgende Eigenschaften
; - Gewicht
; - Satz, den er sagen kann

(define-record parrot ; <- Signatur
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight natural))

(define parrot1 (make-parrot "Hallo" 1))
(define parrot2 (make-parrot "Tschüss" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))
(check-expect (run-over-parrot parrot2)
              (make-parrot "" 2))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Tier ist eins der Folgenden
; (gemischte Daten)
; - Papagei
; - Gürteltier
(define animal
  (signature (mixed dillo parrot)))

; Tiere überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

#|
interface Animal { Animal runOver()}; Animal feed(double amount); }
class Dillo implements Animal { Animal runOver() { ...}}
class Parrot implements Animal { Animal runOver() { ...}}

neue Sorte ->
class Snake implements Animal { Animal runOver() { ...}}

Kehrseite: -> feed(double amount) hinzufügen
           -> alles anpassen

FP vs. OOP:
- in FP billig, neue Funktionen zu definieren
- in OOP billig, neue Klassen zu definieren

Schön wäre: beides billig (expression problem -> Phil Wadler)
|#




