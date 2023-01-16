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




; Listen von Zahlen
; Datendefinition:
; Liste ist eins der Folgenden:
; - die leere Liste
; - eine Cons-Liste aus erstem Element und Rest-Liste

#;(define list-of-numbers
    (signature (mixed empty-list cons-list-of-numbers)))

(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

(define-record empty-list
  make-empty
  empty?)

(define empty (make-empty))

; Eine cons-Liste hat folgende Eigenschaften:
; - erstes Element
; - Rest-Liste
(define-record (cons-list-of element)
  cons
  cons?
  (first element) ; war: number
  (rest (list-of element)))

; einelementige Liste
(define list1 (cons 5 empty))

; zweielementige Liste
(define list2 (cons 2 (cons 5 empty)))

; dreielementige Liste: 7 2 5
(define list3 (cons 7 (cons 2 (cons 5 empty))))

; vierelementige Liste: 6 7 2 5
(define list4 (cons 6 list3))

(define list-of-numbers (list-of number))

; Elemente einer Liste aufsummieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list1)
              5)
(check-expect (list-sum list2)
              7)
(check-expect (list-sum empty)
              0)

; Schablone
(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; neutrales Element der Addition
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Elemente einer Liste multiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list1)
              5)
(check-expect (list-product list2)
              10)
(check-expect (list-product empty)
              1)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; neutrales Element der Multiplikation
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; gerade Elemente aus Eingabeliste
(: extract-evens (list-of-numbers -> list-of-numbers))

(check-expect (extract-evens list1)
              empty)
(check-expect (extract-evens list2)
              (cons 2 empty))
(check-expect (extract-evens (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
              (cons 2 (cons 4 empty)))

; Malte
#;(define extract-evens
    (lambda (list)
      (cond
        ((empty? list) empty)
        ((and (cons? list)
              (= (remainder (first list)
                            2)
                 1))
         (extract-evens (rest list)))
        (else
         (cons (first list) (extract-evens (rest list)))))))

#;(define even?
    (lambda (n)
      (= (remainder n 2)
         0)))

(define extract-evens
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define f (first list))
       (define r (rest list))
       (if (even? f)
           (cons f
                 (extract-evens r))
           (extract-evens r))))))
; ungerade Elemente aus Eingabeliste
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list1)
              list1)
(check-expect (extract-odds list2)
              (cons 5 empty))
(check-expect (extract-odds (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
              (cons 1 (cons 3 empty)))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define f (first list))
       (define r (rest list))
       (if (odd? f)
           (cons f
                 (extract-odds r))
           (extract-odds r))))))

; Alle Elemente extrahieren, die ein Kriterium erfüllen
; Higher-Order-Funktionen
; Typvariable -> %element
(: extract ((%element -> boolean) (list-of %element) -> (list-of %element)))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define f (first list))
       (define r (rest list))
       (if (p? f)
           (cons f
                 (extract p? r))
           (extract p? r))))))

(check-expect (extract (lambda (s) (string=? s "abc"))
                       (cons "abc" (cons "foo" empty)))
              (cons "abc" empty))


(define highway
  (cons dillo1
        (cons parrot1
              (cons dillo2
                    (cons parrot2 empty)))))

; alle Tiere überfahren
(: run-over-animals ((list-of animal) -> (list-of animal)))

(check-expect (run-over-animals highway)
              (cons (run-over-animal dillo1)
                    (cons (run-over-animal parrot1)
                          (cons (run-over-animal dillo2)
                                (cons (run-over-animal parrot2)
                                      empty)))))

(define run-over-animals
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (run-over-animal (first list))
             (run-over-animals (rest list)))))))

; Übung: 1. alle Zahlen in einer Liste verdoppeln
;        2. Abstraktion -> ??? (siehe extract)
(: double (number -> number))
(define double
  (lambda (x)
    (* 2 x)))

; Übung: Abstraktion für list-sum und list-product?
; vielleicht erst mal für Zahlen
; anschließend: geht das auch allgemeiner?
; Name?
; signatur hier

(define list-fold
  (lambda (neutral op list)
    (if (empty? list)
        neutral
        (op (first list)
            (list-fold neutral op (rest list))))))

(check-expect (list-fold 1 * list1)
              5)
(check-expect (list-fold 0 + list2)
              7)

; list-map aus Übung
; muss nicht überall %a sein -> verallgemeinerte Version:
(: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))

(define list-map
  (lambda (op list)
    (if (empty? list)
        empty
        (cons (op (first list))
              (list-map op (rest list))))))

(check-expect (list-map double list2)
              (cons 4 (cons 10 empty)))