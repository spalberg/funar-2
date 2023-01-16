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


