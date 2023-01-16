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