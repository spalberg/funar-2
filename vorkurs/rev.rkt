#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))
(check-expect (rev empty)
              empty)

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       #;(cons (rev (rest list)) (first list))
       #;(cons (first list) (rev (rest list)))
       (add-element (rev (rest list))
                    (first list))))))

#;(define rev
  (lambda (list)
    (rev* list empty)))

; 1 + 2 + 3 + 4 + ... + (n-1) + n -> n*(n+1)/2 -> O(n^2)

; Element an Liste _hinten_ anhängen
(: add-element ((list-of %a) %a -> (list-of %a)))

(check-expect (add-element empty 3)
              (list 3))
(check-expect (add-element (list 2 3) 4)
              (list 2 3 4))
  
(define add-element
  (lambda (xs element)
    (if (empty? xs)
        (list element)
        (cons
         (first xs)
         (add-element (rest xs) element)))))

#;(add-element (list 2 3) 4)
#;(rev (list 2 3 4))

(define rev*
  (lambda (list acc) ; Akkumulator ->
    ; Liste der gesehenen Elemente (umgedreht)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev* (rest list)
             (cons (first list)
                   acc))))))

(check-expect (rev* (list 1 2 3 4) empty)
              (list 4 3 2 1))
(check-expect (rev* empty empty)
              empty)

(rev* (list 1 2 3 4) empty)

; Übung:
; - Elemente einer Liste summieren -> mit Akkumulator
; - extract endrekursiv
(: list-sum ((list-of number) -> number))

(check-expect (list-sum (list 1 2 3 4))
              10)