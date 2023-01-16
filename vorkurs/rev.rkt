#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       #;(cons (rev (rest list)) (first list))
       (cons (first list) (rev (rest list)))))))

; Element an Liste _hinten_ anhängen
(: add-element ((list-of %a) %a -> (list-of %a)))