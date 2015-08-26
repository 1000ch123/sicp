(use slib)
(require 'trace)

(define (deepReverse xs)
  (cond ((null? xs) xs)
        ((pair? (car xs))
          (append
            (deepReverse (cdr xs)) (list (deepReverse (car xs)))))
        (else
          (append
            (deepReverse (cdr xs))
            (list (car xs))))
        )
  )
(trace deepReverse)

(define x (list (list 1 2) (list 3 4)))
(print x)
(print (car x))
(print (cdr x))
(print (list? x))
(print (pair? x))
(print (deepReverse x))

(print "-----------")
; えーこの挙動トラップじゃねー？
(print (append (list 0) 2))
(print (append (list 0) (list 2)))
(print (cdr (append (list 0) 2)))
(print (cdr (append (list 0) (list 2))))

(define input (list (list 1 2) (list 3 5)))
(print input)
