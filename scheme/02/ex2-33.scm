;ex2-33
(use slib)
(require 'trace)
(include "util")

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(print (accumulate + 0 (list 1 2 3 4)))

(define (my-map p seq)
        (accumulate (lambda (x y)
                            (append (list (p y)) x))
                            nil seq))

(print (my-map (lambda (x) (* 3 x)) (list 1 2)))
