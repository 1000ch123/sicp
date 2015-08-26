(use slib)
(require 'trace)

(define (reverse xs)
  (if (null? xs)
    xs
    (append (reverse (cdr xs)) (car xs))))

(print (reverse (list 1 2 3 4)))
