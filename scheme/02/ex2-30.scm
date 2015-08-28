;ex2-30
(use slib)
(require 'trace)
(include "util")


(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(print (square-tree (list 1 2 (list 3 4) (list 5))))

