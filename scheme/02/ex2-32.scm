;ex2-32
(use slib)
(require 'trace)
(include "util")

(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (ls)
                                      (append (list (car s)) ls))
                              rest)))))

;(trace subsets)
(print (subsets '(1 2 3)))
