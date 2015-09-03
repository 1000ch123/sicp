;ex2-34
(use slib)
(require 'trace)
(include "util")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(print (horner-eval 2 (list 1 0 3)))
