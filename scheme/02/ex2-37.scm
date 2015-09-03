;ex2-37
(use slib)
(require 'trace)
(include "ex2-36")


(define (dot-product v w)
  (accumulate + 0 (map * v w)))
