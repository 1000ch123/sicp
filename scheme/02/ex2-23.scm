(use slib)
(require 'trace)

(define (foreach proc xs)
  (if (null? xs)
    nil
    (and (proc (car xs)) (foreach proc (cdr xs)))))

(foreach (lambda (x) (print x)) (list 1 2 3))
