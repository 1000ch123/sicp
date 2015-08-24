(use slib)
(require 'trace)

(define (scale-fit items factor)
  (if (null? items)
    nil
    (cons (* (car items) factor)
          (scale-fit (cdr items) factor))))

(print (scale-fit (list 1 2 3 4 5) 10))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(print (map abs (list 1 -2 3 4 -5 6 -7)))
