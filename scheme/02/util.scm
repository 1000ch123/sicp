(define (msg m)
  (print "------")
  (print m)
  (print "------"))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq))
          )))


