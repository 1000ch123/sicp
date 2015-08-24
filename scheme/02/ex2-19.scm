(use slib)
(require 'trace)

(define no-more? null?)
(define first-denomination car)
(define except-first-denomination cdr)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values)) coin-values)
             ))))

(define coins (list 1 5 10 50 100 500))
(print (cc 100 (list 1 5 10)))
