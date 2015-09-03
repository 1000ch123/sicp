;ex2-36
(use slib)
(require 'trace)
(include "util")


(define (pick-heads seqs)
  (map (lambda (x) (car x)) seqs))

(define (pick-tails seqs)
  (map (lambda (x) (cdr x)) seqs))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate   op init (pick-heads seqs))
            (accumulate-n op init (pick-tails seqs)))))

(define ss (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(print ss)
(print (cdr (car ss)))
(print (pick-heads ss))
(print (pick-tails ss))
(print (pick-heads (pick-tails ss)))
(print (accumulate-n + 0 ss))
