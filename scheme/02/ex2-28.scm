;ex2-28
(use slib)
(require 'trace)

; a, b ともにsingle，lsitどちらもありうる
(define (join a b)
  (cond ((null? b) (list a))
        ((pair? a) (if (pair? (car b))
                       (append a (car b))
                       (append a b)))
        (else (if (pair? (car b))
                  (append (list a) (car b))
                  (append (list a) b)
                  ))
        ))

(define (fringe tree)
  (cond ((null? tree) tree)
        ((pair? (car tree)) (join (fringe (car tree)) (fringe (cdr tree))))
        (else (join (car tree) (fringe (cdr tree))))
         ))


(trace fringe)
(define x (list (list 1 2) (list 3 4)))
(print x)
(print (fringe x))
