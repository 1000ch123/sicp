;ex2-29
(use slib)
(require 'trace)
(include "util")


; left,right: branch
(define (make-mobile left right)
  (list left right))

; len: length of branch
; structure; weight or mobile
(define (make-branch len structure)
  (list len structure))

(msg "a: write selectors")
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define m
  (make-mobile
    (make-branch 5 100)
    (make-branch 2 200)))

(print (left-branch m))
(print (right-branch m))
(print (branch-length (left-branch m)))
(print (branch-structure (left-branch m)))

(msg "b: write total-weight")

(define (leaf? branch)
    (not (pair? (branch-structure branch))))

(define (total-weight mobile)
  (let ((l (left-branch mobile))
        (r (right-branch mobile)))
  (+ (if (leaf? l)
         (branch-structure l)
         (total-weight l)
         )
     (if (leaf? r)
         (branch-structure r)
         (total-weight r)
         )
     )))

(define mm
  (let ((mk make-mobile))
       (mk (mk m m) (mk m (mk m m))))
  )
(print mm )
(print (total-weight mm ))


