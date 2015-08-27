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

(print m)
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
         (total-weight (branch-structure l))
         )
     (if (leaf? r)
         (branch-structure r)
         (total-weight (branch-structure r))
         )
     )))

(define m2
  (let ((mm make-mobile) (mb make-branch))
       (mm (mb 10 m) (mb 20 m)))
  )

(define m3
  (let ((mm make-mobile) (mb make-branch))
       (mm (mb 30 m2) (mb 20 m)))
  )


;(trace total-weight)
(print m2)
(print (left-branch m2))
(print (branch-structure (left-branch m2)))
(print (left-branch (branch-structure (left-branch m2))))
(print (pair? (branch-structure (left-branch m2))))
(print (not (pair? (branch-structure (left-branch m2)))))
(print (total-weight m2))


(msg "c: write balanced?")

(define (weight branch)
  (if (leaf? branch)
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(print "--weight--")
(print (left-branch m2))
(print (weight (left-branch m2)))
(print (weight (right-branch m2)))
(print (total-weight m2))

(define (torque branch)
  (* (weight branch) (branch-length branch)))

(print "--torque--")
(print (torque (left-branch m)))
(print (torque (right-branch m)))


(define (partial-balanced? mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))

(print "--partial balance--")
(define mb
  (make-mobile
    (make-branch 2 100)
    (make-branch 1 200)))
(print (partial-balanced? mb))
(print (partial-balanced? m))


(define (balanced? mobile)
  #t)

;(print (balanced? mm))
