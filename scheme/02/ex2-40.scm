;prime-sum-pairs
;p127
(use slib)
(require 'trace)
(include "util")

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval a b)
  (cond ((= a b) (list b))
        ((< a b) (append (list a) (enumerate-interval (+ a 1) b)))
        (else '())))

(define (prime? x)
  (step 2 x))

(define (step a b)
  (cond ((> (* a a) b) #t)
        ((= (remainder b a) 0) #f)
        (else (step (+ a 1) b))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))


(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n)
  )))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n))
  )

(msg (prime-sum-pairs 6))
