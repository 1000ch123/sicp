(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(print (new-if (= 2 3) 0 5))
(print (new-if (= 2 2) 0 5))
(print (new-if (= 2 2) 0 (print 10))) ;print 10が先に評価されてる
(print (if (= 2 2) 0 (print 10))) ;print 10が先に評価されてる


(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
  )

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(print (sqrt 2))
