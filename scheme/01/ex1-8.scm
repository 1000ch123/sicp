; ex1-8
(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)
    )

; new good-enough(cf 1-7)
(define (good-enough? guess x)
  (if (< (cube guess) x)
      (> (abs (/ (cube guess) x)) (- 1 0.1))
      (< (abs (/ (cube guess) x)) (+ 1 0.1))
  ))

(define (cube x)
  (* x (* x x))
  )

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
        guess
        (cbrt-iter (improve guess x) x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))


(use slib)
(require 'trace)
(trace cbrt-iter)
(print (cbrt 0.00001))


