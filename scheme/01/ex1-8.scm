; ex1-8
(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)
    )

; new good-enough(cf 1-7)
(define (good-enough? guess x)
  (if (< (cube guess) x)
      (> (abs (/ (square guess) x)) (- 1 0.1))
      (< (abs (/ (square guess) x)) (+ 1 0.1))
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

(print (cbrt 0.001))


