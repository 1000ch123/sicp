; ex1-7

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

; 元々のgood-enough
;(define (good-enough? guess x)
  ;(< (abs (- (square guess) x)) 0.001)
  ;)

; new good-enough
(define (good-enough? guess x)
  (if (< (square guess) x)
      (> (abs (/ (square guess) x)) (- 1 0.001))
      (< (abs (/ (square guess) x)) (+ 1 0.001))
  ))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(print (sqrt 0.0001))

;(print (sqrt 100000000000000000000))

; sqrtがうまく機能しない例を挙げよ

; 小さい数
; good-enough?で判断しているのは目標値と推定値二乗の差(thres)．
; thresより小さい数の平方根は差が取りづらいので失敗

; 大きい数


