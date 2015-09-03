;ex2-37
(use slib)
(require 'trace)
(include "util")

(define w '((1 2 3 4) (4 5 6 7) (6 7 8 9)))
(define v '(1 2 3 4))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(print (dot-product v v))

(define (matrix-*-vector m v)
        (map (lambda (v_) (dot-product v_ v)) m))

(print (matrix-*-vector w v))

(define (transpose m)
        (accumulate-n (lambda (x y) (cons x y)) '() m))

(print (transpose w))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
                 (accumulate (lambda (x y) (cons (dot-product v x) y))
                             '()
                             cols))
         m)))

(define (matrix-*-matrix2 m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector n v)) m)))


; 3*4 * 4*3 => 3*3 なので matrix2のほうが正解感ある
(print (matrix-*-matrix w w))
(print (matrix-*-matrix2 w w))
