;ex2-38
(use slib)
(require 'trace)
(include "util")

(define (my-fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(print (fold-right / 1 '(1 2 3)))
(print (fold-left / 1 '(1 2 3)))
(print (my-fold-left / 1 '(1 2 3)))

; op が foldr / foldl で同一挙動になるopの条件は？
; op a b = op b a が成立するかどうか
; すなわち交換法則が成り立つかどうかじゃないですかね
