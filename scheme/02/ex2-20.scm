(use slib)
(require 'trace)

(define (same-parity x0 . xs)
  (cons x0 (step x0 xs)))

; 差を２で割ったあまりでパリティチェック
(define (same a b)
  (= (remainder (- a b) 2) 0))

; stepに対しては . xs が「ひとつのリスト」として渡されるっぽい
(define (step x0 xs)
  (cond ((null? xs) '())
        ((same x0 (car xs)) (cons (car xs) (step x0 (cdr xs))))
        (else (step x0 (cdr xs)))
        ))

(print (same-parity 1 2 3 4 5 6 7 8 9))
