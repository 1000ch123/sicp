(use slib)
(require 'trace)

(define a (list 1 3 (list 5 7) 9))
(print a)
; car / cdr の連結.後ろから処理されるのね
(print (car (cdaddr a)))

(define b (list (list 7)))
(print b)
(print (caar b))

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(print c)
(print (car (cdr (cadr (cadr (cadr (cadr (cadr c))))))))
