(use slib)
(require 'trace)

(define (msg m)
  (print "------")
  (print m)
  (print "------"))
; cons : pairを返す
(msg "cons : returns pair")
(define p (cons 1 2))
(print p)

; list : listを返す
(msg "list : returns list")
(define l (list 1 2))
(print l)

; pair / list での car / cdr
(msg "car/cdr with pair/list")
(print (car p))
(print (car l))
(print (cdr p))
(print (cdr l))

; aoppend listの結合
(msg "append")
(define a1 (append (list 1) (list 2)))
(define a2 (append (list 1) 2))
(print a1)
(print a2)
;(append 1 (list 2)) これはエラー


(msg "nested cons")
(define n1 (cons (cons 0 1 ) 2))
(define n2 (cons 0 (cons 1 2)))
(print n1)
(print n2)


(msg "car/cdr with nested cons")
(print (car n1))
(print (car n2))
(print (cdr n1))
(print (cdr n2))

(msg "append with const")
(define a3 (append (list 0 1 ) 2))
(print a3)
(print (car a3))
(print (cdr a3))


