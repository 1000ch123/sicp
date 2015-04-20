; 複合オペレータ
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(print (a-plus-abs-b 10 20))

(print (a-plus-abs-b 10 -20))

(print (a-plus-abs-b 10 0))
