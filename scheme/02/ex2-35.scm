;ex2-35
(use slib)
(require 'trace)
(include "util")

; これenumrate-tree使って良いかどうかで難易度激変だな
; まぁダメだよな
; いやよさげだ
; テキストのenumerate-tree間違ってる..cdrのところcadrにしないと上手く動かんよ
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cadr tree))))
        ))

(define (count-leaves t)
  (accumulate + 0 (map (lambda x 1) (enumerate-tree t))))

(define mytree (list 1 (list 2 (list 3 4)) 5))
(print mytree)
(print (enumerate-tree mytree))
(print (count-leaves mytree))
