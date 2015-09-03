;ex2-39
(use slib)
(require 'trace)
(include "util")


; foldr
; [op 1 [op 2 [op 3]]]
(define (reverse-r seq)
  (fold-right (lambda (x y) (append y (list x))) '() seq))

; foldl
; [[[op 1] op 2 ]op 3]
(define (reverse-l seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))


(print (reverse-r '(1 2 3)))
(print (reverse-l '(1 2 3)))
