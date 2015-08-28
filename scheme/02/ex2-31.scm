;ex2-31
(use slib)
(require 'trace)
(include "util")

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc tree)
             (proc tree)))
       tree))

