(define (msg m)
  (print "------")
  (print m)
  (print "------"))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq))
          )))

(define (pick-heads seqs)
  (map (lambda (x) (car x)) seqs))

(define (pick-tails seqs)
  (map (lambda (x) (cdr x)) seqs))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate   op init (pick-heads seqs))
            (accumulate-n op init (pick-tails seqs)))))


