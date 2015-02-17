(include "bench.scm")

(define (iota n)
  (define (i n acc)
    (if (= n 0)
        acc
        (i (- n 1)
           (cons n acc))))
  (i n '()))

(define (nrev lst)
  (if (null? lst)
      lst
      (append (nrev (cdr lst))
              (list (car lst)))))

(define (main n)
  (let* ((n (string->number n))
         (lst (iota n)))
    (write `(nrev termite ,n ,(time* (nrev lst))))
    (newline)))
