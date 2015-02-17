(include "bench.scm")

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (main n)
  (let ((n (string->number n)))
    (write `(fib termite ,n ,(time* (fib n))))
    (newline)))
