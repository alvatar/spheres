(include "bench.scm")

(define (main n)
  (let ((n (string->number n)))
    (write
     `(self
       termite
       ,n
       ,(time*
         (let loop ((n n))
           (! (self) n)
           (?)
           (if (> n 0)
               (loop (- n 1)))))))
    (newline)))
