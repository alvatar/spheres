(include "bench.scm")

(define (main n)
  (let ((n (string->number n)))
    (write
     `(self
       gambit
       ,n
       ,(time*
         (let loop ((n n))
           (thread-send (current-thread) n)
           (thread-receive)
           (if (> n 0)
               (loop (- n 1)))))))
    (newline)))
