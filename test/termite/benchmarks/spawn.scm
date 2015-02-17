(include "bench.scm")

(define (main n)
  (let ((n (string->number n)))
    (write
     `(spawn
        termite
        ,n
        ,(time*
          (let loop ((n n))
            (spawn (lambda () n))
            (if (> n 0)
                (loop (- n 1)))))))
    (newline)))
