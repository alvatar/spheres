#!/usr/local/Gambit/bin/gsi -:dar1

(include "bench.scm")

(define (main n)
  (let ((n (string->number n)))
    (write
     `(spawn
        gambit
        ,n
        ,(time*
          (let loop ((n n))
            (thread-start! (make-thread (lambda () n)))
            (if (> n 0)
                (loop (- n 1)))))))
    (newline)))
