(declare (block))

(include "bench.scm")

(define (run n)
  (let ((this (self)))
    (spawn
      (lambda ()
        (let loop ((n n))
          (if (> n 0)
              (begin
                (if (even? n)
                    (migrate-task node2)
                    (migrate-task node1)))    
              (begin
                (! this 'done)
                (shutdown!)))
          (loop (- n 1))))))
  (?))

(define (main n)
  (cond
   ((equal? (current-node) node1)
    ;; code for node 1
    (write `(migrate
             termite
             ,n
             ,(time* (run n))))
    (newline)
    (remote-spawn node2 (lambda () (exit)))
    (? 1 'done))

   ;; code for node2
   (else (?))))
