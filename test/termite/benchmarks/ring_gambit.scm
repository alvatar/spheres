(include "bench.scm")

(define (make-relay next)
  (let loop ()
    (let ((k (thread-receive)))
      (cond
       ((> k 0)
        (thread-send next (- k 1))
        (loop))
       (else
        (thread-send next k))))))

(define (ring n k)
  (let loop ((current (current-thread))
             (n n))
    (cond
     ((> n 1)
      (loop (thread-start!
             (make-thread
              (lambda ()
                (make-relay current))))
            (- n 1)))
     (else
      (thread-send (current-thread) k)
      (make-relay current)))))


(define (main n k)
  (let ((n (string->number n))
        (k (string->number k)))
    (write `(ring gambit (,n ,k) ,(time* (ring n k))))
    (newline)))
