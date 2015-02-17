(include "bench.scm")

(define (make-relay next)
  (let loop ()
    (let ((k (?)))
      (cond
       ((> k 0)
        (! next (- k 1))
        (loop))
       (else
        (! next k))))))

(define (ring n k)
  (let loop ((current (self))
             (n n))
    (cond
     ((> n 1)
      (loop (spawn 
              (lambda ()
                (make-relay current)))
            (- n 1)))
     (else
      (! (self) k)
      (make-relay current)))))

(define (main n k)
  (let ((n (string->number n))
        (k (string->number k)))
    (write `(ring termite (,n ,k) ,(time* (ring n k))))
    (newline)
    (? 1 'ok)))
