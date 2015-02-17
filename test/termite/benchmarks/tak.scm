(include "bench.scm")

(define (tak x y z)
  (if (<= x y)
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (main x y z)
  (let ((x (string->number x))
        (y (string->number y))
        (z (string->number z)))
    (write `(tak termite (,x ,y ,z) ,(time* (tak x y z))))
    (newline)))
