;;!!! Arithmetic
;; .author Álvaro Castro-Castilla, 2012-2015. See LICENSE file.


;;------------------------------------------------------------------------------
;;!! Constants

(define pi (angle -inf.0))
(define pi2 (fl* #i2 pi))
(define pi/2 (fl/ pi #i2))
(define -pi/2 (fl/ pi #i-2))
(define pi/4 (fl/ pi #i4))
(define -pi/4 (fl/ pi #i-4))
(define pi3/4 (fl* #i3 (fl/ #i4 pi)))

(define e #i2.71828182845904523536)
(define log2e #i1.44269504088896340736)
(define log10e #i0.434294481903251827651)
(define ln2 #i0.693147180559945309417)
(define ln10 #i2.30258509299404568402)
(define sqrt2 #i1.41421356237309504880)
(define sqrt1/2 #i0.707106781186547524401)

(define euler #i0.5772156649)


;;------------------------------------------------------------------------------
;;!! Aritmethic functions

;;! Logarithm in base N
(define logN (lambda (x N) (/ (log x) (log N))))

;;! Sum of all values
(define (sum l) (apply + l))

;;! Product of all values
(define (product l) (apply * l))

;;! Inverse function
(define inverse ##inverse)

;;! Square
(define (square x) (* x x))

;;! Fast factorial algorithm
;; ported from http://www.cs.berkeley.edu/~fateman/papers/factorial.pdf
;; split recursive, based on P. Luschny's code
(define (factorial n)
  (let ((p 1)
        (r 1)
        (NN 1)
        (log2n (inexact->exact (floor (logN n 2))))
        (h 0)
        (shift 0)
        (high 1)
        (len 0))
    (letrec ((prod (lambda (n)
                     (declare (fixnum n))
                     (let ((m (arithmetic-shift n -1)))
                       (cond ((= m 0)
                              (set! NN (+ NN 2))
                              NN)
                             ((= n 2)
                              (let ((NN1 (+ NN 2))
                                    (NN2 (+ NN 4)))
                                (set! NN NN2)
                                (* NN1 NN2)))
                             (else
                              (* (prod (- n m)) (prod m))))))))
      (let loop ()
        (if (not (= h n))
            (begin
              (set! shift (+ shift h))
              (set! h (arithmetic-shift n (- log2n)))
              (set! log2n (- log2n 1))
              (set! len high)
              (set! high (if (odd? h) h (- h 1)))
              (set! len (arithmetic-shift (- high len) -1))
              (cond ((> len 0)
                     (set! p (* p (prod len)))
                     (set! r (* r p))))
              (loop))))
      (arithmetic-shift r shift))))
