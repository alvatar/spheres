;;!!! Inexact-specific operations
;;! .author Álvaro Castro-Castilla, 2010-2015

(declare (mostly-flonum))


;;! Default accuracy for inexact comparisons
(define equal-accuracy 0.000001)

;;! Is equal?
(define* (~= a b (accuracy: equal-accuracy))
  ;; (< (abs (- a b)) equal-accuracy)) -> Faster in interpreted code
  (and (>= a (- b equal-accuracy))
       (<= a (+ b equal-accuracy))))

;;! Is equal to zero?
(define* (~zero? a (accuracy: equal-accuracy))
  (=~e a 0.0 accuracy))

;;! Decimal part
(define (~decimal-part x)
  (- x (truncate x)))

;;! Average between two values
(define (~average a b)
  (/ (+ a b) 2.0))


;-------------------------------------------------------------------------------
;;!! Random number utilities

;;! A random number from -1.0 to 1.0
(define (random-real-symmetric)
  (fl+ -1.0 (fl* (random-real) 2.0)))

;;! Random real given an interval
(define (random-real/interval a b)
  (let ((ia (exact->inexact a))
        (ib (exact->inexact b)))
    (+ ia (* (random-real) (- ib ia)))))
