;;!!! Intervals
;;. author Alvaro Castro-Castilla, 2012-2015


;;! Normalize value from a range to 0.0-1.0
(define (normalize x low high)
  (/ (- x low) (- high low)))

;;! Map number to 0-255 interval
(define (u8-normalize x low high)
  (inexact->exact
   (round (* 255 (normalize x low high)))))

;;! Map intervals
(define (map-interval x a-low a-high b-low b-high)
  (+ (* (normalize x a-low a-high)
        (- b-high b-low))
     b-low))

;;! Map intervals (to exact integers)
(define (map-interval/integers x a-low a-high b-low b-high)
  (inexact->exact (round (+ (* (normalize x a-low a-high)
                               (- b-high b-low))
                            b-low))))

;;! Takes a value and two boundaries, using any as reference, inverts the intervals
(define (invert-interval x low high)
  (+ low (- high x)))

;;! Clamp value between low and high values
(define (clamp x low high)
  (cond ((< x low) low)
        ((> x high) high)
        (else x)))

;;! Clamp the value if lower than
(define (clamp-low x low)
  (if (< x low) low x))

;;! Clamp the value if higher than
(define (clamp-high x hi)
  (if (> x high) high x))
