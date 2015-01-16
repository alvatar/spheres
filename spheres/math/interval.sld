(define-library (spheres/math interval)
  (export normalize
          u8-normalize
          map-interval
          map-interval/integers
          invert-interval
          clamp
          clamp-low
          clamp-high)

  (include "interval.scm"))
