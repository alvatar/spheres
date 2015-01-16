(define-library (spheres/math interval)
  (export normalize
          u8-normalize
          denormalize
          u8-denormalize
          map-interval
          map-interval/integers
          invert-interval
          clamp
          clamp-low
          clamp-high)

  (include "interval.scm"))
