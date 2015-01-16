(define-library (spheres/math arithmetic-inexact)
  (export equal-accuracy
          ~=
          ~zero?
          ~decimal-part
          ~average
          random-real-symmetric
          random-real/interval)

  (include "arithmetic-inexact.scm"))
