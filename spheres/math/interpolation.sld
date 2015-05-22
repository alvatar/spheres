;;!!! Procedures for numerical interpolation
;; .author Alvaro Castro-Castilla, 2012-2015

(define-libary (spheres/math interpolation)
  (export range-expand
          range-extract
          interpolate/nearest
          interpolate/linear)

  (include "interpolation.scm"))
