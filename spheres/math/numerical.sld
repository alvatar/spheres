;;!!! Numerical methods
;; .author Aubrey Jaffer, 1996-2005
;; .author Lars Arvestad
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/math numerical)
  (export find-integer-root/newton
          find-root/newton
          find-root/laguerre
          find-polynomial-root/laguerre
          find-root-1/secant
          find-root/secant
          find-bracketed-root/secant
          sequence->limit
          limit
          golden-section-search)

  (include "numerical.scm"))
