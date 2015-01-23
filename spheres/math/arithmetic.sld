;;!!! Arithmetic
;; .author Álvaro Castro-Castilla, 2012-2015. See LICENSE file.

(define-library (spheres/math arithmetic)
  (export pi
          pi2
          pi/2
          -pi/2
          pi/4
          -pi/4
          pi3/4
          e
          log2e
          log10e
          ln2
          ln10
          sqrt2
          sqrt1/2
          euler
          ;; Arithmetic functions
          logN
          sum
          product
          inverse
          square
          factorial)

  (include "arithmetic.scm"))
