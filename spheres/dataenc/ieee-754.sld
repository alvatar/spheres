;;!!! Byte integer and IEEE floating-point conversions.
;; .author Aubrey Jaffer, 2003
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/dataenc ieee-754)
  (export u8vector->ieee-float
          u8vector->ieee-double
          ieee-float->u8vector
          ieee-double->u8vector
          ieee-byte-collate!
          ieee-byte-decollate!)
  (import (spheres/math bit-logic))

  (include "ieee-754.scm"))
