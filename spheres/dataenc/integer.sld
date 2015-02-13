;;!!! Byte integer conversions
;; .author Aubrey Jaffer, 2003
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/dataenc integer)
  (export u8vector->integer
          integer->u8vector
          integer-byte-collate!)
  
  (import (spheres/math bit-logic))

  (include "integer.scm"))
