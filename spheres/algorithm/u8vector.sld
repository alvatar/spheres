;;!!! SRFI-66 Octet Vectors + extensions
;; .author Michael Sperber, 2005
;; .author Mikael More, 2008, 2011-2014
;; .author Alvaro Castro-Castilla, 2012-2015

(define-library (spheres/algorithm u8vector)
  (export u8vector-copy!
          u8vector-compare
          u8vector=?
          apply-u8vector-append
          u8vector-reverse
          subu8vector-reverse
          u8vector-invert!
          u8vector-shift!
          u8vector-pad-to-length
          u8vector-xor
          u8vector-xor/byte)

  (include "u8vector.scm"))
