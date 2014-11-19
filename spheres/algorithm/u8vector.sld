(define-library (spheres/algorithm u8vector)
  (export u8vector-copy!
          u8vector-compare
          u8vector=?
          apply-u8vector-append
          u8vector-reverse
          u8vector-invert!
          u8vector-pad-to-length
          u8vector-xor
          u8vector-xor/byte)
  
  (include "u8vector.scm"))
