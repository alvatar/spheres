;;!!! Base32 encoding

(define-library (spheres/crypto base32)
  (export base32-substring->u8vector
          base32-string->u8vector
          subu8vector->base32-string
          u8vector->base32-string)
  (import (spheres/algorithm u8vector))

  (include "base32.scm"))
