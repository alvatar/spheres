(define-library (spheres/crypto base64)
  (export base64-substring->u8vector
          base64-string->u8vector
          subu8vector->base64-string
          u8vector->base64-string)
  (import (spheres/algorithm u8vector))

  (include "base64.scm"))
