;;!!! Base64 encoding
;; .author Marc Feeley. Copyright (c) 2005-2007 by Marc Feeley, All Rights Reserved.
;; .author Álvaro Castor-Castilla, 2014-2015. Minor modifications and compatibility.

(define-library (spheres/crypto base64)
  (export base64-substring->u8vector
          base64-string->u8vector
          subu8vector->base64-string
          u8vector->base64-string)
  (import (spheres/structure fifo))
  (import (spheres/algorithm u8vector))

  (include "base64.scm"))
