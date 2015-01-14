(define-library (spheres/crypto hmac)
  (export hmac-u8vector
          hmac-subu8vector
          hmac-ansi-string
          hmac-ansi-substring
          hmac-string
          hmac-substring
          hmac-crc32
          hmac-md5
          hmac-sha-1
          hmac-sha-224
          hmac-sha-256)
  (import (spheres/algorithm u8vector)
          (spheres/string u8)
          (spheres/crypto digest))

  (include "hmac.scm"))
