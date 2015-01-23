(define-library (spheres/crypto rsa)
  (export make-rsa-key-pair
          public-rsa-key
          private-rsa-key
          rsa-key=
          rsa-key->list
          list->rsa-key
          PKCS1-pad
          PKCS1-unpad
          rsa-crypt
          rsa-encrypt-u8vector
          rsa-decrypt-u8vector
          get-u8vector-password
          make-salt
          PBKDF1
          PBKDF2)
  (import (spheres/algorithm u8vector)
          (spheres/codec bignum)
          (spheres/codec base64)
          (spheres/crypto digest)
          (spheres/string u8))
  
  (include "rsa.scm"))
