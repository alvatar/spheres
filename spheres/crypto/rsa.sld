;;!!! Provides procedures to encrypt, decrypt, sign and verify messages
;; using the RSA public-key cryptosystem.
;; .author Marc Feeley, 2006-2007
;; .author Alvaro Castro-Castilla
;; .license: lgpl/v2.1
;;
;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

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
          (spheres/string u8vector)
          (spheres/dataenc bignum)
          (spheres/dataenc base64)
          (spheres/crypto digest))

  (include "rsa.scm"))
