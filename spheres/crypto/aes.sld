(define-library (spheres/crypto aes)
  (export u8vector->aes-context
          aes-encrypt-ecb
          aes-decrypt-ecb
          aes-encrypt-cbc
          aes-decrypt-cbc
          aes-encrypt-subu8vector
          aes-decrypt-subu8vector
          aes-encrypt-u8vector
          aes-decrypt-u8vector)

  (include "aes.scm"))
