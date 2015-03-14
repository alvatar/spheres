(define-library (spheres/crypto digest)
  (export close-digest
          open-digest
          digest-u8vector
          digest-sub8vector
          digest-update-subu8vector
          digest-ansi-string
          digest-ansi-substring
          digest-string
          digest-substring
          digest-file
          crc32
          md5
          sha-1
          sha-224
          sha-256)
  (import (spheres/string u8vector))

  (include "digest.scm"))
