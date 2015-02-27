;;!!! Provides procedures to read and write encrypted files.
;; .author Marc Feeley, 2006-2007
;; .author Alvaro Castro-Castilla, 2015
;; .license lgpl/v2.1
;;
;; Copyright (C) 2006-2007 by Marc Feeley, All Rights Reserved.

(define-library (spheres/crypto io)
  (export enter-filename-password
          enter-new-filename-password
          read-encrypted-file
          write-encrypted-file)

  (import (spheres/io genport)
          (spheres/io ttyui)
          (spheres/crypto aes)
          (spheres/crypto rsa)
          (spheres/crypto pad))

  (include "io.scm"))
