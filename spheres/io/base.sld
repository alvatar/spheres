;;!!! Basic I/O extensions
;; .author Alvaro Castro-Castilla, 2013-2015

(define-library (spheres/io base)
  (export write-u8vector
          echo-u8vector-port)

  (include "base.scm"))
