;;!!! File I/O
;; .author Alvaro Castro-Castilla, 2014-2015

(define-library (spheres/io file)
  (export u8vector->file
          file->u8vector)

  (include "file.scm"))
