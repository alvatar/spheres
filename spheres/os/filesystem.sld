;;!!! Filesets and combinators
;; .author Alvaro Castro Castilla, 2012-2015

(define-library (spheres/os filesystem)
  (export extension=?
          ends-with?
          newer-than?
          shift
          f-and
          f-or
          f-not
          any?
          none?
          fileset
          path-add-trailing-directory-separator
          directory?
          regular?
          make-directory
          delete-file
          delete-directory
          delete-files
          copy-file
          copy-directory
          copy-files
          read-file
          read-files
          append-files)

  (include "filesystem.scm"))
