;;!!! tter implementation of SRFI-25
;; .author Jussi Piitulainen

(define-library (spheres/structure multi-dimensional-array)
  (export array?
          make-array
          shape
          array
          array-rank
          array-start
          array-end
          array-ref
          array-set!
          share-array)

  (include "type.scm")
  (include "op-tter.scm")
  (include "ix-tter.scm")
  (include "array.scm"))
