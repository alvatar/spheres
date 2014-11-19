;;!!! ctor implementation of SRFI-25
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
  (include "op-ctor.scm")
  (include "ix-ctor.scm")
  (include "array.scm"))
