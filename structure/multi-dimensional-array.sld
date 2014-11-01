;;!!! default implementation of SRFI-25 (tter)
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

  (include "multi-dimensional-array/type.scm")
  (include "multi-dimensional-array/op-tter.scm")
  (include "multi-dimensional-array/ix-tter.scm")
  (include "multi-dimensional-array/array.scm"))
