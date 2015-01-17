(define-library (spheres/math euclidean)
  (export make-identity-matrix4x4
          make-translation-matrix
          make-scaling-matrix
          make-x-rotation-matrix
          make-y-rotation-matrix
          make-z-rotation-matrix
          make-rotation-matrix)

  (include "euclidean.scm"))
