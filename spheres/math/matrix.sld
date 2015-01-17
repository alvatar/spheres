(define-library (spheres/math matrix)
  (export make-matrix
          matrix?
          matrix-rows
          matrix-columns
          matrix-ref
          matrix-set!
          matrix:+matrix
          matrix:+scalar
          matrix:+
          matrix:-matrix
          matrix:-scalar
          matrix:-
          matrix:*matrix
          matrix:*scalar
          matrix:*
          matrix:transpose
          matrix:minor-submatrix
          matrix:determinant
          matrix:cofactor
          matrix:inverse
          matrix:cholesky
          matrix:inverse/cholesky
          matrix:map)

  (include "matrix.scm"))
