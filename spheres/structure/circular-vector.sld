;;!!! A very simple fixed-size circular buffer based on a vector
;; .author Per Eckerdal
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure circular-vector)
  (export make-circular-vector
          circular-vector-length
          circular-vector-fill!
          circular-vector-push!
          circular-vector-first
          circular-vector-last)

  (include "circular-vector.scm"))
