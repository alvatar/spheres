;;!!! Hilbert vectors are like vectors that grow as large as they need to.
;; That is, they can be indexed by arbitrarily large nonnegative integers.
;; The implementation allows for arbitrarily large gaps by arranging
;; the entries in a tree.
;; So-called because they live in an infinite-dimensional vector
;; space...
;;
;; .author Richard Kelsey and Jonathan Rees
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 1993-2007 by Richard Kelsey and Jonathan Rees.

(define-library (spheres/structure sparse-vector)
  (export make-sparse-vector
          sparse-vector?
          sparse-vector-ref
          sparse-vector-set!
          sparse-vector->list)

  (include "sparse-vector.scm"))
