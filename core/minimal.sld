;;!!! Minimal toolkit for avoiding dependencies but still enjoy some goodies
;; .author Alvaro Castro-Castilla, 2014

(define-library (spheres/core minimal)
  (export foldr
          foldl
          reduce
          filter
          any
          every
          drop
          map*
          map**
          curry
          compose
          complement
          quicksort
          string-split
          string-concatenate
          string-join)
  (import (gambit))
  (include "minimal.scm"))
