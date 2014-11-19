(define-library (spheres/algorithm vector)
  (export vector-unfold
          vector-unfold-right
          vector-reverse-copy
          vector-concatenate
          vector-empty?
          vector=
          vector-fold
          vector-fold-right
          vector-map
          vector-map!
          vector-for-each
          vector-count
          vector-index
          vector-index-right
          vector-skip
          vector-skip-right
          vector-binary-search
          vector-any
          vector-every
          vector-swap!
          vector-reverse!
          vector-copy!
          vector-reverse-copy!
          reverse-vector->list
          reverse-list->vector)
  (import (spheres/core base))
  
  (include "vector.scm"))
