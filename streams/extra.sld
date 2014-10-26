(define-library (spheres/streams extra)
  (export stream-intersperse
          stream-permutations
          stream-split
          stream-unique
          stream-fold-one
          stream-member
          stream-merge
          stream-partition
          stream-finds
          stream-find
          stream-remove
          stream-every
          stream-any
          stream-and
          stream-or
          stream-fold-right
          stream-fold-right-one
          stream-assoc
          stream-equal?
          stream-quick-sort
          stream-insertion-sort
          stream-maximum
          stream-minimum)
  (import (spheres/streams primitive)
          (spheres/streams derived))

  (include "extra.scm"))
