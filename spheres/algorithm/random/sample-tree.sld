;;!!! Random sampling of trees
;; .author Oleg Kiselyov
;; .author Alvaro Castro-Castilla, 2015. See LICENSE

(define-library (spheres/algorithm/random sample-tree)
  (export random-node
          random-node/leaves)
  (import (spheres/algorithm/random sample))

  (include "sample-tree.scm"))
