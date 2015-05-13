;;!!! K-ary tree algorithms
;; .author Alvaro Castro-Castilla 2010

(define-library (spheres/algorithm tree)
  (export make-node
          node-data
          node-children
          node?
          make-leaf
          leaf?
          leaf-data
          k-ary:take-levels
          k-ary:extract-level
          k-ary:skim-level
          k-ary:depth)

  (import (spheres/core base)
          (spheres/algorithm list))

  (include "tree.scm"))
