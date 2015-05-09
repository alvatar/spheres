;;!!! Suffix tree, a data structure for representing sets of lists efficiently,
;; provided there is an ordering relation on the elements of lists.
;;
;; .author Ivan Raikov, 2011
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure suffix-tree)
  (export make-suffix-tree suffix-tree-equal? suffix-tree?
          suffix-tree-insert suffix-tree-remove
          suffix-tree-lookup suffix-tree-lookup/partial
          suffix-tree-partition suffix-tree-merge
          suffix-tree-branches suffix-tree-compfn suffix-tree-keyfn
          suffix-tree-branch-label suffix-tree-branch-children suffix-tree-branch-eol )
  (import (spheres/algorithm list)
          (spheres/core match))
  
  (include "suffix-tree.scm"))
