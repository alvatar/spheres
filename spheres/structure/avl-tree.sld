;;!!! AVL Tree
;; .author Hans Oesterholt, 2005
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure avl-tree)
  (export avl
          avl-from-avl
          avl-insert!
          avl-remove!
          avl-find
          avl-exists?
          avl-map
          avl-for-each
          avl-filter
          avl-nodes
          avl-empty?
          avl-min
          avl-max
          avl-atomic)

  (include "avl-tree.scm"))
