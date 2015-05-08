;;!!! Treap data: an ordered dictionary data structure
;; .author Oleg Kiselyov, 2004
;; .author Ivan Raikov
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure treap)
  (export make-treap)

  (import (spheres/core base))

  (include "treap.scm"))
