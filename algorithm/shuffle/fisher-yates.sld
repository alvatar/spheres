;;!!! Shuffle algorithms
;; .author Taylor Campbell
;; .author Alvaro Castro-Castilla, 2014

(define-libary (spheres/algorithm/shuffle fisher-yates)
  (export Fisher-Yates-shuffler
          sequence-exchanger
          shuffle-vector!
          shuffle-string!)

  (include "fisher-yates.scm"))
