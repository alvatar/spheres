;;!!! Shuffle algorithms
;; .author Taylor Cambpell

(define-library (spheres/algorithm shuffle)
  (export flip-coin
          binary-shuffle-list
          binary-shuffle-list!
          merge-shuffle-list
          merge-shuffle-list!
          insertion-shuffle-list
          insertion-shuffle-list!
          selection-shuffle-list
          selection-shuffle-list!
          Fisher-Yates-shuffler
          sequence-exchanger
          shuffle-vector!
          shuffle-string!)
  (import (spheres/algorithm list))

  (include "shuffle.scm"))
