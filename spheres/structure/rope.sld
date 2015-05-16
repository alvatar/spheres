;;!!! A straightforward implementation of "Ropes, An Alternative to Strings"
;;   H. Boehm, R. Atkinson, M. Plass
;;   Software Practice and Experience 25, Dec 1995, pp. 1315-1330
;; .author Evan Hanson, 2013
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure rope)
  (export empty-rope
          current-maximum-leaf-length
          string->rope rope->string
          rope-length rope-depth
          rope rope? rope=? rope-null?
          rope-balanced? rope-balance
          rope-ref subrope rope-reverse
          rope-append rope-concatenate
          rope-fold rope-for-each
          read-rope make-rope-iterator
          open-output-rope get-output-rope)

  (import (spheres/core base))

  (include "rope.scm"))
