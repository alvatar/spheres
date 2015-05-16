;;!!! Random Access List
;; .author Juergen Lorenz, 2011-2013

(define-library (spheres/structure random-access-list)
  (export make-ral
          ral?
          ral-node?
          ral-null?
          ral-item?
          ral-width
          ral-start
          ral-height
          ral-max-height
          ral-count
          ral-level
          ral-place
          ral-place-next
          ral-cursor-jump
          ral-cursor-next
          ral-ref ral-set!
          ral-insert!
          ral-remove!
          ral-clear!
          ral-add!
          ral-add-left!
          ;;ral-start! ral-forth! ral-moveto! ral-go! ral-go-on!
          ral->list
          ;;ral-print
          ral-filter
          ral-map
          ral-restructure
          ral-for-each
          ral-split
          ral-join
          ral-from-upto
          ral-eql?
          ral-equal?
          ral-eqv?
          ral-eq?)

  (import (spheres/core base))

  (include "random-access-list.scm"))



;; (import scheme
;;   (only chicken
;;         define-record-type define-record-printer optional when unless
;;         define-inline condition-case let-optionals void fxmin fxmax fxshr
;;         fx+ fx- fx>= fx> fx< fx= fx<= gensym vector-resize getter-with-setter)
;;         ; get-output-string open-output-string)
;;   (only data-structures o identity list-of?)
;;   (only extras format random))
;; (import scheme
;;         dbc
;;         (only chicken cut
;;               fixnum? fx+ fx- fx>= fx> fx< fx<= fx=)
;;                                         ; get-output-string open-output-string)
;;         (only data-structures list-of? constantly)
;;         (prefix %random-access-lists %))
