;;!!! Skip Lists
;; .author Taylor R. Campbell, 2007-2009

(define-library (spheres/structure skip-list)
  (export make-skip-list-type/comparator
          make-skip-list-type/order-predicate
          skip-list-type?
          skip-list-type/key-comparator
          skip-list-type/key-order-predicate
          skip-list?
          skip-list/type
          skip-list/count
          skip-list/fold
          skip-list/empty?
          alist->skip-list
          skip-list->alist
          skip-list/key-list
          skip-list/datum-list
          skip-list/lookup
          skip-list/member?
          skip-list/insert!
          skip-list/modify!
          skip-list/delete!
          skip-list/min
          skip-list/min-datum
          skip-list/min-pair
          skip-list/delete-min!
          skip-list/delete-min-datum!
          skip-list/delete-min-pair!
          skip-list/update-min!
          skip-list/max
          skip-list/max-datum
          skip-list/max-pair
          skip-list/delete-max!
          skip-list/delete-max-datum!
          skip-list/delete-max-pair!
          skip-list/update-max!
          skip-list/split<!
          skip-list/split>=!
          skip-list/split>!
          skip-list/split>=!
          skip-list/union!
          skip-list/left-union!
          skip-list/right-union!
          skip-list/union-merge!
          skip-list/search
          skip-list/update!
          skip-list/update-min!
          skip-list/update-max!
          skip-list/update-max-node!)

  (include "skip-list.scm"))
