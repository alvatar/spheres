;;!!! Persistent Vector
;; .author Francesco Bracchi, 2013
;; .source https://github.com/francesco-bracchi/gambit-persistent
;; .version 702cf28b6f60605e874eb036699df0bc20fc9bc2

(define-library (spheres/structure persistent-vector)
  (export persistent-vector-exception? ;; exceptions
          persistent-vector-exception-procedure
          persistent-vector-exception-arguments
          persistent-vector-exception-arg-num

          persistent-vector-range-exception?
          persistent-vector-range-exception-procedure
          persistent-vector-range-exception-arguments
          persistent-vector-range-exception-arg-num

          persistent-vector-type-exception?
          persistent-vector-type-exception-procedure
          persistent-vector-type-exception-arguments
          persistent-vector-type-exception-arg-num

          ;; creators
          persistent-vector
          make-persistent-vector

          ;; predicates
          persistent-vector?

          ;; actions
          persistent-vector-length
          persistent-vector-ref
          persistent-vector-set
          persistent-vector-map
          persistent-vector-for-each
          persistent-vector-push

          ;; conversions
          persistent-vector->vector
          vector->persistent-vector

          persistent-vector->list
          list->persistent-vector

          persistent-vector->string
          string->persistent-vector)

  (include "persistent-vector.scm"))
