;;!!! Persistent Zipper Vector
;; .author Francesco Bracchi, 2013
;; .source https://github.com/francesco-bracchi/gambit-persistent
;; .version 702cf28b6f60605e874eb036699df0bc20fc9bc2

(define-library (spheres/structure persistent-zipper-vector)
  (export persistent-vector ;; creators
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

  (include "persistent-zipper-vector.scm"))
