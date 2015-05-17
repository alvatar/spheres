;;!!! Persistent Map
;; .author Francesco Bracchi, 2013
;; .source https://github.com/francesco-bracchi/gambit-persistent
;; .version 702cf28b6f60605e874eb036699df0bc20fc9bc2

(define-library (spheres/structure persistent-map)
  (export persistent-map-exception? ;; exceptions
          persistent-map-exception-procedure

          persistent-map-key-not-found-exception?
          persistent-map-key-not-found-exception-procedure
          persistent-map-key-not-found-exception-arguments

          persistent-map-type-exception?
          persistent-map-type-exception-procedure
          persistent-map-type-exception-arguments
          persistent-map-type-exception-arg-num

          ;; constructors
          make-persistent-map
          persistent-map?
          persistent-map-length
          persistent-map-set
          persistent-map-ref
          persistent-map-reduce
          persistent-map-for-each
          persistent-map-keys
          persistent-map-values
          persistent-map-merge
          persistent-map->list
          list->persistent-map)

  (include "persistent-map.scm"))
