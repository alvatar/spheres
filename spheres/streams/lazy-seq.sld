;;!!! Alternative lazy sequences based on lambdas
;;
;; .author Moritz Heidkamp, 2014
;; .author Alvaro Castro-Castilla, 2015

(define-libray (spheres/streams lazy-seq)
  (export make-lazy-seq
          lazy-seq
          lazy-null
          lazy-seq?
          lazy-seq-realized?
          lazy-null?
          lazy-seq->list
          list->lazy-seq
          lazy-seq->string
          string->lazy-seq
          lazy-list
          lazy-head
          lazy-tail
          lazy-length
          lazy-append
          lazy-reverse
          lazy-take
          lazy-drop
          lazy-ref
          lazy-take-while
          lazy-drop-while
          lazy-map
          lazy-filter
          lazy-each
          lazy-iterate
          lazy-repeat
          lazy-repeatedly
          lazy-numbers
          input-port->lazy-seq
          lazy-cycle
          lazy-append-map
          lazy-fold
          lazy-concatenate
          lazy-flatten)

  (import (spheres/core base))

  (define-syntax lazy-seq
    (syntax-rules ()
      ((_ body ...)
       (make-lazy-seq
        (lambda () body ...)))))

  (include "lazy-seq.scm"))
