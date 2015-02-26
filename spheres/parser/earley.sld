;;!!! The Earley parser
;; .author Marc Feeley, 1990-2007
;; .license lgpl/v2.1

(define-library (spheres/parser earley)
  (export make-parser
          parse->parsed?
          parse->trees
          parse->nb-trees)

  (include "earley.scm"))
