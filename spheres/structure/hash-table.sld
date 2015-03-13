;;!!! SRFI-69: Basic hash tables
;; .author Arthur T Smyles. Implemented based on Gambit's tables
;; .author Álvaro Castro-Castilla, 2014 - Ported to SchemeSpheres

(define-library (spheres/structure hash-table)
  (export make-hash-table
          hash-table?
          alist->hash-table
          hash-table-equivalence-function
          hash-table-hash-function
          hash-table-ref
          hash-table-ref/default
          hash-table-set!
          hash-table-delete!
          hash-table-exists?
          hash-table-update!
          hash-table-update!/default
          hash-table-size
          hash-table-keys
          hash-table-values
          hash-table-walk
          hash-table-fold
          hash-table->alist
          hash-table-copy
          hash-table-merge!
          hash
          string-hash
          string-ci-hash
          hash-by-identity)

  (include "hash-table.scm"))
