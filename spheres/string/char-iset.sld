;;!!! SRFI-14 Char set library, implementation based on Integer Sets
;; .author Alex Shinn

(define-library (spheres/string char-iset)
  (export char-set?
          char-set-contains?
          ;; Extra
          char-set
          ucs-range->char-set
          char-set-copy
          char-set-size
          char-set-fold
          char-set-for-each
          list->char-set
          string->char-set
          char-set->string
          char-set-adjoin!
          char-set-adjoin
          char-set-union
          char-set-union!
          char-set-intersection
          char-set-intersection!
          char-set-difference
          char-set-difference!
          char-set-complement
          ;; Char sets
          char-set:empty
          char-set:ascii
          char-set:full
          char-set:lower-case
          char-set:upper-case
          char-set:title-case
          char-set:letter
          char-set:punctuation
          char-set:symbol
          char-set:blank
          char-set:whitespace
          char-set:digit
          char-set:letter+digit
          char-set:hex-digit
          char-set:iso-control
          char-set:graphic
          char-set:printing
          ;; Boundary char sets
          char-set:control
          char-set:extend-or-spacing-mark
          char-set:regional-indicator
          char-set:hangul-l
          char-set:hangul-v
          char-set:hangul-t
          char-set:hangul-lv
          char-set:hangul-lvt)
  (import (spheres/structure integer-set))

  (include "char-iset.scm"))
