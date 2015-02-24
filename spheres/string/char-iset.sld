;;!!! SRFI-14 Char set library, implementation based on Integer Sets
;; .author Alex Shinn

(define-library (spheres/string char-iset)
  (export char-set?
          char-set-contains?
          char-set:lower-case
          char-set:upper-case
          char-set:title-case
          char-set:letter
          char-set:digit
          char-set:letter+digit
          char-set:graphic
          char-set:printing
          char-set:whitespace
          char-set:iso-control
          char-set:punctuation
          char-set:symbol
          char-set:hex-digit
          char-set:blank
          char-set:regional-indicator
          char-set:extend-or-spacing-mark
          char-set:hangul-l
          char-set:hangul-v
          char-set:hangul-t
          char-set:hangul-lv
          char-set:hangul-lvt)
  (import (spheres/structure integer-set))

  (include "char-iset.scm"))
