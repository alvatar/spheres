;;!!! Knuth-Morris-Pratt Fixed-Pattern Search Algorithm
;;
;; This code is written by Taylor R. Campbell and placed in the Public
;; Domain.  All warranties are disclaimed.

(define-library (spheres/algorithm knuth-morris-pratt-search)
  (export u8vector-forward-search-cache/kmp
          u8vector-backward-search-cache/kmp
          u8vector-search-forward/kmp
          u8vector-search-backward/kmp
          u8vector-search-forward*/kmp
          u8vector-search-backward*/kmp)
  (import (spheres/algorithm list))

  (include "knuth-morris-pratt-search.scm"))
