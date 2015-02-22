;;!!! Boyer-Moore Fixed-Pattern Search Algorithm
;; .author Taylor R. Campbell
;; .author Alvaro Castro-Castilla, 2015
;;
;; This code is written by Taylor R. Campbell and placed in the Public
;; Domain.  All warranties are disclaimed.

(define-library (spheres/algorithm boyer-moore-search)
  (export u8vector-forward-search-cache/bm
          u8vector-backward-search-cache/bm
          u8vector-search-forward/bm
          u8vector-search-backward/bm
          u8vector-search-forward*/bm
          u8vector-search-backward*/bm)
  (import (spheres/algorithm list)
          (spheres/algorithm u8vector))

  (include "boyer-moore-search.scm"))
