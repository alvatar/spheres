;;!!! SRFI-115 (Regexp) + Perl-Compatible Regexp
;; Simple non-bactracking NFA implementation
;; .author Alex Shinn, 2013
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-library (spheres/string regexp)
  (export regexp
          regexp?
          valid-sre?
          /rx/
          regexp->sre
          char-set->sre
          regexp-matches
          regexp-matches?
          regexp-search
          regexp-replace
          regexp-replace-all
          regexp-fold
          regexp-extract
          regexp-split
          regexp-match?
          regexp-match-count
          regexp-match-submatch
          regexp-match-submatch/list
          regexp-match-submatch-start
          regexp-match-submatch-end
          regexp-match->list
          regexp-match->sexp)

  (import (spheres/core condition)
          (spheres/algorithm list)
          (spheres/structure hash-table)
          (spheres/string char-set))

  ;; Syntactic sugar.
  ;; Note: this macro has been renamed
  (define-syntax /rx/
    (syntax-rules ()
      ((rx sre ...)
       (regexp `(: sre ...)))))

  (include "char-sets/boundary.scm")
  (include "regexp.scm"))
