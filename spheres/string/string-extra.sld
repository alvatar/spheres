;;!!! Extra string utilities
;; .author Per Eckerdal
;; .author Mikael More
;; .author Marc Feeley
;; .author Alvaro Castro-Castilla

(define-library (spheres/string string-extra)
  (export string-remove-prefix
          string-remove-suffix
          char->string
          string-strip
          string-replace-char
          string-split-char
          string-split-at-first
          string-split-at-first-nice
          string-name-split
          string-set-first-char!
          string-capitalize!
          string-decapitalize!
          string-constantize
          string-camelize
          string-dasherize
          string-spaceize
          string-humanize
          string-remove-suffix
          string-remove-prefix
          string-pluralize
          string-depluralize
          string-invert
          string-uninvert
          dumps)

  (import (spheres/string string)
          (spheres/string u8vector)
          (spheres/algorithm list))

  (include "string-extra.scm"))
