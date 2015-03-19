(define-library (spheres/string string-extra)
  (export string-remove-prefix
          string-remove-suffix
          char->string
          string-strip
          string-replace-char
          string-split
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


;; (import (srfi strings
;;               lists)
;;         ../misc/u8v)

  (import (spheres/string string)
          (spheres/string u8vector)
          (spheres/algorithm list))

  (include "string-extra.scm"))
