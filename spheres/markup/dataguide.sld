;;!!! DataGuide is a "structural summary" for semistructured data and may be
;; considered as analog of traditional database schema in context of
;; semistructured data management.

(define-library (spheres/markup dataguide)
  (export sxml-guide-flat
          sxml-guide
          xml-guide-flat)
  (import (spheres/core base)
          (spheres/markup sxpath-context-xlink))

  (include "internal/ssax-macros.scm")
  (include "internal/dataguide.scm"))
