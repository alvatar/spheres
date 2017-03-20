;;!!! XML to SXML parser (SSAX)

(define-library (spheres/markup sxml-parser)
  (export xml-string->sxml
          xml-string->sxml-start-at-idx
          xml-string->sxml-end-at-idx
          xml-string->sxml-read-to-idx
          xml-string->sxml-read-to-idx/all-data
          html-entity-unicode-numbers
          html-entity-unicode-chars)
  (import (spheres/core base)
          (spheres/string string))

  (include "sxml-parser.scm"))
