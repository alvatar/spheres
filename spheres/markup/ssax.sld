;;!!! SSAX XML to SXML parser

(define-library (spheres/markup ssax)
  (export xml-string->sxml
          xml-string->sxml-start-at-idx xml-string->sxml-end-at-idx
          xml-string->sxml-read-to-idx  xml-string->sxml-read-to-idx/all-data
          html-entity-unicode-numbers
          html-entity-unicode-chars)
  (import (spheres/core base))

  (include "ssax.scm"))
