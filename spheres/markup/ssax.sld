(define-library (spheres/markup ssax)
  (export html-entity-unicode-numbers
          html-entity-unicode-chars
          xml-string->sxml)
  (import (spheres/core base))

  (include "ssax.scm"))
