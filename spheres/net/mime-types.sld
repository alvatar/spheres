;;!!! Filename extension to MIME content-type lookup table module
;; .author Per Eckerdal, 2008
;; .author Mikael More, 2013
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/net mime-types)
  (export mimes
          filename-get-extension
          filename->mime-type
          filename->mime-content-type)

  (import (spheres/string string)
          (spheres/string string-extra))

  (include "mime-types.scm"))
