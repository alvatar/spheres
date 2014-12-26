;;!!! String formatting SRFIs

(define-library (spheres/string format)
  (export format
          format+
          cat)

  (include "format/format-srfi-28.scm")
  (include "format/format-srfi-48.scm")
  (include "format/format-srfi-54.scm"))
