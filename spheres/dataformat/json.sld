;;!!! JSON reader and writer

(define-library (spheres/dataformat json)
  (export json-decode
          json-encode
          json-read
          json-write
          json-error
          json-error?)

  (include "json.scm"))
