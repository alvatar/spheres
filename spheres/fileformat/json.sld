;;!!! JSON reader and writer

(define-library (spheres/fileformat json)
  (export json-null json-null? json-object json-read json-write)

  (include "json.scm"))
