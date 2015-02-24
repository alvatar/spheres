;;!!! Comma-Separated Value (CSV)
;; .author Neil W. Van Dyke
;; .author Vincent St-Amour
;; .license: lgpl/v2.1

(define-library (spheres/dataformat csv)
  (export make-csv-reader-maker
          make-csv-reader
          csv-for-each
          csv-map
          csv->list
          csv->sxml)

  (include "csv.scm"))
