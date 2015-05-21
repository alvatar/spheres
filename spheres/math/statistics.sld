;;!!! Statistics procedures
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/math statistics)
  (export mean
          median
          mode
          geometric-mean
          range
          percentile
          variance
          standard-deviation
          coefficient-of-variation
          standard-error-of-the-mean)
  (import (spheres/algorithm list)
          (spheres/algorithm sort-merge)
          (spheres/structure hash-table))

  (include "statistics.scm"))
