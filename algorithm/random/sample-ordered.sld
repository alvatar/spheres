;;!!! Reservoir sampling, random selection
;; .author Alvaro Castro-Castilla, 2012-2014

(define-library (spheres/algorithm/random sample-ordered)
  (export random-sample-ordered)
  (import (spheres/algorithm sort-merge)
          (spheres/streams primitive)
          (spheres/streams derived))
  
  (include "sample-ordered.scm"))
