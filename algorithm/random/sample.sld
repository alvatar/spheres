;;!!! Reservoir sampling, random selection
;; .author Alvaro Castro-Castilla, 2012-2014

(define-library (spheres/algorithm/random sample)
  (export random-sample)
  (import (spheres/streams primitive)
          (spheres/streams derived))
  
  (include "sample.scm"))
