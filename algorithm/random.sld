;;!!! Reservoir sampling, random selection
;; .author Alvaro Castro-Castilla, 2012-2014

(define-library (spheres/algorithm random)
  (export random-pick
          random-pick-unique
          random-extract
          random-sample
          random-sample-ordered)
  (import (spheres/core base)
          (spheres/core match)
          (spheres/algorithm list)
          (spheres/algorithm sort-merge)
          (spheres/streams primitive)
          (spheres/streams derived))
  
  (include "random.scm"))
