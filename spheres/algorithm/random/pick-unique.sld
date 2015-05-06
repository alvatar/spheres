;;!!! Reservoir sampling, random selection
;; .author Alvaro Castro-Castilla, 2012-2014

(define-library (spheres/algorithm/random pick-unique)
  (export random-pick-unique)
  (import (spheres/algorithm list))
  
  (include "pick-unique.scm"))
