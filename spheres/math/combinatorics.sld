(define-library (spheres/math combinatorics)
  (export binomial-coefficient
          permutations
          combinations
          non-continuous-sequences
          power-set)
  (import (spheres/math arithmetic))

  (include "combinatorics.scm"))
