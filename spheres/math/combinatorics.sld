;;!!! Procedures for countable discrete structures and combinatorial problems
;; .author Alvaro Castro-Castilla, 2012-2015

(define-library (spheres/math combinatorics)
  (export binomial-coefficient
          permutations
          permutations-for-each
          combinations
          combinations-for-each
          non-continuous-sequences
          power-set
          power-set-for-each
          subsets
          cartesian-product
          cartesian-product-for-each
          cartesian-product-right
          cartesian-product-right-for-each)
  (import (spheres/algorithm list)
          (spheres/math arithmetic))

  (include "combinatorics.scm"))
