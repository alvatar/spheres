(define-library (spheres/math arithmetic)
  (export pi
          pi2
          pi/2
          -pi/2
          pi/4
          -pi/4
          pi3/4
          e
          log2e
          log10e
          ln2
          ln10
          sqrt2
          sqrt1/2
          euler
          ->integer
          logN
          sum
          product
          inverse
          square
          factorial
          extended-gcd
          modulo-inverse
          expt-mod
          totient
          modulo-power
          random-exact
          random-exact/-1/+1
          random-prime)

  (include "arithmetic.scm"))
