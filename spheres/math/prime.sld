;;!!! Prime numbers
;; .author Aubrey Jaffer, Copyright (C) 1991, 1993, 1995, 2001, 2002, 2006
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/math prime)
  (export prime?
          primes<
          primes>)

  (import (spheres/math arithmetic-modular))

  (include "prime.scm"))
