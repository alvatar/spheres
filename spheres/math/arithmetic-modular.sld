;;!!! Modular arithmetic
;; .author Aubrey Jaffer, Copyright (C) 1991, 1993, 1995, 2001, 2002, 2006
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/math arithmetic-modular)
  (export modular:extended-euclid
          modular:symmetric-modulus
          modular:characteristic
          modular:normalize
          modular:invertable?
          modular:invert
          modular:negate
          modular:+
          modular:-
          modular:*
          modular:expt)

  (include "arithmetic-modular.scm"))
