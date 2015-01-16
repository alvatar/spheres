(define-library (spheres/math vect2)
  (export make-vect2 ;; type
          vect2-x
          vect2-y
          vect2?
          vect2-x-set!
          vect2-y-set!
          vect2:+vect2 ;; operations
          vect2:-vect2
          vect2:*vect2
          vect2:*scalar
          vect2:=vect2
          vect2:proportional-vect2?
          vect2:make-zero
          vect2:squared-magnitude
          vect2:symmetric
          vect2:x-projection
          vect2:y-projection
          vect2:max-component
          vect2:min-component
          vect2:x/y
          vect2:y/x
          vect2:abs
          vect2:inverses
          vect2:clamp-vect2
          vect2:clamp-scalars
          vect2:inexact->exact
          vect2:exact->inexact
          vect2:~=
          vect2:~zero
          vect2:~sqrt
          vect2:~magnitude
          vect2:~normalize
          vect2:~inverses
          vect2:random
          vect2:random-symmetric
          vect2:~random
          vect2:~random-symmetric)
  
  (include "vect2.scm"))
