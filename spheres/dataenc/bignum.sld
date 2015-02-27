(define-library (spheres/dataenc bignum)
  (export bignum->fixnum-list
          fixnum-list->bignum
          bignum->u8vector
          u8vector->bignum)

  (include "bignum.scm"))
