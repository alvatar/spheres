(define-library (spheres/math bit-logic)
  (export logand
          logior
          logxor
          lognot
          bitwise-if
          logtest
          logcount
          log2-binary-factors
          first-set-bit
          logbit?
          copy-bit
          bit-field
          ash
          rotate-bit-field
          reverse-bit-field
          integer->list
          list->integer
          booleans-integer)

  (include "bit-logic.scm"))
