;;!!! Number constraints for Kanren

(define-library (spheres/logic/kanren numbers)
  (export build-num
          zeroo
          poso
          >lo
          pluso
          minuso
          *o
          odd-*o
          bound-*o
          =lo
          <lo
          <=lo
          <o
          <=o
          /o
          splito
          logo
          exp2
          repeated-mul
          expo)

  (import (spheres/logic kanren))

  (include "numbers.scm"))
