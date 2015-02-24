;;!!! FMT: formatting library
;; .author Alex Shinn, 2006-2009
;; .version 0.8.4
;; Copyright (c) 2006-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-library (spheres/string fmt)
  (export <TODO>)

  (import (spheres/algorithm list)
          (spheres/structure hash-table)
          (spheres/string string))

  (include "fmt/let-optionals.scm") ; if you don't have LET-OPTIONALS*
  (include "fmt/make-eq-table.scm")
  (include "fmt/mantissa.scm")
  (include "fmt/fmt.scm")
  (include "fmt/fmt-pretty.scm")     ; optional pretty printing
  (include "fmt/fmt-column.scm")     ; optional columnar output
  (include "fmt/fmt-c.scm")          ; optional C formatting utilities
  (include "fmt/fmt-js.scm")         ; optional C formatting utilities
  (include "fmt/fmt-color.scm")      ; optional color utilities
  (include "fmt/fmt-unicode.scm"))
