;;!!! Simple parsing of input
;;
;; .author Oleg Kiselyov
;; .author Marco Benelli, 2010
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 2010 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.
;;
;; Original taken from: http://okmij.org/ftp/Scheme/xml.html

(define-library (spheres/parser simple)
  (export peek-next-char
          assert-curr-char
          skip-until
          skip-while
          input-parse:init-buffer
          next-token
          next-token-of
          read-text-line
          read-string)
  (import (spheres/string string))

  (include "simple.scm"))
