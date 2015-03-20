;;!!! Sack HTTP server middleware
;;
;; Copyright (C) 2008-2009 Per Eckerdal, 2010-2013 Mikael More, 2005-2007 Marc Feeley.

(define-library (spheres/net sack-server)
  (export sack-start!)

  (import (spheres/core exception)
          ;; string-downcase! string-downcase reverse-list->string string-prefix?
          (spheres/string string)
          ;; string-split-char
          (spheres/string string-extra)
          ;; string->utf8-vector
          (spheres/string u8vector)
          (spheres/structure token-table)
          (spheres/os date-format)
          (spheres/net/sack uri)
          (spheres/net/sack http-util)
          (spheres/net/sack io-primitives))

  (include "sack-server.scm"))
