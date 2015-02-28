;!!! rfc1123 - date format
;; .author Marco Benelli, 2012
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 2012 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

(define-library (spheres/os date-format)
  (export weekdays
          months
          seconds->string
          time->string)

  (include "date-format.scm"))
