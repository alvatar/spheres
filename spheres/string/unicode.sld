;;!!! Unicode handling procedures
;; .author Alvaro Castro-Castilla, 2015
;; .author Mikael More, 2010-2012
;; .author Florian Loitsch, 2007-2012
;;
;; Copyright (C) 2007-2012 Florian Loitsch
;; Copyright (C) 2010-2012 Mikael More

(define-library (spheres/string unicode)
  (export unicode-char-upper
          unicode-char-lower
          unicode-string-upper unicode-string-upper! ; "aBc" => "ABC"
          unicode-string-lower unicode-string-lower! ; "aBc" => "abc"
          unicode-string-capitalize-strict!          ; "aBc" => "Abc"
          unicode-string-capitalize-strict           ;
          string-unicode-ci<?
          string-unicode-ci<=?
          string-unicode-ci=?
          string-unicode-ci>?
          string-unicode-ci>=?)

  (include "unicode.scm"))
