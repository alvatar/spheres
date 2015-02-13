;;!!! SRFI-29 Localization
;; .author Scott G. Miller, 2002
;; .author Alvaro Castro-Castilla, 2014-2015

;; Copyright (C) Scott G. Miller (2002). All Rights Reserved.

(define-library (spheres/os localization)
  (export current-language
          current-country
          load-bundle!
          store-bundle!
          declare-bundle!
          localized-template)
  
  (import (spheres/string/format format-srfi-28))
  
  (include "localization.scm"))
