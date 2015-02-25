;;!!! URL encoding and decodingb
;; .author Marc Feeley, 2011
;;
;; Copyright (c) 2011 by Marc Feeley, All Rights Reserved.

(define-library (spheres/net url)
  (export url-encode
          url-decode)

  (include "url.scm"))
