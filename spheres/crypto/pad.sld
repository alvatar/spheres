;;!!! RFC 1423: Provides procedures to pad and unpad messages.
;; .author Marc Feeley, 2006-2007
;; .author Alvaro Castro-Castilla, 2015
;; .license lgpl/v2.1
;;
;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

(define-library (spheres/crypto pad)
  (export RFC1423-pad
          RFC1423-unpad)

  (include "pad.scm"))
