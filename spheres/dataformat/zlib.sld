;;!!! Zlib compression
;; .author Manuel Serrano, 2006
;; .author Marc Feeley, 2006-2012
;; Copyright (c) 2006 by Manuel Serrano, All Rights Reserved.
;; Copyright (c) 2006-2012 by Marc Feeley, All Rights Reserved.

(define-library (spheres/dataformat zlib)
  (export gzip-genport
          deflate-genport
          gzip-u8vector

          gunzip-genport
          inflate-genport
          gunzip-u8vector

          make-zlib-condition
          zlib-condition?
          zlib-condition-msg)
  (import (spheres/io genport)
          (spheres/crypto digest))

  (include "zlib.scm"))
