;;!!! Procedural representation of I/O ports
;; .author Marc Feeley, 2006-2012
;; Copyright (c) 2006-2012 by Marc Feeley, All Rights Reserved.

(define-library (spheres/io genport)
  (export implement-type-genport
          make-genport

          genport-input-port?
          genport-open-input-file
          genport-native-input-port->genport
          genport-open-input-subu8vector
          genport-open-input-u8vector
          genport-close-input-port
          genport-read-subu8vector
          genport-read-u8vector
          genport-read-file

          genport-output-port?
          genport-open-output-file
          genport-native-output-port->genport
          genport-open-output-u8vector
          genport-get-output-u8vector
          genport-close-output-port
          genport-write-subu8vector
          genport-write-u8vector
          genport-write-file)

  (include "genport.scm"))
