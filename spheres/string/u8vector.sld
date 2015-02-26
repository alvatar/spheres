(define-library (spheres/string u8vector)
  (export hex-char->integer
          utf8-u8vector->string
          string->utf8-u8vector
          substring->utf8-u8vector
          utf8-u8vector-port->string-reader
          u8vector->string
          string->u8vector
          ISO-8859-1-substring->u8vector
          ISO-8859-1-string->u8vector
          subu8vector->ISO-8859-1-string
          u8vector->ISO-8859-1-string
          subu8vector->hex-string
          u8vector->hex-string
          hex-string->u8vector
          u8vector-normalize-eol:s)
  
  (include "u8vector.scm"))
