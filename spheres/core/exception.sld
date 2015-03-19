;;!!! Exceptions
;; .author Christian Jaeger, 2006-2008
;; .author Per Eckerdal
;; .author Mikael More
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright 2006-2008 Christian Jaeger

(define-library (spheres/core exception)
  (export make-exception/continuation
          exception/continuation?
          exception/continuation-exception
          exception/continuation-exception-set!
          exception/continuation-continuation
          exception/continuation-continuation-set!
          exception/continuation-contextline
          exception/continuation-contextlines
          exception/continuation-message-in-context
          exception/continuation-procedure
          exception/continuation-locat
          exception/continuation-text
          repl-within-exception/continuation
          exception/continuation->serialisation-object
          exception/continuation->u8vector
          u8vector->backtrace
          with-exception/continuation-catcher
          exception->string
          exception/continuation->string)

  (include "exception.scm"))
