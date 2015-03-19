(define-library (spheres/net/sack io-primitives)
  (export io-primitives-read-subu8vector
          io-primitives-write-subu8vector
          io-primitives-display
          io-primitives-force-output
          io-primitives-read-u8
          io-primitives-read-line-until-crlf
          io-primitives#with-io-primitives
          with-standard-primitives)

  (define-macro (io-primitives-read-subu8vector v)
    `(vector-ref ,v 0))

  (define-macro (io-primitives-read-substring v)
    `(vector-ref ,v 1))

  (define-macro (io-primitives-read-u8 v)
    `(vector-ref ,v 2))

  (define-macro (io-primitives-write-subu8vector v)
    `(vector-ref ,v 3))

  (define-macro (io-primitives-display v)
    `(vector-ref ,v 4))

  (define-macro (io-primitives-force-output v)
    `(vector-ref ,v 5))

  (define-macro (io-primitives-close-port v)
    `(vector-ref ,v 6))

  (define-macro (io-primitives-read-line-until-crlf v)
    `(vector-ref ,v 7))

  (define-macro (io-primitives#with-io-primitives io-primitives . code)
    `(let ((read-subu8vector (io-primitives-read-subu8vector ,io-primitives))
           (read-substring (io-primitives-read-substring ,io-primitives))
           (read-u8 (io-primitives-read-u8 ,io-primitives))
           (write-subu8vector (io-primitives-write-subu8vector ,io-primitives))
           (display (io-primitives-display ,io-primitives))
           (force-output (io-primitives-force-output ,io-primitives))
           (close-port (io-primitives-close-port ,io-primitives))
           (read-line-until-crlf/s ((io-primitives-read-line-until-crlf ,io-primitives) #f)) ; return-partial-string? = #t.
           (read-line-until-crlf/fal ((io-primitives-read-line-until-crlf ,io-primitives) #t))) ; return-partial-string? = #f.
       ,@code))

  (include "io-primitives.scm"))
