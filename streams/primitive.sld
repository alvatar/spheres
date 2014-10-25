;;!! Streams primitives
;; .author Philip L. Bewig

(define-library (spheres/streams primitive)
  (export stream-lazy
          stream-delay
          stream-cons
          stream-lambda
          stream-eager
          stream-force
          stream-null
          stream-pair?
          stream-null?
          stream-car
          stream-cdr)

  (define-syntax stream-lazy
    (syntax-rules ()
      ((stream-lazy expr)
       (make-stream
        (cons 'lazy (lambda () expr))))))

  (define-syntax stream-delay
    (syntax-rules ()
      ((stream-delay expr)
       (stream-lazy (stream-eager expr)))))

  (define-syntax stream-cons
    (syntax-rules ()
      ((stream-cons obj strm)
       (stream-eager (make-stream-pare (stream-delay obj) (stream-lazy strm))))))

  (define-syntax stream-lambda
    (syntax-rules ()
      ((stream-lambda formals body0 body1 ...)
       (lambda formals (stream-lazy (let () body0 body1 ...))))))

  (include "primitive.scm"))
