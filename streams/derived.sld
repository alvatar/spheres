;;!! Streams derived

(define-library (spheres/streams derived)
  (export define-stream
          stream
          stream-match
          stream-match-test
          stream-match-pattern
          stream-of
          list->stream
          port->stream
          stream->list
          stream-append
          stream-concat
          stream-constant
          stream-drop
          stream-drop-while
          stream-filter
          stream-fold
          stream-for-each
          stream-from
          stream-iterete
          stream-length
          stream-map
          stream-range
          stream-ref
          stream-reverse
          stream-scan
          stream-take
          stream-take-while
          stream-unfold
          stream-unfolds
          stream-zip)
  (import (spheres/streams primitive))

  (define-syntax define-stream
    (syntax-rules ()
      ((define-stream (name . formal) body0 body1 ...)
       (define name (stream-lambda formal body0 body1 ...)))))

  (define-syntax stream
    (syntax-rules ()
      ((stream) stream-null)
      ((stream x y ...) (stream-cons x (stream y ...)))))

  (define-syntax stream-let
    (syntax-rules ()
      ((stream-let tag ((name val) ...) body1 body2 ...)
       ((letrec ((tag (stream-lambda (name ...) body1 body2 ...))) tag) val ...))))

  ;; Ported to syntax-rules by Kon Lovett, 2009
  (define-syntax stream-match
    (syntax-rules ()
      ((_ ?strm-expr ?clause ...)
       (let ((strm ?strm-expr))
         (cond
          ((not (stream? strm))
           (error 'stream-match "not a stream given as argument" strm))
          ((stream-match-test strm ?clause) => car)
          ...
          (else
           (error 'stream-match "no matching pattern")))) ) ) )

  (define-syntax stream-match-test
    (syntax-rules ()
      ((_ ?strm (?pattern ?fender ?expr))
       (stream-match-pattern ?strm ?pattern () (and ?fender (list ?expr))) )
      ((_ ?strm (?pattern ?expr))
       (stream-match-pattern ?strm ?pattern () (list ?expr)) ) ) )

                                        ;FIXME - this forces use of `_' identifier
  (define-syntax stream-match-pattern
    (syntax-rules (_)
      ((_ ?strm () (?binding ...) ?body)
       (and (stream-null? ?strm)
            (let (?binding ...) ?body)) )
      ((_ ?strm (_ . ?rest) (?binding ...) ?body)
       (and (stream-pair? ?strm)
            (let ((strm (stream-cdr ?strm)))
              (stream-match-pattern strm ?rest (?binding ...) ?body))) )
      ((_ ?strm (?var . ?rest) (?binding ...) ?body)
       (and (stream-pair? ?strm)
            (let ((temp (stream-car ?strm))
                  (strm (stream-cdr ?strm)))
              (stream-match-pattern strm ?rest ((?var temp) ?binding ...) ?body))) )
      ((_ ?strm _ (?binding ...) ?body)
       (let (?binding ...) ?body) )
      ((_ ?strm ?var (?binding ...) ?body)
       (let ((?var ?strm) ?binding ...) ?body) ) ) )


  ;; Original SRFI implementation
  ;; (define-syntax stream-of
  ;;   (syntax-rules ()
  ;;     ((_ expr rest ...)
  ;;      (stream-of-aux expr stream-null rest ...))))
  ;; (define-syntax stream-of-aux
  ;;   (syntax-rules (in is)
  ;;     ((stream-of-aux expr base)
  ;;      (stream-cons expr base))
  ;;     ((stream-of-aux expr base (var in stream) rest ...)
  ;;      (stream-let loop ((strm stream))
  ;;                  (if (stream-null? strm)
  ;;                      base
  ;;                      (let ((var (stream-car strm)))
  ;;                        (stream-of-aux expr (loop (stream-cdr strm)) rest ...)))))
  ;;     ((stream-of-aux expr base (var is exp) rest ...)
  ;;      (let ((var exp)) (stream-of-aux expr base rest ...)))
  ;;     ((stream-of-aux expr base pred? rest ...)
  ;;      (if pred? (stream-of-aux expr base rest ...) base))))
  (define-syntax stream-of
    (syntax-rules (is in)
      ((_ "aux" ?expr ?base)
       (stream-cons ?expr ?base))
      ((_ "aux" ?expr ?base (?var in ?strm) ?rest ...)
       (stream-let loop ((strm ?strm))
                   (if (stream-null? strm)
                       ?base
                       (let ((?var (stream-car strm)))
                         (stream-of "aux" ?expr (loop (stream-cdr strm)) ?rest ...)))))
      ((_ "aux" ?expr ?base (?var is ?exp) ?rest ...)
       (let ((?var ?exp)) (stream-of "aux" ?expr ?base ?rest ...)))
      ((_ "aux" ?expr ?base ?pred? ?rest ...)
       (if ?pred? (stream-of "aux" ?expr ?base ?rest ...) ?base))
      ((_ ?expr ?rest ...)
       (stream-of "aux" ?expr stream-null ?rest ...))))
  
  (include "derived.scm"))
