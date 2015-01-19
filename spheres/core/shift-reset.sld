(define-library (spheres/core shift-reset)
  (export shift*
          shift
          reset*
          reset)

  (define-syntax reset
    (syntax-rules ()
      ((_ ?e ?f ...) (reset* (lambda () ?e ?f ...)))))

  (define-syntax shift
    (syntax-rules ()
      ((_ ?k ?e ?f ...) (shift* (lambda (?k) ?e ?f ...)))))

  (include "shift-reset.scm"))
