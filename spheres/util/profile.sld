;;!!! Minimalistic profiling
;;
;; .author Phil Dawe
;; .author Alvaro Castro-Castilla, 2012-2015

(define-library (spheres/util profile)

  (export (%reset-timer
           %%accum-time
           %get-times
           define/profile))

  (define-syntax define/profile
    (syntax-rules (lambda)
      ((_ (?name . ?args) . ?body)
       (define (?name . ?args)
         (%%accum-time '?name
                       (lambda () . ?body))))
      ((_ ?name (lambda (?args) . ?body))
       (define ?name
         (lambda (?args)
           (%%accum-time '?name
                         (lambda () . ?body)))))))

  (include "profile.scm"))
