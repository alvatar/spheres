;;!!! Logging
;;
;; .author Alvaro Castro-Castilla, 2014-2015

(define-library (spheres/util log)
  (export log
          info
          warn
          error
          log-debug
          info-debug
          err-debug)

  ;; Conditionally-expanded macros. Will only log on debug mode
  (cond-expand
   (debug
    (define-syntax info-debug
      (syntax-rules ()
        ((_ . ?forms)
         (info . ?forms))))
    (define-syntax warn-debug
      (syntax-rules ()
        ((_ . ?forms)
         (warn . ?forms))))
    (define-syntax err-debug
      (syntax-rules ()
        ((_ . ?forms)
         (err . ?forms)))))
   (else
    (define-syntax info-debug
      (syntax-rules ()
        ((_ . ?forms)
         #!void)))
    (define-syntax warn-debug
      (syntax-rules ()
        ((_ . ?forms)
         #!void)))
    (define-syntax err-debug
      (syntax-rules ()
        ((_ . ?forms)
         #!void)))))

  (include "log.scm"))
