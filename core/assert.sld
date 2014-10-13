;;!!! Assertion procedures
;; .author Oleg Kiselyov
;; .author Álvaro Castro Castilla, 2013-2014

(define-library (spheres/core assert)
  (export assert
          assure
          assertion-violation
          assertion-errors-display)
  (import (gambit))

  (define-syntax assert
    (let-syntax ((cerr
                  (syntax-rules ()
                    ((cerr . ?forms)
                     (for-each (lambda (f) (if (procedure? f)
                                          (f (current-error-port))
                                          (display f (current-error-port))))
                               (list . ?forms))))))
      (syntax-rules ()
        ((assert _expr . _others)
         (letrec-syntax
             ((write-report
               (syntax-rules ()
                 ;; given the list of expressions or vars,
                 ;; create a cerr form
                 ((_ exprs prologue)
                  (k!reverse () (cerr . prologue)
                             (write-report* ! exprs #\newline)))))
              (write-report*
               (syntax-rules ()
                 ((_ rev-prologue () prefix)
                  (k!reverse () ("\n" . rev-prologue) (k!id !)))
                 ((_ rev-prologue (x . rest) prefix)
                  (symbol?? x
                            (write-report* (x ": " 'x #\newline . rev-prologue) 
                                           rest #\newline)
                            (write-report* (x prefix . rev-prologue) rest "")))))
              ;; return the list of all unique "interesting"
              ;; variables in the expr. Variables that are certain
              ;; to be bound to procedures are not interesting.
              (vars-of 
               (syntax-rules (!)
                 ((_ vars (op . args) (k-head ! . k-args))
                  (id-memv?? op 
                             (quote let let* letrec let*-values lambda cond quasiquote
                                    case define do assert)
                             (k-head vars . k-args) ; won't go inside
                             ;; ignore the head of the application
                             (vars-of* vars args (k-head ! . k-args))))
                 ;; not an application -- ignore
                 ((_ vars non-app (k-head ! . k-args)) (k-head vars . k-args))))
              (vars-of*
               (syntax-rules (!)
                 ((_ vars () (k-head ! . k-args)) (k-head vars . k-args))
                 ((_ vars (x . rest) k)
                  (symbol?? x
                            (id-memv?? x vars
                                       (vars-of* vars rest k)
                                       (vars-of* (x . vars) rest k))
                            (vars-of vars x (vars-of* ! rest k))))))
              (do-assert
               (syntax-rules (report:)
                 ((_ () expr)           ; the most common case
                  (do-assert-c expr))
                 ((_ () expr report: . others) ; another common case
                  (do-assert-c expr others))
                 ((_ () expr . others)
                  (do-assert (expr and) . others))
                 ((_ exprs)
                  (k!reverse () exprs (do-assert-c !)))
                 ((_ exprs report: . others)
                  (k!reverse () exprs (do-assert-c ! others)))
                 ((_ exprs x . others)
                  (do-assert (x . exprs) . others))))
              (do-assert-c
               (syntax-rules ()
                 ((_ exprs)
                  (or exprs
                      (begin (vars-of () exprs
                                      (write-report ! 
                                                    ("failed assertion: " 'exprs "\n" "bindings")))
                             (error "assertion failure"))))
                 ((_ exprs others)
                  (or exprs
                      (begin (write-report others
                                           ("failed assertion: " 'exprs))
                             (error "assertion failure")))))))
           (do-assert () _expr . _others))))))

  (define-syntax assure
    (syntax-rules ()
      ((assure exp error-msg) (assert exp report: error-msg))))

  (include "assert.scm"))
