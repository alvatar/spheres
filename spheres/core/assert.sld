;;!!! Assertion procedures
;; .author Oleg Kiselyov
;; .author Álvaro Castro Castilla, 2013-2014

(define-library (spheres/core assert)
  (export check-arg
          assert
          assure
          assertion-violation
          assertion-errors-display)
  (import (spheres/core meta))

  (define-syntax check-arg
    (syntax-rules ()
      ((check-arg ?pred ?val ?caller)
       (let ((val ?val))
         (if (?pred val)
             val
             (begin
               (println (string-append "Value: " (object->string val)))
               (println (string-append "Predicate: " (object->string '?pred)))
               (error "Failed argument check in" '?caller)))))
      ((check-arg ?pred ?val ?caller ?reason)
       (let ((val ?val))
         (if (?pred val)
             val
             (begin
               (println (string-append "Value: " (object->string val)))
               (println (string-append "Predicate: " (object->string '?pred)))
               (println (string-append "Reason: " ?reason))
               (error "Failed argument check in" '?caller)))))))
  
  (define-syntax assert
    (syntax-rules ()
      ((assert _expr . _others)
       (letrec-syntax
           ((write-report
             ;; given the list of expressions or vars,
             ;; create a assertion-errors-display form
             (syntax-rules ()
               ((_ exprs prologue)
                (k!reverse () (assertion-errors-display . prologue)
                           (write-report* ! exprs (string #\newline))))))
            (write-report*
             (syntax-rules ()
               ((_ rev-prologue () prefix)
                (k!reverse () (#\newline . rev-prologue) (k!id !)))
               ((_ rev-prologue (x . rest) prefix)
                (symbol?? x
                          (write-report* (x ": " 'x (string #\newline) . rev-prologue) 
                                         rest (string #\newline))
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
                                        ; not an application -- ignore
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
             (syntax-rules ()
               ((_ () expr)             ; the most common case
                (do-assert-c expr))
               ((_ () expr report: . others) ; another common case
                (do-assert-c expr others))
               ((_ () expr . others) (do-assert (expr and) . others))
               ((_ exprs)
                (k!reverse () exprs (do-assert-c !)))
               ((_ exprs report: . others)
                (k!reverse () exprs (do-assert-c ! others)))
               ((_ exprs x . others) (do-assert (x . exprs) . others))))
            (do-assert-c
             (syntax-rules ()
               ((_ exprs)
                (or exprs
                    (begin (vars-of () exprs
                                    (write-report ! 
                                                  ("failed assertion: " 'exprs #\newline "bindings")))
                           (error "assertion failure"))))
               ((_ exprs others)
                (or exprs
                    (begin (write-report others
                                         ("failed assertion: " 'exprs))
                           (error "assertion failure")))))))
         (do-assert () _expr . _others)))))

  (define-syntax assure
    (syntax-rules ()
      ((assure exp error-msg) (assert exp report: error-msg))))

  (include "assert.scm"))
