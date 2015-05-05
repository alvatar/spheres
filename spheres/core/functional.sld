;;!!! Functional programming procedures
;; .author Alvaro Castro-Castilla, 2012-2014. All rights reserved.

(define-library (spheres/core functional)
  (export cut
          cute
          define-associative
          curried
          define-curried
          define-memoized
          ;; functional combinators
          U
          Y
          Y!
          compose
          recursive-compose
          tail-recursive-compose
          left-section
          right-section
          curry
          uncurry
          complement
          reversed
          andf
          orf
          arguments-chainf
          arguments-eachf
          arguments-allf
          ;; memoization
          memoize/key-gen
          memoize)

  ;;------------------------------------------------------------------------------
  ;;! SRFI-26 Notation for Specializing Parameters without Currying
  ;; Sebastian.Egner@philips.com, 5-Jun-2002.
  ;; adapted from the posting by Al Petrofsky <al@petrofsky.org>
  ;; placed in the public domain.
  ;; Modified by Álvaro Castro-Castilla 2012
  ;; Made internal syntaxes private with letrec

  ;; (srfi-26-internal-cut slot-names combination . se)
  ;;   transformer used internally
  ;;     slot-names  : the internal names of the slots
  ;;     combination : procedure being specialized, followed by its arguments
  ;;     se          : slots-or-exprs, the qualifiers of the macro
  (define-syntax srfi-26-internal-cut
    (syntax-rules (<> <...>)
      ;; construct fixed- or variable-arity procedure:
      ;;   (begin proc) throws an error if proc is not an <expression>
      ((srfi-26-internal-cut (slot-name ...) (proc arg ...))
       (lambda (slot-name ...) ((begin proc) arg ...)))
      ((srfi-26-internal-cut (slot-name ...) (proc arg ...) <...>)
       (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))
      ;; process one slot-or-expr
      ((srfi-26-internal-cut (slot-name ...)   (position ...)      <>  . se)
       (srfi-26-internal-cut (slot-name ... x) (position ... x)        . se))
      ((srfi-26-internal-cut (slot-name ...)   (position ...)      nse . se)
       (srfi-26-internal-cut (slot-name ...)   (position ... nse)      . se))))
  
  ;;! cut
  (define-syntax cut
    (syntax-rules ()
      ((cut . slots-or-exprs)
       (srfi-26-internal-cut () () . slots-or-exprs))))

  ;; (srfi-26-internal-cute slot-names nse-bindings combination . se)
  ;;   transformer used internally
  ;;     slot-names     : the internal names of the slots
  ;;     nse-bindings   : let-style bindings for the non-slot expressions.
  ;;     combination    : procedure being specialized, followed by its arguments
  ;;     se             : slots-or-exprs, the qualifiers of the macro
  (define-syntax srfi-26-internal-cute
    (syntax-rules (<> <...>)
      ;; If there are no slot-or-exprs to process, then:
      ;; construct a fixed-arity procedure,
      ((srfi-26-internal-cute
        (slot-name ...) nse-bindings (proc arg ...))
       (let nse-bindings (lambda (slot-name ...) (proc arg ...))))
      ;; or a variable-arity procedure
      ((srfi-26-internal-cute
        (slot-name ...) nse-bindings (proc arg ...) <...>)
       (let nse-bindings (lambda (slot-name ... . x) (apply proc arg ... x))))
      ;; otherwise, process one slot:
      ((srfi-26-internal-cute
        (slot-name ...)         nse-bindings  (position ...)   <>  . se)
       (srfi-26-internal-cute
        (slot-name ... x)       nse-bindings  (position ... x)     . se))
      ;; or one non-slot expression
      ((srfi-26-internal-cute
        slot-names              nse-bindings  (position ...)   nse . se)
       (srfi-26-internal-cute
        slot-names ((x nse) . nse-bindings) (position ... x)       . se))))
  ;;! cute
  (define-syntax cute
    (syntax-rules ()
      ((cute . slots-or-exprs)
       (srfi-26-internal-cute () () () . slots-or-exprs))))

  
  ;;------------------------------------------------------------------------------
  ;;!! Function generation

  ;;! Defines a function and its associated associative function (that will take
  ;; any number of arguments and apply it to the result of the two previous ones)
  (define-syntax define-associative
    (let-syntax ((define-associative-aux
                   (syntax-rules ()
                     ((_ name f)
                      (define-syntax name
                        (syntax-rules ()
                          ((_ arg1 arg2)
                           (f arg1 arg2))
                          ((_ arg1 arg2 . rest)
                           (name (f arg1 arg2) . rest))))))))
      (syntax-rules ()
        ((_ name (f arg1 arg2) body)
         (begin
           (define (f arg1 arg2) body)
           (define-associative-aux name f))))))


  ;;! Define an automatically curryable function
  ;;
  ;; (define-curried (foo x y z) (+ x (/ y z))) ;; foo has arity 3
  ;; ((foo 3) 1 2) ;; (foo 3) is a procedure with arity 2
  ;; ((foo 3 1) 2) ;; (foo 3 2) is a procedure with arity 1
  (define-syntax curried
    (syntax-rules ()
      ((_ () body ...)
       (lambda () body ...))
      ((_ (arg) body ...)
       (lambda (arg) body ...))
      ((_ (arg args ...) body ...)
       (lambda (arg . rest)
         (let ((next (curried (args ...) body ...)))
           (if (null? rest)
               next
               (apply next rest)))))))

  (define-syntax define-curried
    (syntax-rules ()
      ((_ (name args ...) body ...)
       (define name (curried (args ...) body ...)))))

  ;; Curried lambda
  ;; (lambda-curried (x y z) (+ x y z)) =>
  ;;   (lambda (x) (lambda (y) (lambda (z) (+ x y z))))
  ;; (map map (map (lambda-curried (a b) (* a b)) '(1 2 3)) '((4 5 6) (7 8 9) (10 11 12)))
  ;; (##define-macro (lambda-curried bindings . body)
  ;;   (define (fold-right kons knil lis1)
  ;;     (let recur ((lis lis1))
  ;;       (if (null? lis) knil
  ;;           (let ((head (car lis)))
  ;;             (kons head (recur (cdr lis)))))))
  ;;   (if (null? bindings) `(lambda () ,@body)
  ;;       (fold-right (lambda (arg curr-body) `(lambda (,arg) ,curr-body))
  ;;                   (cons 'begin body) bindings)))

  ;;! Macro for memoized function definition (with default key generator)
  (define-syntax define-memoized
    (syntax-rules (lambda)
      ((_ (name args ...) body ...)
       (define name
         (letrec ((name (lambda (args ...) body ...)))
           (memoize name))))
      ((_ name (lambda (args ...) body ...))
       (define-memoized (name args ...) body ...))))

  ;;! Macro for memoized function definition (specifying a key generator)
  ;; (define-syntax define-memoized/key-gen
  ;;   (syntax-rules ()
  ;;     ((_ name
  ;;         (lambda (args-for-key ...) body-for-key ...)
  ;;         (lambda (args ...) body ...))
  ;;      (define name
  ;;        (letrec ((name (lambda (args ...) body ...)))
  ;;          (memoize/key-gen
  ;;           (lambda (args-for-key ...) body-for-key ...)
  ;;           name))))))

  ;;TODO!!!
  ;; r2rs-style currying define.
  ;; (define-syntax define
  ;;   (let-syntax ((old-define define))
  ;;     (letrec-syntax
  ;;         ((new-define
  ;;           (syntax-rules ()
  ;;             ((_ (var-or-prototype . args) . body)
  ;;              (new-define var-or-prototype (lambda args . body)))
  ;;             ((_ var expr) (old-define var expr)))))
  ;;       new-define)))
  ;; (define-syntax define
  ;;   (let-syntax ((old-define define))
  ;;     (define-syntax new-define
  ;;       (syntax-rules ()
  ;;         ((_ (var-or-prototype . args) . body)
  ;;          (new-define var-or-prototype (lambda args . body)))
  ;;         ((_ var expr) (old-define var expr))))
  ;;     new-define))
  ;; (let ((multiplier 2))
  ;;   (define ((curried-* x) y) (* x y))
  ;;   (map (curried-* multiplier) '(3 4 5)))

  (include "functional.scm"))
