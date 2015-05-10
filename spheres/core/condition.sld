;;!!! SRFI-34 and SRFI-35 Exception Handling for Programs, Conditions
;; .author Richard Kelsey, Michael Sperber, 2002
;; Copyright (C) Richard Kelsey, Michael Sperber (2002). All Rights Reserved.

(define-library (spheres/core condition)
  (export guard
          define-condition-type
          condition
          make-condition-type
          condition-type?
          condition-subtype?
          condition-type-field-supertypep
          make-condition
          condition-has-type?
          condition-ref
          type-field-alist-ref
          make-compound-condition
          extract-condition
          type-field-alist->condition
          condition-types
          check-condition-type-field-alist
          &condition)
  (import (spheres/algorithm list))

  ;; Provided by Gambit natively:
  ;; with-exception-handler
  ;; raise

  ;;! guard
  ;; Examples
  ;; (guard (condition
  ;;         (else
  ;;          (display "condition: ")
  ;;          (write condition)
  ;;          (newline)
  ;;          'exception))
  ;;        (+ 1 (raise 'an-error)))
  ;; PRINTS: condition: an-error
  ;; => exception
  ;;
  ;; (guard (condition
  ;;         (else
  ;;          (display "something went wrong")
  ;;          (newline)
  ;;          'dont-care))
  ;;        (+ 1 (raise 'an-error)))
  ;; PRINTS: something went wrong
  ;; => dont-care
  ;;
  ;; (guard (condition
  ;;         ((assq 'a condition) => cdr)
  ;;         ((assq 'b condition)))
  ;;        (raise (list (cons 'a 42))))
  ;; => 42
  ;;
  ;; (guard (condition
  ;;         ((assq 'a condition) => cdr)
  ;;         ((assq 'b condition)))
  ;;        (raise (list (cons 'b 23))))
  ;; => (b . 23)

  (define-syntax %%guard-aux
    (syntax-rules (else =>)
      ((_ reraise (else result1 result2 ...))
       (begin result1 result2 ...))
      ((_ reraise (test => result))
       (let ((temp test))
         (if temp
             (result temp)
             reraise)))
      ((_ reraise (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (%%guard-aux reraise clause1 clause2 ...))))
      ((_ reraise (test))
       test)
      ((_ reraise (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (%%guard-aux reraise clause1 clause2 ...))))
      ((_ reraise (test result1 result2 ...))
       (if test
           (begin result1 result2 ...)
           reraise))
      ((_ reraise (test result1 result2 ...) clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (%%guard-aux reraise clause1 clause2 ...)))))

  (define-syntax guard
    (syntax-rules ()
      ((_ (var clause ...) e1 e2 ...)
       ((call-with-current-continuation
         (lambda (guard-k)
           (with-exception-handler
            (lambda (condition)
              ((call-with-current-continuation
                (lambda (handler-k)
                  (guard-k
                   (lambda ()
                     (let ((var condition)) ; clauses may SET! var
                       (%%guard-aux (handler-k (lambda ()
                                                 (raise condition)))
                                    clause ...))))))))
            (lambda ()
              (call-with-values
                  (lambda () e1 e2 ...)
                (lambda args
                  (guard-k (lambda ()
                             (apply values args)))))))))))))


  ;;!! SRFI-35 Conditions
  ;; Copyright (C) Richard Kelsey, Michael Sperber (2002). All Rights Reserved.

  ;;! define-condition-type
  (define-syntax define-condition-type
    (syntax-rules ()
      ((define-condition-type ?name ?supertype ?predicate
         (?field1 ?accessor1) ...)
       (begin
         (define ?name
           (make-condition-type '?name
                                ?supertype
                                '(?field1 ...)))
         (define (?predicate thing)
           (and (condition? thing)
                (condition-has-type? thing ?name)))
         (define (?accessor1 condition)
           (condition-ref (extract-condition condition ?name)
                          '?field1))
         ...))))

  ;;! condition
  (define-syntax condition
    (syntax-rules ()
      ((condition (?type1 (?field1 ?value1) ...) ...)
       (type-field-alist->condition
        (list
         (cons ?type1
               (list (cons '?field1 ?value1) ...))
         ...)))))

  (include "condition.scm"))
