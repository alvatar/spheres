;;!!! SRFI-64
;; .author Per Bothner. Copyright (c) 2005, 2006, 2007, 2012, 2013 Per Bothner
;; .author Alvaro Castro-Castilla

(define-library (spheres/util test)
  (export
   test-begin
   test-end
   test-assert
   test-eqv
   test-eq
   test-equal
   test-approximate
   test-assert
   test-error
   test-apply
   test-with-runner
   test-match-nth
   test-match-all
   test-match-any
   test-match-name
   test-skip
   test-expect-fail
   test-read-eval-string
   test-runner-group-path
   test-group
   test-group-with-cleanup
   test-result-ref
   test-result-set!
   test-result-clear
   test-result-remove
   test-result-kind
   test-passed?
   test-log-to-file
   ;; misc test-runner functions
   test-runner?
   test-runner-reset
   test-runner-null
   test-runner-simple
   test-runner-current
   test-runner-factory
   test-runner-get
   test-runner-create
   test-runner-test-name
   ;; test-runner field setter and getter functions - see %test-record-define:
   test-runner-pass-count
   test-runner-pass-count!
   test-runner-fail-count
   test-runner-fail-count!
   test-runner-xpass-count
   test-runner-xpass-count!
   test-runner-xfail-count
   test-runner-xfail-count!
   test-runner-skip-count
   test-runner-skip-count!
   test-runner-group-stack
   test-runner-group-stack!
   test-runner-on-test-begin
   test-runner-on-test-begin!
   test-runner-on-test-end
   test-runner-on-test-end!
   test-runner-on-group-begin
   test-runner-on-group-begin!
   test-runner-on-group-end
   test-runner-on-group-end!
   test-runner-on-final
   test-runner-on-final!
   test-runner-on-bad-count
   test-runner-on-bad-count!
   test-runner-on-bad-end-name
   test-runner-on-bad-end-name!
   test-result-alist
   test-result-alist!
   test-runner-aux-value
   test-runner-aux-value!
   ;; default/simple call-back functions, used in default test-runner,
   ;; but can be called to construct more complex ones.
   test-on-group-begin-simple
   test-on-group-end-simple
   test-on-bad-count-simple
   test-on-bad-end-name-simple
   test-on-final-simple
   test-on-test-end-simple
   test-on-final-simple
   %test-begin
   %test-end
   %test-on-test-begin
   %test-on-test-end
   %test-report-result)

  (import (spheres/core condition))

  ;; internal
  (define-syntax %test-evaluate-with-catch
    (syntax-rules ()
      ((%test-evaluate-with-catch test-expression)
       (guard (err (else #f)) test-expression))))

  ;; internal
  (define-syntax %test-comp1body
    (syntax-rules ()
      ((%test-comp1body r expr)
       (let ()
         (if (%test-on-test-begin r)
             (let ()
               (let ((res (%test-evaluate-with-catch expr)))
                 (test-result-set! r 'actual-value res)
                 (%test-on-test-end r res))))
         (%test-report-result)))))

  (define-syntax %test-comp2body
    (syntax-rules ()
      ((_ r comp expected expr)
       (let ()
         (if (%test-on-test-begin r)
             (let ((exp expected))
               (test-result-set! r 'expected-value exp)
               (let ((res (%test-evaluate-with-catch expr)))
                 (test-result-set! r 'actual-value res)
                 (%test-on-test-end r (comp exp res)))))
         (%test-report-result)))))

  ;; internal
  (define-syntax %test-comp2
    (syntax-rules ()
      ((%test-comp2 comp tname expected expr)
       (let* ((r (test-runner-get))
              (name tname))
         (test-result-alist! r (list (cons 'test-name tname)))
         (%test-comp2body r comp expected expr)))
      ((%test-comp2 comp expected expr)
       (let* ((r (test-runner-get)))
         (test-result-alist! r '())
         (%test-comp2body r comp expected expr)))))

  ;;! test-begin
  (define-syntax test-begin
    (syntax-rules ()
      ((test-begin suite-name)
       (%test-begin suite-name #f))
      ((test-begin suite-name count)
       (%test-begin suite-name count))))

  ;;! test-group
  (define-syntax test-group
    (syntax-rules ()
      ((test-group suite-name . body)
       (let ((r (test-runner-current)))
         ;; Ideally should also set line-number, if available.
         (test-result-alist! r (list (cons 'test-name suite-name)))
         (if (%test-should-execute r)
             (dynamic-wind
                 (lambda () (test-begin suite-name))
                 (lambda () . body)
                 (lambda () (test-end  suite-name))))))))

  ;;! test-group-with-cleanup
  (define-syntax test-group-with-cleanup
    (syntax-rules ()
      ((test-group-with-cleanup suite-name form cleanup-form)
       (test-group suite-name
                   (dynamic-wind
                       (lambda () #f)
                       (lambda () form)
                       (lambda () cleanup-form))))
      ((test-group-with-cleanup suite-name cleanup-form)
       (test-group-with-cleanup suite-name #f cleanup-form))
      ((test-group-with-cleanup suite-name form1 form2 form3 . rest)
       (test-group-with-cleanup suite-name (begin form1 form2) form3 . rest))))

  ;;! test-result-ref
  (define-syntax test-result-ref
    (syntax-rules ()
      ((test-result-ref runner pname)
       (test-result-ref runner pname #f))
      ((test-result-ref runner pname default)
       (let ((p (assq pname (test-result-alist runner))))
         (if p (cdr p) default)))))

  ;;! test-end
  (define-syntax test-end
    (syntax-rules ()
      ((test-end)
       (%test-end #f '()))
      ((test-end suite-name)
       (%test-end suite-name '()))))

  ;;! test-assert
  (define-syntax test-assert
    (syntax-rules ()
      ((test-assert tname test-expression)
       (let* ((r (test-runner-get))
              (name tname))
         (test-result-alist! r '((test-name . tname)))
         (%test-comp1body r test-expression)))
      ((test-assert test-expression)
       (let* ((r (test-runner-get)))
         (test-result-alist! r '())
         (%test-comp1body r test-expression)))))

  ;;! test-equal
  (define-syntax test-equal
    (syntax-rules ()
      ((test-equal . rest)
       (%test-comp2 equal? . rest))))

  ;;! test-eqv
  (define-syntax test-eqv
    (syntax-rules ()
      ((test-eqv . rest)
       (%test-comp2 eqv? . rest))))

  ;;! test-eq
  (define-syntax test-eq
    (syntax-rules ()
      ((test-eq . rest)
       (%test-comp2 eq? . rest))))

  ;;! test-approximate
  (define-syntax test-approximate
    (syntax-rules ()
      ((test-approximate tname expected expr error)
       (%test-comp2 (%test-approximimate= error) tname expected expr))
      ((test-approximate expected expr error)
       (%test-comp2 (%test-approximimate= error) expected expr))))

  ;;! test-error
  (define-syntax %test-error
    (syntax-rules ()
      ((_ r etype expr)
       (%test-comp1body
        r
        (guard (ex ((condition-type? etype)
                    (and (condition? ex) (condition-has-type? ex etype)))
                   ((procedure? etype)
                    (etype ex))
                   ((equal? etype #t)
                    #t)
                   (else #t))
               expr #f)))))
 (define-syntax test-error
   (syntax-rules ()
     ((test-error name etype expr)
      (let ((r (test-runner-get)))
        (test-result-alist! r `((test-name . ,name)))
        (%test-error r etype expr)))
     ((test-error etype expr)
      (let ((r (test-runner-get)))
        (test-result-alist! r '())
        (%test-error r etype expr)))
     ((test-error expr)
      (let ((r (test-runner-get)))
        (test-result-alist! r '())
        (%test-error r #t expr)))))

  ;;! test-with-runner
  (define-syntax test-with-runner
    (syntax-rules ()
      ((test-with-runner runner form ...)
       (let ((saved-runner (test-runner-current)))
         (dynamic-wind
             (lambda () (test-runner-current runner))
             (lambda () form ...)
             (lambda () (test-runner-current saved-runner)))))))

  ;;! test-match-nth
  (define-syntax test-match-nth
    (syntax-rules ()
      ((test-match-nth n)
       (test-match-nth n 1))
      ((test-match-nth n count)
       (%test-match-nth n count))))

  ;;! test-match-all
  (define-syntax test-match-all
    (syntax-rules ()
      ((test-match-all pred ...)
       (%test-match-all (%test-as-specifier pred) ...))))

  ;;! test-match-any
  (define-syntax test-match-any
    (syntax-rules ()
      ((test-match-any pred ...)
       (%test-match-any (%test-as-specifier pred) ...))))

  ;;! test-skip
  (define-syntax test-skip
    (syntax-rules ()
      ((test-skip pred ...)
       (let ((runner (test-runner-get)))
         (%test-runner-skip-list! runner
                                  (cons (test-match-all (%test-as-specifier pred)  ...)
                                        (%test-runner-skip-list runner)))))))

  ;;! test-expect-fail
  (define-syntax test-expect-fail
    (syntax-rules ()
      ((test-expect-fail pred ...)
       (let ((runner (test-runner-get)))
         (%test-runner-fail-list! runner
                                  (cons (test-match-all (%test-as-specifier pred)  ...)
                                        (%test-runner-fail-list runner)))))))

  (include "test.scm"))
