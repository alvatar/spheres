(define-library (spheres/util test)
  (export test-runner-reset
          test-runner-group-path
          test-runner-null
          test-log-to-file
          test-runner-simple
          test-runner-current
          test-runner-factory
          test-runner-create
          test-on-group-begin-simple
          test-on-group-end-simple
          test-on-bad-count-simple
          test-on-bad-end-name-simple
          test-on-final-simple
          test-on-test-begin-simple
          test-on-test-end-simple
          test-result-set!
          test-result-clear
          test-result-remove
          test-result-kind
          test-passed?
          test-runner-test-name
          test-apply
          test-read-eval-string
          test-begin
          test-group
          test-group-with-cleanup
          test-result-ref
          test-end
          test-assert
          test-equal
          test-eqv
          test-eq
          test-approximate
          test-error
          test-with-runner
          test-match-nth
          test-match-all
          test-match-any
          test-skip
          test-expect-fail)
  (import (spheres/util condition))
  
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
               expr)))))
  (define-syntax test-error
    (syntax-rules ()
      ((test-error name etype expr)
       (let ((r (test-runner-get)))
         (test-assert name (%test-error r etype expr))))
      ((test-error etype expr)
       (let ((r (test-runner-get)))
         (test-assert (%test-error r etype expr))))
      ((test-error expr)
       (let ((r (test-runner-get)))
         (test-assert (%test-error r #t expr))))))

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
