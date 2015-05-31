(define-library (spheres/util check)
  (export check
          check-ec
          check-report
          check-set-mode!
          check-reset!
          check-passed?

          check:mode
          check:proc)
  (import (spheres/algorithm comprehension))

  ;;!! Check
  (define-syntax check
    (syntax-rules (=>)
      ((check expr => expected)
       (check expr (=> equal?) expected))
      ((check expr (=> equal) expected)
       (if (>= check:mode 1)
           (check:proc 'expr (lambda () expr) equal expected)))))

  ;; (*) is a compile-time check that (arg ...) is a list
  ;; of pairwise disjoint bound variables at this point.
  (define-syntax check-ec
    (let-syntax
        ((check-ec:make
          (syntax-rules (=>)
            ((check-ec:make qualifiers expr (=> equal) expected (arg ...))
             (if (>= check:mode 1)
                 (check:proc-ec
                  (let ((cases 0))
                    (let ((w (first-ec
                              #f
                              qualifiers
                              (:let equal-pred equal)
                              (:let expected-result expected)
                              (:let actual-result
                                    (let ((arg arg) ...) ; (*)
                                      expr))
                              (begin (set! cases (+ cases 1)))
                              (if (not (equal-pred actual-result expected-result)))
                              (list (list 'let (list (list 'arg arg) ...) 'expr)
                                    actual-result
                                    expected-result
                                    cases))))
                      (if w
                          (cons #f w)
                          (list #t
                                '(check-ec qualifiers
                                           expr (=> equal)
                                           expected (arg ...))
                                (if #f #f)
                                (if #f #f)
                                cases))))))))))
      (syntax-rules (nested =>)
        ((check-ec expr => expected)
         (check-ec:make (nested) expr (=> equal?) expected ()))
        ((check-ec expr (=> equal) expected)
         (check-ec:make (nested) expr (=> equal) expected ()))
        ((check-ec expr => expected (arg ...))
         (check-ec:make (nested) expr (=> equal?) expected (arg ...)))
        ((check-ec expr (=> equal) expected (arg ...))
         (check-ec:make (nested) expr (=> equal) expected (arg ...)))
        ;;
        ((check-ec qualifiers expr => expected)
         (check-ec:make qualifiers expr (=> equal?) expected ()))
        ((check-ec qualifiers expr (=> equal) expected)
         (check-ec:make qualifiers expr (=> equal) expected ()))
        ((check-ec qualifiers expr => expected (arg ...))
         (check-ec:make qualifiers expr (=> equal?) expected (arg ...)))
        ((check-ec qualifiers expr (=> equal) expected (arg ...))
         (check-ec:make qualifiers expr (=> equal) expected (arg ...)))
        ;;
        ((check-ec (nested q1 ...) q etc ...)
         (check-ec (nested q1 ... q) etc ...))
        ((check-ec q1 q2             etc ...)
         (check-ec (nested q1 q2)    etc ...)))))

  (include "check.scm"))
