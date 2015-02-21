;;!!! Quickcheck implementation
;; .author Francesco Bracchi, 2012
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/util quickcheck)
  (export all/gen
          run-test
          generate!
          test!
          assert
          make-quickcheck-exception
          quickcheck-exception?
          quickcheck-exception-message

          assert!
          =>
          <=>
          all?
          exists?

          in-set<<
          alt<<
          in-set
          any-of

          ;; reals
          quantile:normal
          quantile:exponential
          quantile:uniform

          real<<
          normal<<
          exponential<<

          a-real
          a-normal
          an-exponential

          ;; integers
          in-range
          an-integer

          ;; strings
          current-grammar
          string<<
          a-string
          haiku
          kant
          insult)

  ;;! Wrapper for testing thunks
  (define-syntax test!
    (syntax-rules ()
      ((_ ?expr)
       (run-test (lambda () ?expr)))
      ((_ ?expr ?config)
       (run-test (lambda () ?expr) ?config))))

  ;;! logical implication
  (define-macro (=> p q)
    `(or (not ,p) ,q))

  ;;! logical if and only if
  (define-macro (<=> p q)
    (let ((p0 (gensym 'p))
          (q0 (gensym 'q)))
      `(let((,p0 p)
            (,q0 q))
         (and (or ,p0 (not ,q0))
              (or ,q0 (not ,p0))))))

  (define-syntax assert!
    (syntax-rules ()
      ((_ ?expr)
       (if ?expr (display #\.) (display #\*)))))

  (include "quickcheck/generator.scm")
  (include "quickcheck/run.scm")
  (include "quickcheck/generic.scm")
  (include "quickcheck/real.scm")
  (include "quickcheck/integer.scm")
  (include "quickcheck/strings.scm"))
