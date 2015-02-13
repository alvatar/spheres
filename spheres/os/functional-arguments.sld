;;!!! SRFI-37 args-fold
;; .author Anthony Carrico 2002
;; .author Alvaro Castro-Castilla, added some extra syntax

(define-library (spheres/os functional-arguments)
  (export args-fold
          option
          option-names
          option-required-arg?
          option-optional-arg?
          option-processor
          option?
          args-fold-check
          args-fold-receive)
  
  ;;! args-fold-check
  (define-syntax args-fold-check
    (syntax-rules ()
      ((_ ?args-fold ?expected-number ?fail-processor)
       (receive args-fold-result
                ?args-fold
                (if (= (length args-fold-result) ?expected-number)
                    (apply values args-fold-result)
                    (begin (apply ?fail-processor args-fold-result)
                           (apply values (vector->list (make-vector 3 #f)))))))
      ((_ ?args-fold ?expected-number ?pass-processor ?fail-processor)
       (receive args-fold-result
                ?args-fold
                (if (= (length args-fold-result) ?expected-number)
                    (begin (apply ?pass-processor args-fold-result)
                           (apply values args-fold-result))
                    (begin (apply ?fail-processor args-fold-result)
                           (apply values (vector->list (make-vector 3 #f)))))))))

  ;;! args-fold-receive
  ;; A checked variation of RECEIVE for args-fold
  ;;
  ;; Example:
  ;;
  ;; (args-fold-receive (project-name template platforms)
  ;;                    (args-fold (cddr (command-line))
  ;;                               ;; Option processors
  ;;                               (list (option '(#\t "template") #t #f
  ;;                                             (lambda (option name arg template platforms)
  ;;                                               (values arg platforms)))
  ;;                                     (option '(#\p "platform") #t #f
  ;;                                             (lambda (option name arg template platforms)
  ;;                                               (values template (cons arg platforms)))))
  ;;                               ;; Unrecognized option processor
  ;;                               (lambda (option name arg . seeds)
  ;;                                 (println (string-append "Unrecognized option: -" (string name)))
  ;;                                 (exit error:invalid-argument))
  ;;                               ;; Operand processor: its output gets passed to receive
  ;;                               (lambda (operand template platforms)
  ;;                                 (values operand template platforms))
  ;;                               ;; Default argument values
  ;;                               #f
  ;;                               '())
  ;;                    ;; if args-fold returns the same number of arguments we expect (3)
  ;;                    (lambda (project-name template platforms)
  ;;                      (unless template
  ;;                              (println "Missing argument: template")
  ;;                              (exit error:invalid-argument))
  ;;                      (when (null? platforms)
  ;;                            (println "Missing argument: platform")
  ;;                            (exit error:invalid-argument)))
  ;;                    ;; otherwise, we consider the args-fold malformed and avoid running RECEIVE
  ;;                    ;; (it will fail, due to args number mismatch
  ;;                    (lambda args
  ;;                      (println "Missing or malformed arguments. Try \"sfusion help\" for more information.")
  ;;                      (exit error:invalid-argument))
  ;;                    (create-project project-name template platforms)) ; do the actual work
  ;;! args-fold-receive
  (define-syntax args-fold-receive
    (syntax-rules ()
      ((_ ?vars ?args-fold ?pass-processor ?fail-processor . ?forms)
       (receive ?vars
                (args-fold-check ?args-fold (length '?vars) ?pass-processor ?fail-processor)
                . ?forms))))

  (include "functional-arguments.scm"))
