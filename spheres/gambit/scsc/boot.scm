
;; reset environment to support iterative syntax-case development
(namespace (""))
(eval '(namespace ("")))
(set! ##expand-source (lambda (src) src))

;; eval maintains its own namespace configuration associated with the
;; interaction-cte. the content of syntax-case-prelude.scm needs to be
;; evaluated so that $sc-put-cte definitions emitted by psyntax to
;; eval share the namespace context of syntax-case-prelude. this
;; allows definitions to access functions andmap, ormap, gensym? which
;; are in the sc# namespace.

(eval
 '(include "syntax-case-prelude.scm"))

(define psyntaxpp
  "psyntax-gambit.pp")

(define psyntaxss
  "psyntax.ss")

(define make-read-expander
  (lambda (expander desourcify)
    (define include-expr-ref 1)
    (lambda (input)
      ;; begin is prepended to simulate top-level expansion. this
      ;; practice bypasses the "invalid context for definition" error
      ;; that otherwise occurs without its presence.
      (let ((expansion (expander (let ((src (vector-ref (##read-all-as-a-begin-expr-from-port
                                                            input
                                                            (##current-readtable)
                                                            ##wrap-datum
                                                            ##unwrap-datum
                                                            #f
                                                            #t)
                                                        include-expr-ref)))
                                   (if desourcify
                                       (##desourcify src)
                                       src)))))

        ;; source vectors marked as #(visit) are restored to their
        ;; former #(source1) for regular desourcification.
        (sc#unmark! expansion)
        (##desourcify expansion)))))

;;
;; eval the bootstrapping psyntax.pp
;;
(eval
 (call-with-input-file psyntaxpp
   (lambda (input)
     (input-port-readtable-set! input (readtable-sharing-allowed?-set (input-port-readtable input) 'serialize))
     `(begin ,@(read-all input)))))

;; psyntax.pp ships with ctem and rtem assigned to '(E), which emits
;; visit content to eval, and not the expanded output. this is an odd
;; choice, considering that psyntax.pp itself appends the $sc-put-cte
;; definitions after the revisit content. psyntax.pp can thus not
;; bootstrap itself with its default settings!

;; to accomodate this, we bootstrap psyntax.ss with '(E) values of
;; ctem and rtem, and then use the resultant expander to re-expand
;; psyntax.ss with ctem and rtem with respective values of '(L C) and
;; '(L) to generate the final syntax-case.scm.

(eval
 (call-with-input-file psyntaxss
   (make-read-expander sc#sc-expand #t)))

(define syntax-case
  "syntax-case.scm")

(call-with-output-file syntax-case
  (lambda (output)
    
    (define prelude "syntax-case-prelude.scm")
    (define postlude "syntax-case-postlude.scm")

    (define read-as-string
      (lambda (input)
        (read-line input #!eof)))
    
    (print port: output
           (call-with-input-file prelude
             read-as-string))
    
    (eval '(set! gensym-count 0))

    (output-port-readtable-set! output (readtable-sharing-allowed?-set (output-port-readtable output) 'serialize))

    (write
     (call-with-input-file psyntaxss
       (make-read-expander (sc#make-expander '(L C) '(L)) #f)) output)

    (print port: output
           (call-with-input-file postlude
             read-as-string))))

;;
;; reset the repl namespace
;;
(eval '(##namespace ("")))

(parameterize ((current-readtable (readtable-sharing-allowed?-set (current-readtable) 'serialize)))
    (load "syntax-case"))
