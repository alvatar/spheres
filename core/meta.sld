;;!!! Macros for building complex hygienic macros
;; .author Oleg Kiselyov

(define-library (spheres/core meta)
  (export syntax-case-error
          symbol??
          id-memv??
          id-eq??
          id-eqv??
          ??!apply
          k!id
          k!reverse
          macro-trace)

  ;;! Syntax error
  (define-syntax syntax-case-error
    (syntax-rules ()
      ((_) (0))))

  ;;! A symbol? predicate at the macro-expand time
  ;; symbol?? FORM KT KF
  ;; FORM is an arbitrary form or datum
  ;; expands in KT if FORM is a symbol (identifier), Otherwise, expands in KF
  ;; (symbol?? x 'yes 'no) => yes
  ;; (symbol?? 9 'yes 'no) => no
  ;; By Oleg Kiselyov
  (define-syntax symbol??
    (syntax-rules ()
      ((symbol?? (x . y) kt kf) kf)     ; It's a pair, not a symbol
      ((symbol?? #(x ...) kt kf) kf)    ; It's a vector, not a symbol
      ((symbol?? maybe-symbol kt kf)
       (let-syntax
           ((test
             (syntax-rules ()
               ((test maybe-symbol t f) t)
               ((test x t f) f))))
         (test abracadabra kt kf)))))

  ;;! A macro-expand-time memv function for identifiers
  ;;  id-memv?? FORM (ID ...) KT KF
  ;; FORM is an arbitrary form or datum, ID is an identifier.
  ;; The macro expands into KT if FORM is an identifier, which occurs
  ;; in the list of identifiers supplied by the second argument.
  ;; All the identifiers in that list must be unique.
  ;; Otherwise, id-memv?? expands to KF.
  ;; Two identifiers match if both refer to the same binding occurrence, or
  ;; (both are undefined and have the same spelling).
  ;; (id-memv?? x (a b c) #t #f)
  ;; (id-memv?? a (a b c) 'OK #f)
  ;; (id-memv?? () (a b c) #t #f)
  ;; (id-memv?? (x ...) (a b c) #t #f)
  ;; (id-memv?? "abc" (a b c) #t #f)
  ;; (id-memv?? x () #t #f)
  ;; (let ((x 1))
  ;;   (id-memv?? x (a b x) 'OK #f))
  ;; (let ((x 1))
  ;;   (id-memv?? x (a x b) 'OK #f))
  ;; (let ((x 1))
  ;;   (id-memv?? x (x a b) 'OK #f))
  (define-syntax id-memv??
    (syntax-rules ()
      ((id-memv?? form (id ...) kt kf)
       (let-syntax
           ((test
             (syntax-rules (id ...)
               ((test id _kt _kf) _kt) ...
               ((test otherwise _kt _kf) _kf))))
         (test form kt kf)))))

  ;;! Check if two identifiers are two occurrences of the same identifier
  ;; For the macro id-eq??, two identifiers are equivalent if only if they
  ;; have the same color, or to put it differently, are two occurrences of
  ;; the same identifier. In other words, the two identifiers must be
  ;; inter-changeable at macro-expand time. This is the strictest notion of
  ;; equivalence.
  ;; By Oleg Kiselyov
  (define-syntax id-eq??
    (syntax-rules ()
      ((id-eq?? id b kt kf)
       (let-syntax
           ((id (syntax-rules ()
                  ((id) kf)))
            (ok (syntax-rules ()
                  ((ok) kt))))
         (let-syntax
             ((test (syntax-rules ()
                      ((_ b) (id)))))
           (test ok))))))

  ;;! Check if two identifiers refer to the same binding
  ;; For a macro id-eqv??, the identifiers are equivalent if
  ;; they refer to the same binding (or both identifiers are unbound and
  ;; have the same spelling). Thus macro id-eqv?? can find two identifiers
  ;; equivalent even if they have different colors. The last two test cases
  ;; show the distinction.
  ;; By Oleg Kiselyov
  (define-syntax id-eqv??
    (syntax-rules ()
      ((id-eqv?? a b kt kf)
       (let-syntax
           ((test (syntax-rules (a)
                    ((test a) kt)
                    ((test x) kf))))
         (test b)))))

  ;;! Macro-lambda: A notation for a first-class parameterized future macro-expansion action
  ;; The ??!lambda form is first class: it can be passed to and
  ;; returned from R5RS macros, and can be nested.
  ;; The ??!lambda form is interpreted by a macro ??!apply.
  ;; To be more precise, we specify that the following macro-application
  ;; (??!apply (??!lambda (bound-var ...) body) arg ...)
  ;; expands to body, with all non-shadowed instances of ??! and bound-var
  ;; hygienically replaced by arg.
  ;; A ??!lambda form can have one or several bound variables; the
  ;; corresponding ??!apply form should have just as many arguments.
  ;;
  ;; Examples:
  ;; (??!apply (??!lambda (x) '(a . b)) foo)
  ;; ;===evals-to===> '(a . b)
  ;; (??!apply (??!lambda (x)
  ;;   (list (??!apply (??!lambda (x) (list '(??! x) 5 '(??! x))) (1 2))
  ;;        '(??! x))) (3 4))
  ;; ;===expands-to===> (list (list '(1 2) 5 '(1 2)) '(3 4))
  ;; ;===evals-to===> '(((1 2) 5 (1 2)) (3 4))
  ;; (??!apply (??!lambda (x) (let (((??! x) 1)) (??! x))) foo)
  ;; ;===expands-to===> ((lambda (foo) foo) 1)
  ;; ;===evals-to===> 1
  ;; (??!apply (??!lambda (x k) (??!apply (??! k) (+ 1 (??! x))))
  ;;    4
  ;;    (??!lambda (x) (display (??! x))))
  ;; ;===evals-to===> 5
  ;;
  ;; Additional notes
  ;; When a template contains an identifier that does not appear in the pattern,
  ;; it is called "colored". For example,
  ;; (define-syntax tid-1
  ;;   (syntax-rules ()
  ;;     ((_) (a))))
  ;; (tid-1) ;;=> a
  ;; (define-syntax tid-2
  ;;   (syntax-rules ()
  ;;     ((_ a) (a))))
  ;; (tid-2 a) ;==> a
  ;; (tid-2 tid-1)
  ;; The most challenging was understanding of the difference
  ;; between tid-3 and tid-31.
  ;; (define-syntax tid-3
  ;;   (syntax-rules ()
  ;;     ((_ x)
  ;;      (let-syntax
  ;;        ((foo
  ;;   (syntax-rules ()
  ;;     ((_ y) (lambda (x) y)))))
  ;;        (foo x)))))
  ;; (tid-3 a) ; ==> (lambda (a~2~3) a)
  ;; (define-syntax tid-31
  ;;   (syntax-rules ()
  ;;     ((_ x)
  ;;      (let-syntax
  ;;        ((foo
  ;;   (syntax-rules ()
  ;;     ((_ x y) (lambda (x) y)))))
  ;;        (foo x x)))))
  ;; (tid-31 a) ;==> (lambda (a~3) a~3)
  ;; (define-syntax tid-4
  ;;   (syntax-rules ()
  ;;     ((_ x) (x))
  ;;     ((_) (tid-4 a))))
  ;; (tid-4 a) ;==> a
  ;; By Oleg Kiselyov

  (define-syntax ??!apply
    (syntax-rules (??!lambda)
      ((_ (??!lambda (bound-var . other-bound-vars) body)
          oval . other-ovals)
       (letrec-syntax
           ((subs
             (syntax-rules (??! bound-var ??!lambda)
               ((_ val k (??! bound-var))
                (appl k val))
                                        ; check if bound-var is shadowed in int-body
               ((_ val k (??!lambda bvars int-body))
                (subs-in-lambda val bvars (k bvars) int-body))
               ((_ val k (x)) ; optimize single-elem list substitution
                (subs val (recon-pair val k ()) x))
               ((_ val k (x . y))
                (subs val (subsed-cdr val k x) y))
               ((_ val k x) ; x is an id other than bound-var, or number&c
                (appl k x))))
            (subsed-cdr     ; we've done the subs in the cdr of a pair
             (syntax-rules ()           ; now do the subs in the car
               ((_ val k x new-y)
                (subs val (recon-pair val k new-y) x))))
            (recon-pair ; reconstruct the pair out of substituted comp
             (syntax-rules ()
               ((_ val k new-y new-x)
                (appl k (new-x . new-y)))))
            (subs-in-lambda        ; substitute inside the lambda form
             (syntax-rules (bound-var)
               ((_ val () kp  int-body)
                (subs val (recon-l kp ()) int-body))
                                        ; bound-var is shadowed in the int-body: no subs
               ((_ val (bound-var . obvars) (k bvars) int-body)
                (appl k (??!lambda bvars int-body)))
               ((_ val (obvar . obvars) kp int-body)
                (subs-in-lambda val obvars kp int-body))))
            (recon-l    ; reconstruct lambda from the substituted body
             (syntax-rules ()
               ((_ (k bvars) () result)
                (appl k (??!lambda bvars result)))))
            (appl                     ; apply the continuation
             (syntax-rules ()         ; add the result to the end of k
               ((_ (a b c d) result)
                (a b c d result))
               ((_ (a b c) result)
                (a b c result))))
            (finish
             (syntax-rules ()
               ((_ () () exp)
                exp)
               ((_ rem-bvars rem-ovals exps)
                (??!apply (??!lambda rem-bvars exps) . rem-ovals)))))
         ;; In the following, finish is the continuation...
         (subs oval (finish other-bound-vars other-ovals) body)))))


  ;;-------------------------------------------------------------------------------
  ;; CPS macros

  ;;! Identity macro
  (define-syntax k!id
    (syntax-rules ()
      ((k!id x)
       x)))

  ;;! k!reverse ACC (FORM ...) K
  ;; reverses the second argument, appends it to the first and passes the result to K
  ;; (k!reverse () (1 2 () (4 5)) '!) ;==> '((4 5) () 2 1)
  ;; (k!reverse (x) (1 2 () (4 5)) '!) ;==> '((4 5) () 2 1 x)
  ;; (k!reverse (x) () '!) ;==> '(x)
  (define-syntax k!reverse
    (syntax-rules (!)
      ((k!reverse acc () (k-head ! . k-args))
       (k-head acc . k-args))
      ((k!reverse acc (x . rest) k)
       (k!reverse (x . acc) rest k))))

  
  ;;-------------------------------------------------------------------------------
  ;;!! Misc

  ;;! Macro tracer
  ;; Given your macro:
  ;; (define-syntax test
  ;;   (syntax-rules ()
  ;;     ((test name a1 ...) 
  ;;      (define (name a1 ...) (apply * (list a1 ...))))))
  ;; we re-write it as follows:
  ;; (define-syntax test
  ;;   (syntax-rules ()
  ;;     ((test name a1 ...) 
  ;;      (mtrace 
  ;;       (define (name a1 ...) (apply * (list a1 ...)))))))
  ;; that is, just enclose the template of the clause of interest in mtrace
  ;; Here's a more interesting example, of a tracing of a recursive macro,
  ;; letrec -- taken verbatim from R5RS. We renamed it into rletrec, to
  ;; avoid name clashes:
  ;; (define-syntax rletrec1
  ;;   (syntax-rules ()
  ;;     ((_ ((var1 init1) ...) body ...)
  ;;      (rletrec "generate temp names"
  ;;        (var1 ...)
  ;;        ()
  ;;        ((var1 init1) ...)
  ;;        body ...))
  ;;     ((_ "generate temp names"
  ;;        ()
  ;;        (temp1 ...)
  ;;        ((var1 init1) ...)
  ;;        body ...)
  ;;      (let ((var1 #f) ...)
  ;;        (let ((temp1 init1) ...)
  ;;          (set! var1 temp1)
  ;;          ...
  ;;          body ...)))
  ;;     ((_ "generate temp names"
  ;;        (x y ...)
  ;;        (temp ...)
  ;;        ((var1 init1) ...)
  ;;        body ...)
  ;;      (rletrec "generate temp names"
  ;;        (y ...)
  ;;        (newtemp temp ...)
  ;;        ((var1 init1) ...)
  ;;        body ...))))
  ;; (define-syntax rletrec
  ;;   (syntax-rules ()
  ;;     ((rletrec . args)
  ;;      (mtrace (rletrec1 . args)))))
  (define-syntax macro-trace
    (syntax-rules ()
      ((_ x)
       (begin 
         (display "Trace: ") (write 'x) (newline)
         x)))))

