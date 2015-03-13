;;!!! Base extensions for Scheme Spheres
;; .author Alvaro Castro-Castilla, 2012-2104. All rights reserved.

(define-library (spheres/core base)
  (export letrec*
          and-let*
          let-rest
          receive
          let-values
          let*-values
          case-lambda
          rec
          cond
          case
          ++!
          ++
          --!
          --!
          aif
          when
          unless
          begin0
          push!
          string-null?
          pv
          ps
          let/cc
          dotimes
          symbol->keyword
          keyword->symbol
          ->string
          ->symbol
          ->keyword
          string-append-anything
          symbol-append
          uncons
          uncons-2
          uncons-3
          uncons-4
          uncons-cons
          unlist
          unvector
          list->values
          vector->values
          values->list
          values->vector
          values->length
          values-ref
          pred2?+
          eq?+
          eqv?+
          equal?+
          type-of
          coerce
          ->integer)

  ;;------------------------------------------------------------------------------
  ;;!! R5RS standard Macros

  ;; (define-syntax do
  ;;   (syntax-rules ()
  ;;     ((do ((var init step ...) ...)
  ;;          (test expr ...)
  ;;        command ...)
  ;;      (letrec
  ;;          ((loop
  ;;            (lambda (var ...)
  ;;              (if test
  ;;                  (begin
  ;;                    (if #f #f)
  ;;                    expr ...)
  ;;                  (begin
  ;;                    command
  ;;                    ...
  ;;                    (loop (do "step" var step ...)
  ;;                          ...))))))
  ;;        (loop init ...)))
  ;;     ((do "step" x)
  ;;      x)
  ;;     ((do "step" x y)
  ;;      y)))

  ;;! Equivalent to let, with a late set! binding
  ;; Similar to letrec, except the init expressions are bound to their variables in order.
  ;; letrec* thus relaxes the letrec restriction, in that later init expressions may refer to the
  ;; values of previously bound variables.
  (define-syntax letrec*
    (syntax-rules ()
      ((_ ((?binding ?val) ...) ?body1 ?body2 ...)
       (let ((?binding #!unbound) ...)
         (begin (set! ?binding ?val) ... ?body1 ?body2 ...)))))

  ;;------------------------------------------------------------------------------
  ;;!! SRFIs

  ;;! SRFI-2 AND-LET*: an AND with local bindings, a guarded LET* special form
  ;; .author Álvaro Castro-Castilla (Ported to syntax-rules)
  (define-syntax and-let*
    (syntax-rules ()
      ((_ ())
       #t)
      ((_ () ?body ...)
       (begin ?body ...))
      ((_ ((?expr)))
       ?expr)
      ((_ ((?var ?expr)))
       ?expr)
      ((_ (?expr))
       ?expr)
      ((_ ((?expr) ?clauses ...))
       (let ((var ?expr))
         (if var (and-let* (?clauses ...)) var)))
      ((_ ((?var ?expr) ?clauses ...))
       (let ((?var ?expr))
         (if ?var (and-let* (?clauses ...)) ?var)))
      ((_ ((?var ?expr) ?clauses ...) ?body ...)
       (let ((?var ?expr))
         (if ?var (and-let* (?clauses ...) ?body ...) #f)))
      ((_ ((?expr) ?clauses ...) ?body ...)
       (if ?expr (and-let* (?clauses ...) ?body ...) #f))
      ((_ (?var ?clauses ...) ?body ...)
       (if ?var (and-let* (?clauses ...) ?body ...) #f))))


  ;;!! SRFI-5 A compatible let form with signatures and rest arguments
  ;; Copyright (C) Andy Gaynor (1999). All Rights Reserved.
  ;; Modifications
  ;; - Álvaro Castro-Castilla: rewritten let-loop as a local syntax definition
  (define-syntax let-rest
    (let-syntax
        ((let-loop
          (syntax-rules ()
            ;; Standard binding: destructure and loop.
            ((_ name ((var0 val0) binding ...) (var ...     ) (val ...     ) body)
             (let-loop name (            binding ...) (var ... var0) (val ... val0) body))
            ;; Rest binding, no name: use standard-let, listing the rest values.
            ;; Because of let's first clause, there is no "no bindings, no name" clause.
            ((_ #f (rest-var rest-val ...) (var ...) (val ...) body)
             (let ((var val) ... (rest-var (list rest-val ...))) . body))
            ;; Or call a lambda with a rest parameter on all values.
            ;; ((lambda (var ... . rest-var) . body) val ... rest-val ...))
            ;; Or use one of several other reasonable alternatives.
            ;; No bindings, name: call a letrec'ed lambda.
            ((_ name () (var ...) (val ...) body)
             ((letrec ((name (lambda (var ...) . body)))
                name)
              val ...))
            ;; Rest binding, name: call a letrec'ed lambda.
            ((_ name (rest-var rest-val ...) (var ...) (val ...) body)
             ((letrec ((name (lambda (var ... . rest-var) . body)))
                name)
              val ... rest-val ...)))))
      (syntax-rules ()
        ;; No bindings: use standard-let.
        ((_ () body ...)
         (let () body ...))
        ;; Or call a lambda.
        ;; ((lambda () body ...))
        ;; All standard bindings: use standard-let.
        ((_ ((var val) ...) body ...)
         (let ((var val) ...) body ...))
        ;; Or call a lambda.
        ;; ((lambda (var ...) body ...) val ...)
        ;; One standard binding: loop.
        ;; The all-standard-bindings clause didn't match,
        ;; so there must be a rest binding.
        ((_ ((var val) . bindings) body ...)
         (let-loop #f bindings (var) (val) (body ...)))
        ;; Signature-style name: loop.
        ((_ (name binding ...) body ...)
         (let-loop name (binding ...) () () (body ...)))
        ;; defun-style name: loop.
        ((_ name bindings body ...)
         (let-loop name bindings () () (body ...))))))


  ;;! SRFI-8: RECEIVE
  (define-syntax receive
    (syntax-rules ()
      ((receive formals expression body ...)
       (call-with-values (lambda () expression)
         (lambda formals body ...)))))


  ;;! SRFI-11 Syntax for receiving multiple values
  ;; Copyright (C) Lars T Hansen (1999). All Rights Reserved.

  ;; Equivalent low-level macro:
  ;; (define-macro (let-values* . args)
  ;;   (let ((bindings (car args))
  ;;         (body (cadr args)))
  ;;    (if (null? bindings) (cons 'begin body)
  ;;        (apply (lambda (vars initializer)
  ;;                 (let ((cont 
  ;;                        (cons 'let-values* 
  ;;                              (cons (cdr bindings) body))))
  ;;                   (cond
  ;;                    ((not (pair? vars)) ; regular let case, a single var
  ;;                     `(let ((,vars ,initializer)) ,cont))
  ;;                    ((null? (cdr vars)) ; single var, see the prev case
  ;;                     `(let ((,(car vars) ,initializer)) ,cont))
  ;;                    (else                ; the most generic case
  ;;                     `(call-with-values (lambda () ,initializer)
  ;;                        (lambda ,vars ,cont))))))
  ;;               (car bindings)))))
  (define-syntax let-values
    (syntax-rules ()
      ((let-values (?binding ...) ?body0 ?body1 ...)
       (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
      ((let-values "bind" () ?tmps ?body)
       (let ?tmps ?body))
      ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
       (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
      ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
       (call-with-values 
           (lambda () ?e0)
         (lambda ?args
           (let-values "bind" ?bindings ?tmps ?body))))
      ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
       (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
      ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
       (call-with-values
           (lambda () ?e0)
         (lambda (?arg ... . x)
           (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))

  (define-syntax let*-values
    (syntax-rules ()
      ((let*-values () ?body0 ?body1 ...)
       (begin ?body0 ?body1 ...))
      ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
       (let-values (?binding0)
         (let*-values (?binding1 ...) ?body0 ?body1 ...)))))


  ;;! SRFI-16 Syntax for procedures of variable arity
  ;; Copyright (C) Lars T Hansen (1999). All Rights Reserved.
  ;; This code is in the public domain.
  (define-syntax case-lambda
    (syntax-rules ()
      ((case-lambda 
        (?a1 ?e1 ...) 
        ?clause1 ...)
       (lambda args
         (let ((l (length args)))
           (case-lambda "CLAUSE" args l 
                        (?a1 ?e1 ...)
                        ?clause1 ...))))
      ((case-lambda "CLAUSE" ?args ?l 
                    ((?a1 ...) ?e1 ...) 
                    ?clause1 ...)
       (if (= ?l (length '(?a1 ...)))
           (apply (lambda (?a1 ...) ?e1 ...) ?args)
           (case-lambda "CLAUSE" ?args ?l 
                        ?clause1 ...)))
      ((case-lambda "CLAUSE" ?args ?l
                    ((?a1 . ?ar) ?e1 ...) 
                    ?clause1 ...)
       (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...) 
                    ?clause1 ...))
      ((case-lambda "CLAUSE" ?args ?l 
                    (?a1 ?e1 ...)
                    ?clause1 ...)
       (let ((?a1 ?args))
         ?e1 ...))
      ((case-lambda "CLAUSE" ?args ?l)
       (error "Wrong number of arguments to CASE-LAMBDA."))
      ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
                    ?clause1 ...)
       (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...) 
                    ?clause1 ...))
      ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...) 
                    ?clause1 ...)
       (if (>= ?l ?k)
           (apply (lambda ?al ?e1 ...) ?args)
           (case-lambda "CLAUSE" ?args ?l 
                        ?clause1 ...)))))

  ;;! SRFI-31 A special form rec for recursive evaluation
  ;; Copyright (C) Dr. Mirko Luedde (2002). All Rights Reserved.
  (define-syntax rec
    (syntax-rules ()
      ((rec (?name . ?variables) . ?body)
       (letrec ((?name (lambda ?variables . ?body))) ?name))
      ((rec ?name ?expression)
       (letrec ((?name ?expression)) ?name))))

  ;;! SRFI-61 A more general cond clause
  (define-syntax $$cond/maybe-more
    (syntax-rules ()
      ((_ test consequent)
       (if test
           consequent))
      ((_ test consequent clause ...)
       (if test
           consequent
           (cond clause ...)))))
  (define-syntax cond
    (syntax-rules (=> else)
      ((_ (else else1 else2 ...))
       ;; the (if #t (begin ...)) wrapper ensures that there may be no
       ;; internal definitions in the body of the clause.  R5RS mandates
       ;; this in text (by referring to each subform of the clauses as
       ;; <expression>) but not in its reference implementation of cond,
       ;; which just expands to (begin ...) with no (if #t ...) wrapper.
       (if #t (begin else1 else2 ...)))
      ((_ (test => receiver) more-clause ...)
       (let ((t test))
         ($$cond/maybe-more t
                            (receiver t)
                            more-clause ...)))
      ((_ (generator guard => receiver) more-clause ...)
       (call-with-values (lambda () generator)
         (lambda t
           ($$cond/maybe-more (apply guard    t)
                              (apply receiver t)
                              more-clause ...))))
      ((_ (test) more-clause ...)
       (let ((t test))
         ($$cond/maybe-more t t more-clause ...)))
      ((_ (test body1 body2 ...) more-clause ...)
       ($$cond/maybe-more test
                          (begin body1 body2 ...)
                          more-clause ...))))

  ;;! SRFI-87 => in case clauses
  ;; Included in Alexpander for native availability

  (define-syntax case
    (syntax-rules (else =>)
      ((case (key ...)
         clauses ...)
       (let ((atom-key (key ...)))
         (case atom-key clauses ...)))
      ((case key
         (else => result))
       (result key))
      ((case key
         ((atoms ...) => result))
       (if (memv key '(atoms ...))
           (result key)))
      ((case key
         ((atoms ...) => result)
         clause clauses ...)
       (if (memv key '(atoms ...))
           (result key)
           (case key clause clauses ...)))
      ((case key
         (else result1 result2 ...))
       (begin result1 result2 ...))
      ((case key
         ((atoms ...) result1 result2 ...))
       (if (memv key '(atoms ...))
           (begin result1 result2 ...)))
      ((case key
         ((atoms ...) result1 result2 ...)
         clause clauses ...)
       (if (memv key '(atoms ...))
           (begin result1 result2 ...)
           (case key clause clauses ...)))))

  ;;------------------------------------------------------------------------------
  ;;!! Non-standard Macros

  ;;! Mutable increment
  ;; Equivalent low-level macro:
  ;; (define-macro (++! x) `(set! ,x (fx+ 1 ,x)))
  (define-syntax ++!
    (syntax-rules ()
      ((_ x)
       (set! x (+ 1 x)))))

  ;;! Read-only increment
  ;; Equivalent low-level macro:
  ;; (define-macro (++ x) `(fx+ 1 ,x))
  (define-syntax ++
    (syntax-rules ()
      ((_ x)
       (+ 1 x))))

  ;;! Mutable decrement
  ;; Equivalent low-level macro:
  ;; (define-macro (--! x) `(set! ,x (fx- ,x 1)))
  (define-syntax --!
    (syntax-rules ()
      ((_ x)
       (set! x (- x 1)))))

  ;;! Read-only decrement
  ;; Equivalent low-level macro:
  ;; (define-macro (-- x) `(fx- ,x 1))
  (define-syntax --
    (syntax-rules ()
      ((_ x)
       (- x 1))))

  ;;! Hygienic anaphoric if
  ;; Equivalent low-level macro:
  ;; (##define-macro (aif . args)
  ;;   (let ((it (car args))
  ;;         (arg1 (cadr args))
  ;;         (rest-args (cddr args)))
  ;;    (case (length rest-args)
  ;;      ((1)
  ;;       `(let ((,it ,arg1))
  ;;          (if ,it
  ;;              ,(car rest-args)
  ;;              #f)))
  ;;      ((2)
  ;;       `(let ((,it ,arg1))
  ;;          (if ,it
  ;;              ,(car rest-args)
  ;;              ,(cadr rest-args))))
  ;;      ((3)
  ;;       `(let ((,it ,(car rest-args)))
  ;;          (if ,(arg1 ,it)
  ;;              (cadr rest-args)
  ;;              (caddr rest-args))))
  ;;      (else
  ;;       (error "Too many arguments passed to unhygienic anaphoric if")))))
  (define-syntax aif
    (syntax-rules ()
      ((_ var expr iftrue)
       (let ((var expr))
         (if var
             iftrue
             #f)))
      ((_ var expr iftrue iffalse)
       (let ((var expr))
         (if var
             iftrue
             iffalse)))
      ((_ var pred expr iftrue iffalse)
       (let ((var expr))
         (if (pred var)
             iftrue
             iffalse)))))

  ;;! Anaphoric if (unhygienic)
  ;; Note: the caveat that it can't handle single symbol consequents
  ;; (easy to fix... add two rules to the four argument part of %subst)
  ;; .author pelpel
  ;; origin: http://c2.com/cgi/wiki?DefineSyntax
  ;; (define-syntax aif!!
  ;;   (letrec-syntax
  ;;       ((%reverse
  ;;         (syntax-rules ()
  ;;           ((_ () <result>) <result>)
  ;;           ((_ (<hd> . <tl>) <result>)
  ;;            (%reverse <tl> (<hd> . <result>)))))
  ;;        (%subst  
  ;;         (syntax-rules ()
  ;;           ;; 1. Three argument form: substitute <new> for all occurrences of <old>
  ;;           ;; in <form> 
  ;;           ((_ <new> <old> <form>)
  ;;            (letrec-syntax
  ;;                ((f (syntax-rules (<old>)
  ;;                      ;; (1) Substitution complete, reverse the result.
  ;;                      ((_ () <result>) (%reverse <result> ()))
  ;;                      ;; (2) recurse into sublists (deferred)
  ;;                      ((_ ((<hd> . <tl>) . <rest>) <res>)
  ;;                       (f <rest> ((f (<hd> . <tl>) ()) . <res>)))
  ;;                      ;; (3) These two rules does (substitute <new> <old> ls)
  ;;                      ((_ (<old> . <tl>) <res>)
  ;;                       (f <tl> (<new> . <res>)))
  ;;                      ((_ (<hd> . <tl>) <res>)
  ;;                       (f <tl> (<hd> . <res>))))))
  ;;              (f <form> ())))
  ;;           ;; 2. Four argument form: substitute <new> for all occurrences of <old> in
  ;;           ;; <form> but those inside of sublists (<but> ...).  Useful for defining
  ;;           ;; macros that can be nested.
  ;;           ((_ <new> <old> <form> <but>)
  ;;            (letrec-syntax
  ;;                ((f (syntax-rules (<old> <but>)
  ;;                      ((_ () <result>) (%reverse <result> ()))
  ;;                      ;; (4) ignore (<but> ...)
  ;;                      ((_ ((<but> . <tl>) . <rest>) <res>)
  ;;                       (f <rest> ((<but> . <tl>) . <res>)))
  ;;                      ((_ ((<hd> . <tl>) . <rest>) <res>)
  ;;                       (f <rest> ((f (<hd> . <tl>) ()) . <res>)))
  ;;                      ((_ (<old> . <tl>) <res>)
  ;;                       (f <tl> (<new> . <res>)))
  ;;                      ((_ (<hd> . <tl>) <res>)
  ;;                       (f <tl> (<hd> . <res>))))))
  ;;              (f <form> ()))))))
  ;;     (syntax-rules ()
  ;;       ((_ <condition> <consequent>)
  ;;        (let ((temp <condition>))
  ;;          (if temp
  ;;              (%subst temp it <consequent> aif))))
  ;;       ((_ <condition> <consequent> <alternative>)
  ;;        (let ((temp <condition>))
  ;;          (if temp
  ;;              (%subst temp it <consequent> aif)
  ;;              <alternative>))))))


  ;;! when
  ;; R5RS standard states that an if with only one branch returns an unspecified
  ;; value if the test is false. This macro places an #f automatically
  ;; Equivalent low-level macro:
  ;; (##define-macro (when . args)
  ;;   (let ((condition (car args))
  ;;         (forms (cdr args)))
  ;;     `(and ,condition (begin ,@forms))))
  (define-syntax when
    (syntax-rules ()
      ((_ ?condition . ?stmts)
       (and ?condition (begin . ?stmts)))))

  ;;! unless
  ;; The opposite of when
  ;; Equivalent low-level macro:
  ;; (##define-macro (unless . args)
  ;;   (let ((condition (car args))
  ;;         (forms (cdr args)))
  ;;     `(or ,condition (begin ,@forms))))
  (define-syntax unless
    (syntax-rules ()
      ((_ ?test ?form . ?forms)
       (if ?test #f (begin ?form . ?forms)))))

  ;;! begin0
  ;; Execute a sequence of forms and return the result of the _first_ one.
  ;; Typically used to evaluate one or more forms with side effects and
  ;; return a value that must be computed before
  ;; Equivalent low-level macro:
  ;; (##define-macro (begin0 . args)
  ;;   (let ((form1 (car args))
  ;;         (rest-forms (cdr args))
  ;;         (var (gensym)))
  ;;     `(let ((,var ,form1)) ,@rest-forms ,var)))
  (define-syntax begin0
    (syntax-rules ()
      ((_ form form1 ... ) 
       (let ((val form)) form1 ... val))))

  ;;! push!
  ;; Prepend an ITEM to a LIST, like a Lisp macro PUSH an ITEM can be an
  ;; expression, but ls must be a VAR
  ;; Equivalent low-level macro:
  ;; (##define-macro (push! list obj)
  ;;   `(set! ,list (cons ,obj ,list)))
  (define-syntax push!
    (syntax-rules ()
      ((_ item ls)
       (set! ls (cons item ls)))))

  ;;! string-null?
  ;; Equivalent low-level macro:
  ;; (##define-macro (string-null? str) `(zero? (string-length ,str)))
  (define-syntax string-null?
    (syntax-rules ()
      ((_ str)
       (zero? (string-length str)))))

  ;;! Pretty-print for values, returning values too
  ;; Equivalent low-level macro:
  ;; (##define-macro (pv form)
  ;;   `(call-with-values
  ;;        (lambda () ,form)
  ;;      (lambda args
  ;;        (for-each pp args)
  ;;        (apply values args))))
  (define-syntax pv
    (syntax-rules ()
      ((_ ?form)
       (call-with-values
           (lambda () ?form)
         (lambda args
           (for-each pp args)
           (apply values args))))))

  ;;! Pretty-print for values, pause execution after (for debugging)
  ;; Equivalent low-level macro:
  ;; (##define-macro (ps form)
  ;;   `(call-with-values
  ;;        (lambda () ,form)
  ;;      (lambda args
  ;;        (for-each pp args)
  ;;        (step)
  ;;        (apply values args))))
  (define-syntax ps
    (syntax-rules ()
      ((_ ?form)
       (call-with-values
           (lambda () ?form)
         (lambda args
           (for-each pp args)
           (step)
           (apply values args))))))

  ;;! Letcc macro (hoping and skipping)
  ;; (##define-macro (let/cc . args)
  ;;   `(call-with-current-continuation
  ;;     (lambda (,(car args)) ,@(cdr args))))
  (define-syntax let/cc
    (syntax-rules ()
      ((_ c . body)
       (call-with-current-continuation
        (lambda (c) . body)))))

  ;;! Do a fixed number of times
  (define-syntax dotimes
    (syntax-rules ()
      ((_ (var n res) . body)
       (do ((limit n)
            (var 0 (+ var 1)))
           ((>= var limit) res)
         . body))
      ((_ (var n) . body)
       (do ((limit n)
            (var 0 (+ var 1)))
           ((>= var limit))
         . body))))


  ;;------------------------------------------------------------------------------
  ;;!! Basic types conversion

  ;;! symbol->keyword
  (define-syntax symbol->keyword
    (syntax-rules ()
      ((_ s)
       (string->keyword (symbol->string s)))))

  ;;! keyword->symbol
  (define-syntax keyword->symbol
    (syntax-rules ()
      ((_ k)
       (string->symbol (keyword->string k)))))

  ;;! Anything to string
  (define-syntax ->string
    (syntax-rules ()
      ((_ o)
       (cond ((string? o) o)
             ((symbol? o) (symbol->string o))
             ((keyword? o) (keyword->string o))
             (else (object->string o))))))

  ;;! Anything to symbol
  (define-syntax ->symbol
    (syntax-rules ()
      ((_ o)
       (string->symbol (->string o)))))

  ;;! Anything to keyword
  (define-syntax ->keyword
    (syntax-rules ()
      ((_ o)
       (string->keyword (->string o)))))

  ;;! Build a string from list of elements (anything)
  (define-syntax string-append-anything
    (syntax-rules ()
      ((_ . ol)
       (apply string-append (map (lambda (e) (->string e)) (list . ol))))))

  ;;! Append anything into a symbol
  (define-syntax symbol-append
    (syntax-rules ()
      ((_ . ol)
       (string->symbol (string-append-anything . ol)))))

  
  ;;-----------------------------------------------------------------------------
  ;;!! Multiple values

  ;; List to values (as syntax)
  (define-syntax list->values
    (syntax-rules ()
      ((_ x)
       (apply values x))))

  ;; Vector to values (as syntax)
  (define-syntax vector->values
    (syntax-rules ()
      ((_ x)
       (apply values (vector->list x)))))
    
  ;;! Values to list
  (define-syntax values->list
    (syntax-rules ()
      ((_ x)
       (call-with-values (lambda () x) list))))

  ;;! Values to vector
  (define-syntax values->vector
    (syntax-rules ()
      ((_ x)
       (call-with-values (lambda () x) vector))))

  ;;! Number of values produced
  (define-syntax values-length
    (syntax-rules ()
      ((_ producer)
       (call-with-values
           (lambda () producer)
         (lambda v (length v))))))

  ;;! Extract only the nth-value from a function returning multiple values
  (define-syntax values-ref
    (syntax-rules ()
      ((_ n producer)
       (call-with-values
           (lambda () producer)
         (lambda v (list-ref v n))))))

  ;;! All values pairs must satisfy the given 2-predicate
  (define-syntax pred2?+
    (syntax-rules ()
      ((_ ?pred ?a ?b)
       (let ((la (values->list ?a))
             (lb (values->list ?b)))
         (let recur ((la la)
                     (lb lb))
           (cond
            ((null? la) (if (null? lb) #t #f))
            ((null? lb) (if (null? la) #t #f))
            (else
             (and (?pred (car la) (car lb))
                  (recur (cdr la)
                         (cdr lb))))))))))

  ;;! All values pairs must satisfy eq?
  (define-syntax eq?+
    (syntax-rules ()
      ((_ ?a ?b)
       (pred2?+ eq? ?a ?b))))

  ;;! All values pairs must satisfy eqv?
  (define-syntax eqv?+
    (syntax-rules ()
      ((_ ?a ?b)
       (pred2?+ eqv? ?a ?b))))

  ;;! All values pairs must satisfy equal?
  (define-syntax equal?+
    (syntax-rules ()
      ((_ ?a ?b)
       (pred2?+ equal? ?a ?b))))
  
  (include "base.scm"))
