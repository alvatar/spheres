;;!!! SchemeSpheres prelude for Gambit
;; .author Alvaro Castro-Castilla, 2014-2015. See LICENSE file.


;;------------------------------------------------------------------------------
;; Overriding INCLUDE

(define^ (expander:include file)
  (for-each eval (with-input-from-file file read-all)))

(eval '(define-syntax include
         (syntax-rules ()
           ((_ ?file)
            (for-each eval (with-input-from-file ?file read-all))))))

(define load-procedures load)

(define load
  (let ((gambit-load load))
    (lambda (file)
      (cond ((not (string? file))
             (%load-library file))
            ((string=? ".scm" (path-extension file))
             (expander:include file))
            ((string=? ".sld" (path-extension file))
             (%load-library (cadar (with-input-from-file file read-all))))
            (else
             (gambit-load file))))))

;;------------------------------------------------------------------------------
;; Low-level macros

;;! DEFINE-MACRO in terms of syntax-case
(define-syntax (define-macro x)
  (syntax-case x ()
    ((_ (name . args) . body)
     #'(define-macro name (lambda args . body)))
    ((_ name transformer)
     #'(define-syntax (name y)
         (syntax-case y ()
           ((k . args)
            (datum->syntax
             #'k
             (apply transformer (syntax->datum #'args)))))))))

;;! SRFI-0: Feature-based conditional expansion construct
;; .author Álvaro Castro-Castilla (Adapted to dynamically add new features)
(define-macro add-cond-expand-feature!
  (lambda (new-feature)
    (let ((features (cons new-feature (##cond-expand-features))))
      (##cond-expand-features features)
      (for-each
       eval
       `((define-syntax syntax-rules-error
           (syntax-rules ()
             ((_) (0))))
         (define-syntax cond-expand
           (syntax-rules (and or not else ,@features)
             ((cond-expand) (syntax-rules-error "Unfulfilled cond-expand"))
             ((cond-expand (else body ...))
              (begin body ...))
             ((cond-expand ((and) body ...) more-clauses ...)
              (begin body ...))
             ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
              (cond-expand
               (req1
                (cond-expand
                 ((and req2 ...) body ...)
                 more-clauses ...))
               more-clauses ...))
             ((cond-expand ((or) body ...) more-clauses ...)
              (cond-expand more-clauses ...))
             ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
              (cond-expand
               (req1
                (begin body ...))
               (else
                (cond-expand
                 ((or req2 ...) body ...)
                 more-clauses ...))))
             ((cond-expand ((not req) body ...) more-clauses ...)
              (cond-expand
               (req
                (cond-expand more-clauses ...))
               (else body ...)))
             ,@(map
                (lambda (cef)
                  `((cond-expand (,cef body ...) more-clauses ...)
                    (begin body ...)))
                features)
             ((cond-expand (feature-id body ...) more-clauses ...)
              (cond-expand more-clauses ...)))))))))

;;!! Optional positional and named parameters in SchemeSpheres

;;! Macro expander for lambda*.
;; .author Álvaro Castro-Castilla, based on SRFI-89 by Marc Feeley
(define-macro lambda*
  (lambda (formals . body)
    (define (keyword->symbol k)
      (string->symbol (keyword->string k)))
    (define (variable? x) (symbol? x))
    (define (required-positional? x)
      (variable? x))
    (define (optional-positional? x)
      (and (pair? x)
           (pair? (cdr x))
           (null? (cddr x))
           (variable? (car x))))
    (define (named-with-default? x)
      (and (pair? x)
           (pair? (cdr x))
           (null? (cddr x))
           (keyword? (car x))))
    (define (named-without-default? x)
      (and (pair? x)
           (null? (cdr x))
           (keyword? (car x))))
    (define (named? x)
      (or (named-with-default? x)
          (named-without-default? x)))
    (define (parse-formals formals)
      (define (duplicates? lst)
        (cond ((null? lst)
               #f)
              ((memq (car lst) (cdr lst))
               #t)
              (else
               (duplicates? (cdr lst)))))
      (define (parse-positional-section lst cont)
        (let loop1 ((lst lst) (rev-reqs '()))
          (if (and (pair? lst)
                   (required-positional? (car lst)))
              (loop1 (cdr lst) (cons (car lst) rev-reqs))
              (let loop2 ((lst lst) (rev-opts '()))
                (if (and (pair? lst)
                         (optional-positional? (car lst)))
                    (loop2 (cdr lst) (cons (car lst) rev-opts))
                    (cont lst (cons (reverse rev-reqs) (reverse rev-opts))))))))
      (define (parse-named-section lst cont)
        (let loop ((lst lst) (rev-named '()))
          (if (and (pair? lst)
                   (named? (car lst)))
              (loop (cdr lst) (cons (car lst) rev-named))
              (cont lst (reverse rev-named)))))
      (define (parse-rest lst
                          positional-before-named?
                          positional-reqs/opts
                          named)
        (if (null? lst)
            (parse-end positional-before-named?
                       positional-reqs/opts
                       named
                       #f)
            (if (variable? lst)
                (parse-end positional-before-named?
                           positional-reqs/opts
                           named
                           lst)
                (error "syntax error in formal parameter list"))))
      (define (parse-end positional-before-named?
                         positional-reqs/opts
                         named
                         rest)
        (let ((positional-reqs (car positional-reqs/opts))
              (positional-opts (cdr positional-reqs/opts)))
          (let ((vars
                 (append positional-reqs
                         (map car positional-opts)
                         (map car named)
                         (if rest (list rest) '())))
                (keys
                 (map car named)))
            (cond ((duplicates? vars)
                   (error "duplicate variable in formal parameter list"))
                  ((duplicates? keys)
                   (error "duplicate keyword in formal parameter list"))
                  (else
                   (list positional-before-named?
                         positional-reqs
                         positional-opts
                         named
                         rest))))))

      (define (parse lst)
        (if (and (pair? lst)
                 (named? (car lst)))
            (parse-named-section
             lst
             (lambda (lst named)
               (parse-positional-section
                lst
                (lambda (lst positional-reqs/opts)
                  (parse-rest lst
                              #f
                              positional-reqs/opts
                              named)))))
            (parse-positional-section
             lst
             (lambda (lst positional-reqs/opts)
               (parse-named-section
                lst
                (lambda (lst named)
                  (parse-rest lst
                              #t
                              positional-reqs/opts
                              named)))))))
      (parse formals))
    (define (expand-lambda* formals body)
      (define (range lo hi)
        (if (< lo hi)
            (cons lo (range (+ lo 1) hi))
            '()))
      (define (expand positional-before-named?
                      positional-reqs
                      positional-opts
                      named
                      rest)
        (let ((form
               `(##lambda (,@positional-reqs
                           ,@(if (null? positional-opts)
                                 '()
                                 (cons '#!optional positional-opts))
                           ,@(if (null? named)
                                 '()
                                 (cons '#!key (map
                                               (lambda (x)
                                                 (cond ((named-with-default? x)
                                                        (cons (keyword->symbol (car x))
                                                              (cdr x)))
                                                       ((named-without-default? x)
                                                        (keyword->symbol (car x)))
                                                       (else "error generating named parameters")))
                                               named)))
                           ,@(if rest
                                 (list '#!rest rest)
                                 '()))
                  ;; Surrounding with LET prevents an error when you use internal defines
                  ;; just after the define*/lambda*
                  (let ()
                    ,@body))))
          ;;(pp form)
          form))
      (apply expand (parse-formals formals)))
    (expand-lambda* formals body)))

;;!! Macro expander for define*.
(define-macro define*
  (lambda (pattern . body)
    (if (pair? pattern)
        `(define ,(car pattern)
           (lambda* ,(cdr pattern) ,@body))
        `(define ,pattern ,@body))))
