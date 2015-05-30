;;!!! SchemeSpheres prelude for Gambit
;; .author Alvaro Castro-Castilla, 2014-2015. See LICENSE file.


;;------------------------------------------------------------------------------
;; Overriding INCLUDE and LOAD

(define ##show-loaded/included-files #f)

;; Provide any of this options:
;; - a file path
;; - a library definition
;; - a procedure that when run yields a file path
(define-macro (include file-or-library)
  ;; Allow building paths as a result of a procedure
  (if (procedure? file-or-library)
      (set! file-or-library (file-or-library)))
  (let ((file (cond ((string? file-or-library)
                     (if ##show-loaded/included-files
                         (println "include: " file-or-library))
                     file-or-library)
                    ((%library? file-or-library)
                     (if ##show-loaded/included-files
                         (println "include: " (object->string file-or-library)))
                     (%find-library-scm file-or-library))
                    (else
                     (error "include -- library or file required: " file-or-library)))))
    (or (file-exists? file)
        (error "include -- file not found: " file))
    (parameterize
     ((current-directory (path-directory file)))
     (let ((forms (with-input-from-file (path-strip-directory file) read-all)))
       ;; Force expand-time evaluation of loaded libraries
       (for-each
        (lambda (f)
          (if (pair? f)
              (case (car f)
                ((load)
                 (if (null? (cdr f)) (error "Wrong load syntax"))
                 `(load ,(cdr f))))))
        forms)
       `(begin ,@forms)))))

;; Provide any of this options:
;; - a file path
;; - a library definition
(define-macro load
  (lambda (file-or-library . extra)
    (cond ((string? file-or-library)
           (parameterize
            ((current-directory (path-directory file-or-library)))
            (cond ((string=? ".scm" (path-extension file-or-library))
                   (list 'include file-or-library))
                  ((string=? ".sld" (path-extension file-or-library))
                   (or (file-exists? file-or-library)
                       (error "load -- file not found: " file-or-library))
                   (apply %load-library
                          (cadar (with-input-from-file file-or-library read-all))
                          extra))
                  (else
                   (if ##show-loaded/included-files (println "load: " file-or-library))
                   `(##load ,file-or-library (lambda (_ __) #f) #t #t #f)))))
          ((%library? file-or-library)
           (parameterize
            ((current-directory (%find-library-path file-or-library)))
            (let ((output (apply %load-library file-or-library extra)))
              (cons '##begin
                    (map (lambda (x)
                           (if (eq? (car x) 'load)
                               (if (string=? ".scm" (path-extension (cadr x)))
                                   (cons 'include (cdr x))
                                   (begin
                                     (if ##show-loaded/included-files
                                         (println "load: " (object->string file-or-library)))
                                     `(##load ,(cadr x) (lambda (_ __) #f) #t #t #f)))
                               x))
                         (cdr output))))))
          (else
           (error "load -- library or file required: " file-or-library)
           #!void))))


;;------------------------------------------------------------------------------
;; Low-level macros

;;! DEFINE-MACRO in terms of syntax-case
;; PSyntax already includes this definition internally
;; (define-syntax (define-macro x)
;;   (syntax-case x ()
;;     ((_ (name . args) . body)
;;      #'(define-macro name (lambda args . body)))
;;     ((_ name transformer)
;;      #'(define-syntax (name y)
;;          (syntax-case y ()
;;            ((k . args)
;;             (datum->syntax
;;              #'k
;;              (apply transformer (syntax->datum #'args)))))))))

;;! Type generation macros, adapted to work with PSyntax. The code is
;; taken from _nonstd.scm in Gambit, and the only modification required was
;; renaming ##define-macro to define-macro
(define (%%define-type-expand
         form-name
         super-type-static
         super-type-dynamic-expr
         args)
  (define (generate
           name
           flags
           id
           extender
           constructor
           constant-constructor
           predicate
           implementer
           type-exhibitor
           prefix
           fields
           total-fields)
    (define (generate-fields)
      (let loop ((lst1 (##reverse fields))
                 (lst2 '()))
        (if (##pair? lst1)
            (let* ((field
                    (##car lst1))
                   (descr
                    (##cdr field))
                   (field-name
                    (##vector-ref descr 0))
                   (options
                    (##vector-ref descr 4))
                   (attributes
                    (##vector-ref descr 5))
                   (init
                    (cond ((##assq 'init: attributes)
                           =>
                           (lambda (x) (##constant-expression-value (##cdr x))))
                          (else
                           #f))))
              (loop (##cdr lst1)
                    (##cons field-name
                            (##cons options
                                    (##cons init
                                            lst2)))))
            (##list->vector lst2))))
    (define (all-fields->rev-field-alist all-fields)
      (let loop ((i 1)
                 (lst all-fields)
                 (rev-field-alist '()))
        (if (##pair? lst)
            (let* ((field-name
                    (##car lst))
                   (rest1
                    (##cdr lst))
                   (options
                    (##car rest1))
                   (rest2
                    (##cdr rest1))
                   (val
                    (##car rest2))
                   (rest3
                    (##cdr rest2)))
              (loop (##fx+ i 1)
                    rest3
                    (##cons (##cons field-name
                                    (##vector i
                                              options
                                              val
                                              (generate-parameter i)))
                            rev-field-alist)))
            rev-field-alist)))
    (define (generate-parameter i)
      (##string->symbol
       (##string-append "p"
                        (##number->string i 10))))
    (define (generate-parameters rev-field-alist)
      (if (##pair? constructor)
          (##map (lambda (field-name)
                   (let ((x (##assq field-name rev-field-alist)))
                     (##vector-ref (##cdr x) 3)))
                 (##cdr constructor))
          (let loop ((lst rev-field-alist)
                     (parameters '()))
            (if (##pair? lst)
                (let ((x (##car lst)))
                  (loop (##cdr lst)
                        (let* ((options
                                (##vector-ref (##cdr x) 1))
                               (has-init?
                                (##not (##fx= (##fxand options 8)
                                              0))))
                          (if has-init?
                              parameters
                              (##cons (##vector-ref (##cdr x) 3)
                                      parameters)))))
                parameters))))
    (define (generate-initializations field-alist parameters in-macro?)
      (##map (lambda (x)
               (let* ((field-index (##vector-ref (##cdr x) 0))
                      (options (##vector-ref (##cdr x) 1))
                      (val (##vector-ref (##cdr x) 2))
                      (parameter (##vector-ref (##cdr x) 3)))
                 (if (##memq parameter parameters)
                     parameter
                     (make-quote
                      (if in-macro?
                          (make-quote val)
                          val)))))
             field-alist))
    (define (make-quote x)
      (##list 'quote x))
    (let* ((macros?
            (##not (##fx= (##fxand flags 4) 0)))
           (generative?
            (##not id))
           (augmented-id-str
            (##string-append
             "##type-"
             (##number->string total-fields 10)
             "-"
             (##symbol->string (if generative? name id))))
           (type-fields
            (generate-fields))
           (type-static
            (##structure
             ##type-type
             (if generative?
                 (##string->uninterned-symbol augmented-id-str)
                 (##string->symbol augmented-id-str))
             name
             flags
             super-type-static
             type-fields))
           (type-expression
            (if generative?
                (##string->symbol augmented-id-str)
                `',type-static))
           (type-id-expression
            (if generative?
                `(let ()
                   (##declare (extended-bindings) (not safe))
                   (##type-id ,type-expression))
                `',(##type-id type-static)))
           (all-fields
            (##type-all-fields type-static))
           (rev-field-alist
            (all-fields->rev-field-alist all-fields))
           (field-alist
            (##reverse rev-field-alist))
           (parameters
            (generate-parameters rev-field-alist)))
      (define (generate-getter-and-setter field tail)
        (let* ((descr
                (##cdr field))
               (field-name
                (##vector-ref descr 0))
               (field-index
                (##vector-ref descr 1))
               (getter
                (##vector-ref descr 2))
               (setter
                (##vector-ref descr 3))
               (getter-def
                (if getter
                    (let ((getter-name
                           (if (##eq? getter #t)
                               (##symbol-append prefix
                                                name
                                                '-
                                                field-name)
                               getter))
                          (getter-method
                           (if extender
                               '##structure-ref
                               '##direct-structure-ref)))
                      (if macros?
                          `((define-macro (,getter-name obj)
                              (##list '(let ()
                                         (##declare (extended-bindings))
                                         ,getter-method)
                                      obj
                                      ,field-index
                                      ',type-expression
                                      #f)))
                          `((define (,getter-name obj)
                              ((let ()
                                 (##declare (extended-bindings))
                                 ,getter-method)
                               obj
                               ,field-index
                               ,type-expression
                               ,getter-name)))))
                    `()))
               (setter-def
                (if setter
                    (let ((setter-name
                           (if (##eq? setter #t)
                               (##symbol-append prefix
                                                name
                                                '-
                                                field-name
                                                '-set!)
                               setter))
                          (setter-method
                           (if extender
                               '##structure-set!
                               '##direct-structure-set!)))
                      (if macros?
                          `((define-macro (,setter-name obj val)
                              (##list '(let ()
                                         (##declare (extended-bindings))
                                         ,setter-method)
                                      obj
                                      val
                                      ,field-index
                                      ',type-expression
                                      #f)))
                          `((define (,setter-name obj val)
                              ((let ()
                                 (##declare (extended-bindings))
                                 ,setter-method)
                               obj
                               val
                               ,field-index
                               ,type-expression
                               ,setter-name)))))
                    `())))
          (##append getter-def (##append setter-def tail))))
      (define (generate-structure-type-definition)
        `(define ,type-expression
           ((let ()
              (##declare (extended-bindings))
              ##structure)
            ##type-type
            ((let ()
               (##declare (extended-bindings))
               ##string->uninterned-symbol)
             ,augmented-id-str)
            ',name
            ',(##type-flags type-static)
            ,super-type-dynamic-expr
            ',(##type-fields type-static))))
      (define (generate-constructor-predicate-getters-setters)
        `(,@(if type-exhibitor
                (if macros?
                    `((define-macro (,type-exhibitor)
                        ',type-expression))
                    `((define (,type-exhibitor)
                        ,type-expression)))
                '())
          ,@(if constructor
                (let ((constructor-name
                       (if (##pair? constructor)
                           (##car constructor)
                           constructor)))
                  (if macros?
                      `((define-macro (,constructor-name ,@parameters)
                          (##list '(let ()
                                     (##declare (extended-bindings))
                                     ##structure)
                                  ',type-expression
                                  ,@(generate-initializations
                                     field-alist
                                     parameters
                                     #t))))
                      `((define (,constructor-name ,@parameters)
                          (##declare (extended-bindings))
                          (##structure
                           ,type-expression
                           ,@(generate-initializations
                              field-alist
                              parameters
                              #f))))))
                '())
          ,@(if constant-constructor
                `((define-macro (,constant-constructor ,@parameters)
                    (##define-type-construct-constant
                      ',constant-constructor
                      ,type-expression
                      ,@(generate-initializations
                         field-alist
                         parameters
                         #t))))
                '())
          ,@(if predicate
                (if macros?
                    `((define-macro (,predicate obj)
                        ,(if extender
                             ``(let ((obj ,,'obj))
                                 (##declare (extended-bindings))
                                 (and (##structure? obj)
                                      (let ((t0 (##structure-type obj))
                                            (type-id ,',type-id-expression))
                                        (or (##eq? (##type-id t0) type-id)
                                            (let ((t1 (##type-super t0)))
                                              (and t1
                                                   (or (##eq? (##type-id t1) type-id)
                                                       (##structure-instance-of? obj type-id))))))))
                             ``((let ()
                                  (##declare (extended-bindings))
                                  ##structure-direct-instance-of?)
                                ,,'obj
                                ,',type-id-expression))))
                    `((define (,predicate obj)
                        (##declare (extended-bindings))
                        ,(if extender
                             `(##structure-instance-of?
                               obj
                               ,type-id-expression)
                             `(##structure-direct-instance-of?
                               obj
                               ,type-id-expression)))))
                '())
          ,@(let loop ((lst1 (##reverse fields))
                       (lst2 '()))
              (if (##pair? lst1)
                  (loop (##cdr lst1)
                        (generate-getter-and-setter (##car lst1) lst2))
                  lst2))))
      (define (generate-definitions)
        (if generative?
            (##cons (generate-structure-type-definition)
                    (generate-constructor-predicate-getters-setters))
            (generate-constructor-predicate-getters-setters)))
      `(begin
         ,@(if extender
               (##list `(define-macro (,extender . args)
                          (##define-type-expand
                            ',extender
                            ',type-static
                            ',type-expression
                            args)))
               '())
         ,@(if implementer
               (if macros?
                   (##cons `(define-macro (,implementer)
                              ',(if generative?
                                    (generate-structure-type-definition)
                                    '(begin)))
                           (generate-constructor-predicate-getters-setters))
                   (##list `(define-macro (,implementer)
                              ',(##cons 'begin
                                        (generate-definitions)))))
               (generate-definitions)))))
  (let ((expansion
         (##define-type-parser
           form-name
           super-type-static
           args
           generate)))
    (if ##define-type-expansion-show?
        (pp expansion ##stdout-port))
    expansion))

;;! define-type macro
(define-macro (define-type . args)
  (%%define-type-expand 'define-type #f #f args))

;;! define-structure macro
(define-macro (define-structure . args)
  (%%define-type-expand 'define-structure #f #f args))

;;! define-record-type macro
(define-macro (define-record-type name constructor predicate . fields)
  (%%define-type-expand 'define-type #f #f `(,name constructor: ,constructor
                                                   predicate: ,predicate
                                                   ,@fields)))

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
                (error "syntax error in formal parameter list: " formals))))
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
                   (error "duplicate variable in formal parameter list: " formals))
                  ((duplicates? keys)
                   (error "duplicate keyword in formal parameter list: " formals))
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
