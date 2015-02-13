;;!!! Macros for programming in type-class style in Scheme
;; .author André van Tonder: original work, 2004
;; .author Álvaro Castro-Castilla: testing, adapted to SchemeSpheres



(define-library (spheres/object type-class)

  (export define-class
          define=>
          lambda=>
          with
          import-instance)

  (import (spheres/core match))
  
  ;; (define-class <field-form> ...)
  ;;
  ;; (define=> (<procedure-name> <class-form> ...) . body)
  ;;
  ;; (lambda=> (<class-form> ...) . body)
  ;;
  ;; (with (<instance-form> ...) . body)
  ;;
  ;; (import-instance <instance-form> ...)
  ;;
  ;; <field-form> = field-label
  ;;              | (<superclass-name> field-label)
  ;;
  ;; <class-form> = <class-name>
  ;;              | (<class-name> <prefix-symbol>)
  ;;
  ;; <instance-form> = (<class-name> <instance-expr>)
  ;;                 | (<class-name> <instance-expr> <prefix-symbol>)

  (define-macro (define-class name . fields)
    (define (filter pred lis)           ; Sleazing with EQ? makes this
      (let recur ((lis lis))
        (cond ((null? lis) '())
              ((pred (car lis)) (cons (car lis) (recur (cdr lis))))
              (else (recur (cdr lis))))))
    (let ((k       (gensym))
          (args    (gensym))
          (formals (map (lambda (field) (gensym)) fields))
          (supers  (filter pair? fields))
          (labels  (map (lambda (field)
                          (match field
                                 ((super label) label)
                                 (label         label)))
                        fields)))
      `(begin
         (define ,(string->symbol
                   (string-append "make-" (symbol->string name)))
           (lambda ,formals
             (lambda (,k) (,k . ,formals))))
         (define-macro (,name ,k . ,args)
           `(,,k "descriptor" ,',supers ,',labels . ,,args)))))

  (define-macro (with . body)
    (match body
           ((() . exps)
            `(let () . ,exps))
           ((((name instance) . rest) . exps)
            `(,name with 
                    ,name "" ,instance ,rest . ,exps))
           ((((name instance prefix) . rest) . exps)
            `(,name with 
                    ,name ,(symbol->string prefix)
                    ,instance ,rest . ,exps))
           (("descriptor" supers labels name pre instance rest . exps)
            (let ((pre-labels
                   (map (lambda (label)
                          (string->symbol 
                           (string-append pre (symbol->string label))))
                        labels))
                  (super-bindings
                   (map (lambda (class-label)
                          `(,(car class-label)
                            ,(string->symbol
                              (string-append pre
                                             (symbol->string
                                              (cadr class-label))))
                            ,(string->symbol pre)))
                        supers)))
              `(,instance (lambda ,pre-labels
                            (with ,super-bindings
                                  (with ,rest . ,exps))))))))

  (define-macro (import-instance . bindings)
    (match bindings
           (()
            "Bindings imported")
           (((name instance) . rest)
            `(,name import-instance 
                    ,name "" ,instance ,rest))
           (((name instance prefix) . rest)
            `(,name import-instance
                    ,name ,(symbol->string prefix) 
                    ,instance ,rest))
           (("descriptor" supers labels name pre instance rest)
            (let ((pre-labels.temps
                   (map (lambda (label)
                          (cons 
                           (string->symbol 
                            (string-append pre (symbol->string label)))
                           (gensym)))
                        labels))
                  (super-bindings
                   (map (lambda (class-label)
                          `(,(car class-label)
                            ,(string->symbol
                              (string-append pre
                                             (symbol->string
                                              (cadr class-label))))
                            ,(string->symbol pre)))
                        supers)))
              `(begin ,@(map (lambda (pre-label.temp)
                               `(define ,(car pre-label.temp) #f))
                             pre-labels.temps)
                      (,instance (lambda ,(map cdr pre-labels.temps)
                                   ,@(map (lambda (pre-label.temp)
                                            `(set! ,(car pre-label.temp)
                                                   ,(cdr pre-label.temp)))
                                          pre-labels.temps)))
                      (import-instance . ,super-bindings)
                      (import-instance . ,rest))))))

  (define-macro (lambda=> quals . body)
    (let ((quals-binds (map (lambda (qual)
                              (match qual
                                     ((cls prefix) (list cls (gensym) prefix))
                                     (cls          (list cls (gensym)))))
                            quals)))
      `(lambda ,(map cadr quals-binds)
         (with ,quals-binds
               . ,body))))

  (define-macro (define=> name.quals . body)
    (let ((name  (car name.quals))
          (quals (cdr name.quals)))
      `(define ,name (lambda=> ,quals . ,body))))




  ;;------------------------------------------------------------------------------




  ;; (define make-prefixed-id
  ;;   (lambda (scope prefix sym)
  ;;     (datum->syntax
  ;;      scope
  ;;      (string->symbol
  ;;       (string-append prefix (symbol->string sym))))))


  ;; (define-syntax define-class
  ;;   (lambda (stx)
  ;;     (syntax-case stx ()
  ;;       ((_ name field ...)
  ;;        (with-syntax
  ;;         ([ctor (make-prefixed-id (syntax name) "make-"
  ;;                                  (syntax->datum (syntax name)))]
  ;;          [(formal ...) (generate-temporaries (syntax (field ...)))])
  ;;         (syntax
  ;;          (begin
  ;;            (define ctor
  ;;              (lambda (formal ...)
  ;;                (lambda (k) (k formal ...))))
  ;;            (define-syntax name
  ;;              (let ([fields (syntax->datum (syntax (field ...)))])
  ;;                (lambda (x)
  ;;                  (syntax-case x ()
  ;;                    ((_ scope k prefix-str-stx instance arg (... ...))
  ;;                     (let ([prefix-str (syntax->datum (syntax prefix-str-stx))])
  ;;                       (with-syntax
  ;;                        ([labels (map (lambda (stx)
  ;;                                        (make-prefixed-id (syntax scope)
  ;;                                                          prefix-str
  ;;                                                          (syntax->datum
  ;;                                                           (syntax-case stx ()
  ;;                                                             ((super label) (syntax label))
  ;;                                                             (label (syntax label))))))
  ;;                                      (syntax-e (syntax (field ...))))]
  ;;                         [supers (map (lambda (class-label)
  ;;                                        (with-syntax
  ;;                                         ([class (datum->syntax (syntax scope) (car class-label))]
  ;;                                          [label (make-prefixed-id (syntax scope) prefix-str (cadr class-label))]
  ;;                                          [prefix-sym (string->symbol prefix-str)])
  ;;                                         (syntax (class label prefix-sym))))
  ;;                                      (filter pair? fields))])
  ;;                        (syntax (k "descriptor" scope instance labels supers arg (... ...)))))))))))))))))

  ;; (define-syntax with
  ;;   (lambda (stx)
  ;;     (syntax-case stx ()
  ;;       ((k (instance-form ...) exp ...)
  ;;        (syntax (with/scope k (instance-form ...) exp ...))))))

  ;; (define-syntax with/scope
  ;;   (lambda (stx)
  ;;     (syntax-case stx ()
  ;;       ((_ scope () exp ...)
  ;;        (syntax (begin exp ...)))
  ;;       ((_ scope ((name instance) rest ...) exp ...)
  ;;        (syntax (name scope with/scope "" instance (rest ...) exp ...)))
  ;;       ((_ scope ((name instance prefix) rest ...) exp ...)
  ;;        (with-syntax ([prefix-str (symbol->string (syntax->datum (syntax prefix)))])
  ;;                     (syntax (name scope with/scope prefix-str instance (rest ...) exp ...))))
  ;;       ((_ "descriptor" scope instance pre-labels super-bindings rest ...)
  ;;        (syntax
  ;;         (instance
  ;;          (lambda pre-labels
  ;;            (with/scope scope super-bindings
  ;;                        (with/scope scope rest ...)))))))))

  ;; (define-syntax import-instance
  ;;   (lambda (stx)
  ;;     (syntax-case stx ()
  ;;       ((k (name instance ...) rest ...)
  ;;        (syntax (import/scope k (name instance ...) rest ...))))))

  ;; (define-syntax import/scope
  ;;   (lambda (stx)
  ;;     (syntax-case stx ()
  ;;       ((_ scope)
  ;;        (syntax "Bindings imported"))
  ;;       ((_ scope (name instance) rest ...)
  ;;        (syntax (name scope import/scope "" instance (rest ...))))
  ;;       ((_ scope (name instance prefix) rest ...)
  ;;        (with-syntax ((prefix-str (symbol->string (syntax->datum (syntax prefix)))))
  ;;                     (syntax (name scope import/scope prefix-str instance (rest ...)))))
  ;;       ((_ "descriptor" scope instance (pre-label ...) (super-binding ...) (rest ...))
  ;;        (with-syntax ([(temp ...) (generate-temporaries (syntax (pre-label ...)))])
  ;;                     (syntax
  ;;                      (begin
  ;;                        (define pre-label #f) ...
  ;;                        (instance (lambda (temp ...)
  ;;                                    (set! pre-label temp) ...))
  ;;                        (import/scope scope super-binding ...)
  ;;                        (import/scope scope rest ...))))))))

  ;; (define-syntax lambda=>
  ;;   (lambda (stx)
  ;;     (syntax-case stx ()
  ;;       ((k (class-form ...) body ...)
  ;;        (syntax (lambda=> k (class-form ...) body ...)))
  ;;       ((_ scope quals body ...)
  ;;        (let ([quals-binds (map (lambda (qual tmp)
  ;;                                  (with-syntax ((tmp tmp))
  ;;                                               (syntax-e
  ;;                                                (syntax-case qual ()
  ;;                                                  ((cls prefix) (syntax (cls tmp prefix)))
  ;;                                                  (cls          (syntax (cls tmp)))))))
  ;;                                (syntax-e (syntax quals))
  ;;                                (generate-temporaries (syntax quals)))])
  ;;          (with-syntax
  ;;           ([formals (map cadr quals-binds)]
  ;;            [quals-binds-stx quals-binds])
  ;;           (syntax
  ;;            (lambda formals
  ;;              (with/scope scope quals-binds-stx
  ;;                          body ...)))))))))

  ;; (define-syntax define=>
  ;;   (lambda (stx)
  ;;     (syntax-case stx ()
  ;;       ((k (name quals ...) body ...)
  ;;        (syntax (define name (lambda=> k (quals ...) body ...)))))))
  )
