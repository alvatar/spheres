(module gambit/struct (define-structure)
  (define-syntax define-structure
   (lambda (x)
     (define construct-name
       (lambda (template-identifier . args)
         (datum->syntax-object
          template-identifier
          (string->symbol
           (apply string-append
                  (map (lambda (x)
                         (if (string? x)
                             x
                             (symbol->string (syntax-object->datum x))))
                       args))))))
     (define construct-inits
       (lambda (template-identifier ids)
         (datum->syntax-object
          template-identifier
          (let next ((ids (syntax->list ids)))
            (if (null? ids)
                ids
                `(,(car (syntax-object->datum ids)) . (0 . (#f . ,(next (cdr ids))))))))))
     (syntax-case x ()
       ((_ name id1 ...)
        (sc#andmap identifier? (syntax (name id1 ...)))
        (with-syntax
         ((constructor (construct-name (syntax name) "make-" (syntax name)))
          (predicate (construct-name (syntax name) (syntax name) "?"))
          ((inits ...) (construct-inits (syntax name) (syntax (id1 ...))))
          ((access ...)
           (map (lambda (x) (construct-name x (syntax name) "-" x))
                (syntax (id1 ...))))
          ((assign ...)
           (map (lambda (x)
                  (construct-name x "set-" (syntax name) "-" x "!"))
                (syntax (id1 ...))))
          (structure-length (length (syntax (id1 ...))))
          (type-dtor (construct-name (syntax name) "##type-" (number->string (length (syntax (id1 ...)))) "-" (syntax name)))
          (type-dtor-string (datum->syntax-object (syntax name)
                                                  (symbol->string
                                                   (syntax-object->datum
                                                    (construct-name (syntax name)
                                                                    "##type-"
                                                                    (number->string (length (syntax (id1 ...))))
                                                                    "-"
                                                                    (syntax name))))))
          ((index ...)
           (let f ((i 1) (ids (syntax (id1 ...))))
             (if (null? ids)
                 '()
                 (cons i (f (+ i 1) (cdr ids)))))))
         (syntax (begin
                   (define type-dtor
                     (##structure ##type-type
                                  (##string->uninterned-symbol type-dtor-string)
                                  'name
                                  8
                                  #f
                                  '#(inits ...)))
                   (define constructor
                     (lambda (id1 ...)
                       (##structure type-dtor id1 ...)))
                   (define predicate
                     (lambda (obj)
                       (##structure-direct-instance-of? obj (##type-id type-dtor))))
                   (define access
                     (lambda (obj)
                       (##structure-ref obj index type-dtor access)))
                   ...
                   (define assign
                     (lambda (obj val)
                       (##structure-set! obj val index type-dtor assign)))
                   ...))))))))
