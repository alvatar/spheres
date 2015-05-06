;; Helper for define-c-struct and define-c-union
(define^ (%%c-define-struct-or-union struct-or-union type fields)
  (let* ((type-str (symbol->string type))
         (struct-type-str (string-append
                           (case struct-or-union
                             ((struct) "struct ")
                             ((union) "union ")
                             (else
                              (error "%%c-define-struct-or-union: first parameter must be 'struct or 'union")))
                           type-str))
         (struct-type*-str (string-append struct-type-str "*"))
         (release-type-str (string-append "___release_" type-str))
         (type* (%%generic-symbol-append type-str "*"))
         (type*/nonnull (%%generic-symbol-append type-str "*/nonnull"))
         (type*/release-rc (%%generic-symbol-append type-str "*/release-rc")))
    (define (field-getter-setter field-spec)
      (let* ((field (car field-spec))
             (field-str (symbol->string field))
             (field-description (cadr field-spec)))
        (if (pair? field-description)
            ;; Field is either a 'struct', an 'array' or an 'array of structs'
            (let* ((field-tag (car field-description))
                   (field-type (cadr field-description))
                   (field-type-str (symbol->string field-type)))
              (case field-tag
                ;; Struct
                ((struct)
                 `((define ,(%%generic-symbol-append type-str "-" field-str)
                     (c-lambda (,type*/nonnull)
                               ,(%%generic-symbol-append field-type-str "*/nonnull")
                               ,(string-append "___result_voidstar = &___arg1->" field-str ";")))

                   (define ,(%%generic-symbol-append type-str "-" field-str "-set!")
                     (c-lambda (,type*/nonnull ,field-type)
                               void
                               ,(string-append "___arg1->" field-str " = ___arg2;")))))
                ;; Array of fundamental type
                ((array)
                 ;; generate a getter and a setter
                 `((define ,(%%generic-symbol-append type-str "-" field-str "-ref")
                     (c-lambda (,type*/nonnull int)
                               ,field-type
                               ,(string-append "___result = ___arg1->" field-str "[___arg2];")))
                   (define ,(%%generic-symbol-append type-str "-" field-str "-set!")
                     (c-lambda (,type*/nonnull int ,field-type)
                               void
                               ,(string-append "___arg1->" field-str "[___arg2] = ___arg3;")))))
                ;; Array of structs
                ((struct-array)
                 ;; only generate a getter returning struct address
                 `((define ,(%%generic-symbol-append type-str "-" field-str "-ref")
                     (c-lambda (,type*/nonnull int)
                               ,(%%generic-symbol-append field-type-str "*/nonnull")
                               ,(string-append "___result_voidstar = &___arg1->" field-str "[___arg2];")))))))
            ;; Field is fundamental type
            `((define ,(%%generic-symbol-append type-str "-" field-str)
                (c-lambda (,type*/nonnull)
                          ,field-description
                          ,(string-append "___result = ___arg1->" field-str ";")))

              (define ,(%%generic-symbol-append type-str "-" field-str "-set!")
                (c-lambda (,type*/nonnull ,field-description)
                          void
                          ,(string-append "___arg1->" field-str " = ___arg2;")))))))
    (let ((expansion
           `(begin
              ;; Define the release function which is called when the
              ;; object is no longer accessible from the Scheme world.
              (c-declare
               ,(string-append
                 "static ___SCMOBJ " release-type-str "( void* ptr )\n"
                 "{\n"
                 "  ___EXT(___release_rc)( ptr );\n"
                 "  return ___FIX(___NO_ERR);\n"
                 "}\n"))
              ;; Define type allocator procedure.
              (define ,(%%generic-symbol-append "alloc-" type-str)
                (c-lambda ()
                          ,type*/release-rc
                          ,(string-append "___result_voidstar = ___EXT(___alloc_rc)( sizeof( " struct-type-str " ) );")))
              ;; Dereference
              (define ,(%%generic-symbol-append "*->" type-str)
                (c-lambda (,type*/nonnull)
                          ,type
                          ,(string-append "___result_voidstar = (" type-str "*)___arg1;")))
              ;; Define field getters and setters.
              ,@(apply append (map field-getter-setter fields)))))
      (if #f ;; #t for debugging
          (pp `(definition:
                 (c-define-struct ,type ,@fields)
                 expansion:
                 ,expansion)))
      expansion)))

;;! Defines the c-define-struct macro, which extends the Gambit FFI to
;; interface to C structures.
(define-macro (c-define-struct type . fields)
  (%%c-define-struct-or-union 'struct type fields))

;;! Defines the c-define-union macro, which extends the Gambit FFI to
;; interface to C structures.
(define-macro (c-define-union type . fields)
  (%%c-define-struct-or-union 'union type fields))
