;;!!! Simple FFI generation macros
;; .author Álvaro Castro Castilla, 2013-2015. All Rights Reserved.

;;------------------------------------------------------------------------------
;;!! Old stuff to keep in mind

;; This nasty hack substitutes the '() for ()
;; It turns out that Gambit uses () for argument lists, which is not an acceptable
;; syntax for most syntax-rules expanders
;; (define-macro (c-lambda . body)
;;   `(##c-lambda ,@(map (lambda (f) (if (and (pair? f)
;;                                       (pair? (cdr f))
;;                                       (eq? (cadr f) '()))
;;                                  '()
;;                                  f))
;;                       body)))

;; ;; The same for c-define
;; (define-macro (c-define . body)
;;   `(##c-define ,@(map (lambda (f) (if (and (pair? f)
;;                                       (pair? (cdr f))
;;                                       (eq? (cadr f) '()))
;;                                  '()
;;                                  f))
;;                       body)))


;;------------------------------------------------------------------------------
;;!! Prelude

(define-macro (eval-in-macro-environment . exprs)
  (eval (if (null? (cdr exprs)) (car exprs) (cons 'begin exprs))
        (interaction-environment)))

(define-macro (define^ . args)
  `(eval-in-macro-environment
    (define ,(car args) ,@(cdr args))))


;;------------------------------------------------------------------------------
;;!! Macro utils

(define^ (%%get-key-arg args key default)
  (if (null? args)
      default
      (let ((found (memq key args)))
        (if (and found
                 (not (null? (cdr found))))
            (cadr found)
            default))))

;; Build a string from list of elements (anything)
(define^ (%%generic-string-append . ol)
  (define (->string o)
    (cond ((string? o) o)
          ((symbol? o) (symbol->string o))
          ((keyword? o) (keyword->string o))
          (else (error (string-append "c-define-array :: ->string: undesirable type -- "
                                      (object->string o))))))
  (apply string-append (map ->string ol)))

;; Append anything into a symbol
(define^ (%%generic-symbol-append . ol)
  (string->symbol (apply %%generic-string-append ol)))

;; Turn a scheme-name into a c_name, simply changing the - to _
(define^ (%%scheme-name->c-name name)
  (let* ((name-str (cond ((string? name) name)
                         ((symbol? name) (symbol->string name))
                         (else (object->string name))))
         (new-name (string-copy name-str))
         (name-length (string-length new-name)))
    (let recur ((i 0))
      (if (< i name-length)
          (begin (if (char=? (string-ref new-name i) #\-)
                     (string-set! new-name i #\_))
                 (recur (+ i 1)))
          new-name))))


;;! Schemify a name: turn-it-into-a-scheme-name
;; .author Fred LeMaster
(define^ (%%c-name->scheme-name symbol-or-string)
  ((lambda (str)
     (letrec
         ((str-length (string-length str))
          (case-changed?
           (lambda (i)
             (let ((h (- i 1)))
               (cond ((< h 0) #f)
                     ((and (char-lower-case? (string-ref str h))
                           (char-upper-case? (string-ref str i)))
                      #t)
                     (else #f)))))
          (char-loop
           (lambda (i out)
             (cond ((>= i str-length) out)
                   ((char=? (string-ref str i) #\_)
                    (char-loop (+ i 1) (cons #\- out)))
                   ((and (char=? (string-ref str i) #\:)
                         (< (+ i 1) str-length)
                         (char=? (string-ref str (+ i 1)) #\:))
                    (char-loop (+ i 2) (cons #\- out)))
                   ((case-changed? i)
                    (char-loop (+ i 1)
                               (cons (char-downcase
                                      (string-ref str i))
                                     (cons #\-
                                           out))))
                   (else (char-loop (+ i 1)
                                    (cons
                                     (char-downcase
                                      (string-ref str i))
                                     out)))))))
       (list->string (reverse (char-loop 0 '())))))
   (cond ((symbol? symbol-or-string) (symbol->string symbol-or-string))
         ((string? symbol-or-string) symbol-or-string)
         (else (error "%%c-name->scheme-name: expected symbol or string")))))

;; Generate a list of top-level forms
(define^ (%%begin-top-level-forms #!rest define-blocks)
  (cons 'begin
        (let recur ((ds define-blocks))
          (cond ((null? ds) '())
                ((null? (car ds)) (recur (cdr ds)))
                (else (cons (car ds)
                            (recur (cdr ds))))))))


;;------------------------------------------------------------------------------
;;!! FFI generation

;;! C constants generation macro
;; Creating the bindings in a simple C function makes for more compact
;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2012-February/005688.html
(define-macro (c-define-constants . names)
  (let ((nb-names (length names))
        (wrapper (gensym)))
    (letrec ((interval (lambda (lo hi)
                         (if (< lo hi) (cons lo (interval (+ lo 1) hi)) '()))))
      `(begin
         (##define ,wrapper
           (c-lambda (int)
                     int
                     ,(string-append
                       "static int _tmp_[] = {\n"
                       (apply string-append
                              (map (lambda (i name)
                                     (let ((name-str (symbol->string name)))
                                       (string-append
                                        (if (> i 0) "," "")
                                        name-str)))
                                   (interval 0 nb-names)
                                   names))
                       "};\n"
                       "___result = _tmp_[___arg1];\n")))
         ,@(map (lambda (i name)
                  `(##define ,name (,wrapper ,i)))
                (interval 0 nb-names)
                names)))))

;;! Build a size-of value equivalent to the C operator
;; c-build-sizeof float -> sizeof-float
(define-macro (c-define-sizeof scheme-type . rest)
  (let ((c-type (%%get-key-arg rest c-type: (symbol->string scheme-type))))
    `(define ,(string->symbol (string-append (symbol->string scheme-type) "-size"))
       ((c-lambda () size-t
                  ,(string-append "___result = sizeof(" c-type ");"))))))

;;! Build FFI procedures for C type arrays. Only for use with basic types, not structs.
;; (c-define-array float f32) ->
;; alloc-float*
;; float*-ref
;; float*-set!
;; *->float*
;; f32vector->float*
(define-macro (c-define-array scheme-type . rest)
  (let ((c-type (%%get-key-arg rest c-type: scheme-type))
        (scheme-vector (%%get-key-arg rest scheme-vector: #f)))
    (if (not scheme-vector) (error "c-define-array macro :: scheme-vector: argument is mandatory"))
    (let ((release-type-str (string-append  "___release_" (%%scheme-name->c-name scheme-type)))
          (type scheme-type)
          (type*
           (%%generic-symbol-append scheme-type "*"))
          (type*/nonnull
           (%%generic-symbol-append scheme-type "*/nonnull"))
          (type*/release-rc
           (%%generic-symbol-append scheme-type "*/release-rc")))
      (let ((expansion
             (%%begin-top-level-forms
              `(c-declare
                ,(string-append
                  "static ___SCMOBJ " release-type-str "( void* ptr )\n"
                  "{\n"
                  ;; " printf(\"GC called free()!\\n\");\n"
                  ;; "  ___EXT(___release_rc)( ptr );\n"
                  "  free( ptr );\n"
                  "  return ___FIX(___NO_ERR);\n"
                  "}\n"))
              ;; Alloc managed by Gambit's GC
              `(define ,(%%generic-symbol-append 'alloc- scheme-type '*/unmanaged)
                 (c-lambda (size-t)
                           ,type*/nonnull
                           ,(%%generic-string-append "___result_voidstar = malloc(___arg1*sizeof(" c-type "));")))
              ;; Alloc unmanaged by Gambit's GC
              `(define ,(%%generic-symbol-append 'alloc- scheme-type '*)
                 (c-lambda (size-t)
                           ,type*/release-rc
                           ;; ,(%%generic-string-append "___result_voidstar = ___EXT(___alloc_rc)(___arg1*sizeof(" c-type "));")
                           ,(%%generic-string-append "___result_voidstar = malloc(___arg1*sizeof(" c-type "));")))
              `(define ,(%%generic-symbol-append scheme-type '*-ref)
                 (c-lambda (,type*/nonnull size-t)
                           ,scheme-type
                           "___result = ___arg1[___arg2];"))
              `(define ,(%%generic-symbol-append scheme-type '*-set!)
                 (c-lambda (,type*/nonnull size-t ,scheme-type)
                           void
                           "___arg1[___arg2] = ___arg3;"))
              `(define ,(%%generic-symbol-append '*-> scheme-type)
                 (c-lambda (,type*/nonnull)
                           ,scheme-type
                           "___result = *___arg1;"))
              (if scheme-vector
                  `(define (,(%%generic-symbol-append scheme-vector 'vector-> scheme-type '*) vec)
                     (let* ((length (,(%%generic-symbol-append scheme-vector 'vector-length) vec))
                            (buf (,(%%generic-symbol-append 'alloc- scheme-type '*) length)))
                       (let loop ((i 0))
                         (if (fx< i length)
                             (begin
                               (,(%%generic-symbol-append scheme-type '*-set!) buf i (,(%%generic-symbol-append scheme-vector 'vector-ref) vec i))
                               (loop (fx+ i 1)))
                             buf))))
                  '()))))
        (if #f ;; #t for debugging
            (pp `(definition:
                   (c-define-array scheme-type: ,scheme-type c-type: ,c-type scheme-vector: ,scheme-vector)
                   expansion:
                   ,expansion)))
        expansion))))


;;------------------------------------------------------------------------------
;;!! FFI generation

;;! define types for structs, unions and arrays
;; .author Álvaro Castro-Castilla, based on code by Estevo Castro
;; (c-define-type* (struct MyStruct))
;; (c-define-type* (union MyUnion))
;; (c-define-type* myType)
(define-macro (c-define-type* type/struct/union)
  (let* ((type (if (pair? type/struct/union)
                   (cadr type/struct/union)
                   type/struct/union))
         (struct-or-union (if (pair? type/struct/union)
                              (car type/struct/union)
                              #f))
         (type-str (symbol->string type))
         (release-type-str (string-append "___release_" (%%scheme-name->c-name type-str)))
         (type* (%%generic-symbol-append type-str "*"))
         (type*/nonnull (%%generic-symbol-append type-str "*/nonnull"))
         (type*/release-rc (%%generic-symbol-append type-str "*/release-rc")))
    (let ((expansion
           (%%begin-top-level-forms
            (if struct-or-union
                `(c-define-type ,type (,struct-or-union ,type-str))
                '())
            `(c-define-type ,type* (pointer ,type (,type*)))
            `(c-define-type ,type*/nonnull (nonnull-pointer ,type (,type*)))
            `(c-define-type ,type*/release-rc (nonnull-pointer ,type (,type*) ,release-type-str)))))
      (if #f ;; #t for debugging
          (pp `(definition:
                 (c-define-extended-type ,type)
                 expansion:
                 ,expansion)))
      expansion)))

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
