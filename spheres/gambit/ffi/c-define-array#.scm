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
