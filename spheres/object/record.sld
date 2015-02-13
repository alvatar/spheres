;;!!! .title Gambit srfi-99 records procedural and inspection implementation
;; with r6rs optional extensions).
;; .author Arthur T Smyles
;; .author Álvaro Castro-Castilla
;;
;; Copyright (c) 2008, Arthur T Smyles
;; Copyright (c) 2014, Álvaro Castro-Castilla
;; All rights reserved.

(define-library (spheres/object record)
  (export make-rtd
          rtd?
          rtd-constructor
          rtd-predicate
          rtd-deconstructor
          rtd-accessor
          rtd-mutator
          record?
          record-rtd
          rtd-name
          rtd-parent
          rtd-map
          rtd-for-each
          rtd-map-all
          rtd-for-all
          rtd-field-names
          rtd-all-field-names
          rtd-field-mutable?
          rtd-uid
          rtd-sealed?
          rtd-opaque?
          rtd-field-flag-printable?
          rtd-field-flag-mutable?
          rtd-field-flag-equality?
          rtd-field-flag-init?
          ;; Macros
          define-record-type)

  (define-syntax define-record-type
    (syntax-rules ()
      ((_ (type-name parent) constructor-spec predicate-spec . field-specs)
       (define-record-type-helper0
         type-name parent constructor-spec predicate-spec . field-specs))
      ((_ type-name constructor-spec predicate-spec . field-specs)
       (define-record-type-helper0
         type-name #f constructor-spec predicate-spec . field-specs))))

 (define-syntax define-record-type-helper
   (syntax-rules ()
     ((_ type-name fields parent #f predicate
         ((accessor field) ...) ((mutator mutable-field) ...))
      (define-record-type-helper
        type-name fields parent ignored predicate
        ((accessor field) ...) ((mutator mutable-field) ...)))
     ((_ type-name fields parent constructor #f
         ((accessor field) ...) ((mutator mutable-field) ...))
      (define-record-type-helper
        type-name fields parent constructor ignored
        ((accessor field) ...) ((mutator mutable-field) ...)))
     ((_ type-name fields parent (constructor args) predicate
         ((accessor field) ...) ((mutator mutable-field) ...))
      (begin (define type-name (make-rtd 'type-name 'fields parent))
             (define constructor (rtd-constructor type-name 'args))
             (define predicate (rtd-predicate type-name))
             (define accessor (rtd-accessor type-name 'field))
             ...
             (define mutator (rtd-mutator type-name 'mutable-field))
             ...))
     ((_ type-name fields parent constructor predicate
         ((accessor field) ...) ((mutator mutable-field) ...))
      (begin (define type-name (make-rtd 'type-name 'fields parent))
             (define constructor (rtd-constructor type-name))
             (define predicate (rtd-predicate type-name))
             (define accessor (rtd-accessor type-name 'field))
             ...
             (define mutator (rtd-mutator type-name 'mutable-field))
             ...))))

 ;; rsc-macro-transformer version
 ;; (define-syntax define-record-type-helper0
 ;;   (rsc-macro-transformer
 ;;    (lambda (form env)
 ;;      (define (syntax-violation proc msg . data)
 ;;        (apply error (list proc msg data)))
 ;;      (define (complain)
 ;;        (syntax-violation 'define-record-type "illegal syntax" form))
 ;;      (define (for-all proc lis)
 ;;        (let recur ((rest lis))
 ;;          (cond ((null? rest) #t)
 ;;                ((proc (car rest)) (recur (cdr rest)))
 ;;                (else #f))))
 ;;      (define (filter pred lis)          ; Sleazing with EQ? makes this
 ;;        (let recur ((lis lis))    
 ;;          (if (null? lis) lis   ; Use NOT-PAIR? to handle dotted lists.
 ;;              (let ((head (car lis))
 ;;                    (tail (cdr lis)))
 ;;                (if (pred head)
 ;;                    (let ((new-tail (recur tail))) ; Replicate the RECUR call so
 ;;                      (if (eq? tail new-tail) lis
 ;;                          (cons head new-tail)))
 ;;                    (recur tail))))))
 ;;      (let* ((type-name (list-ref form 1))
 ;;             (parent (list-ref form 2))
 ;;             (cspec (list-ref form 3))
 ;;             (pspec (list-ref form 4))
 ;;             (fspecs (cdddr (cddr form)))
 ;;             (type-name-string
 ;;              (begin (if (not (symbol? type-name))
 ;;                         (complain))
 ;;                     (symbol->string type-name)))
 ;;             (constructor-name
 ;;              (cond ((eq? cspec #f)
 ;;                     #f)
 ;;                    ((eq? cspec #t)
 ;;                     (string->symbol
 ;;                      (string-append "make-" type-name-string)))
 ;;                    ((symbol? cspec)
 ;;                     cspec)
 ;;                    ((pair? cspec)
 ;;                     (car cspec))
 ;;                    (else (complain))))
 ;;             (constructor-args
 ;;              (cond ((pair? cspec)
 ;;                     (if (not (for-all symbol? cspec))
 ;;                         (complain)
 ;;                         (list->vector (cdr cspec))))
 ;;                    (else #f)))
 ;;             (predicate-name
 ;;              (cond ((eq? pspec #f)
 ;;                     #f)
 ;;                    ((eq? pspec #t)
 ;;                     (string->symbol
 ;;                      (string-append type-name-string "?")))
 ;;                    ((symbol? pspec)
 ;;                     pspec)
 ;;                    (else (complain))))
 ;;             (field-specs
 ;;              (map (lambda (fspec)
 ;;                     (cond ((symbol? fspec)
 ;;                            (list 'immutable
 ;;                                  fspec
 ;;                                  (string->symbol
 ;;                                   (string-append
 ;;                                    type-name-string
 ;;                                    "-"
 ;;                                    (symbol->string fspec)))))
 ;;                           ((not (pair? fspec))
 ;;                            (complain))
 ;;                           ((not (list? fspec))
 ;;                            (complain))
 ;;                           ((not (for-all symbol? fspec))
 ;;                            (complain))
 ;;                           ((null? (cdr fspec))
 ;;                            (list 'mutable
 ;;                                  (car fspec)
 ;;                                  (string->symbol
 ;;                                   (string-append
 ;;                                    type-name-string
 ;;                                    "-"
 ;;                                    (symbol->string (car fspec))))
 ;;                                  (string->symbol
 ;;                                   (string-append
 ;;                                    type-name-string
 ;;                                    "-"
 ;;                                    (symbol->string (car fspec))
 ;;                                    "-set!"))))
 ;;                           ((null? (cddr fspec))
 ;;                            (list 'immutable
 ;;                                  (car fspec)
 ;;                                  (cadr fspec)))
 ;;                           ((null? (cdddr fspec))
 ;;                            (cons 'mutable fspec))
 ;;                           (else (complain))))
 ;;                   fspecs))
 ;;             (fields (list->vector (map cadr field-specs)))
 ;;             (accessor-fields
 ;;              (map (lambda (x) (list (caddr x) (cadr x)))
 ;;                   (filter (lambda (x) (>= (length x) 3))
 ;;                           field-specs)))
 ;;             (mutator-fields
 ;;              (map (lambda (x) (list (cadddr x) (cadr x)))
 ;;                   (filter (lambda (x) (= (length x) 4))
 ;;                           field-specs))))
 ;;        `(define-record-type-helper
 ;;           ,type-name ,fields ,parent
 ;;           ,(if constructor-args
 ;;                (list constructor-name constructor-args)
 ;;                constructor-name)
 ;;           ,predicate-name
 ;;           ,accessor-fields ,mutator-fields)))))

 ;; syntax-case version
 (define-syntax define-record-type-helper0
   (lambda (x)
     (define (complain)
       (syntax-violation 'define-record-type "illegal syntax" x))
     (syntax-case x ()
       ((_ tname pname constructor-spec predicate-spec . field-specs)
        (let* ((type-name (syntax->datum #'tname))
               (parent (syntax->datum #'pname))
               (cspec (syntax->datum #'constructor-spec))
               (pspec (syntax->datum #'predicate-spec))
               (fspecs (syntax->datum #'field-specs))
               (type-name-string
                (begin (if (not (symbol? type-name))
                           (complain))
                       (symbol->string type-name)))
               (constructor-name
                (cond ((eq? cspec #f)
                       #f)
                      ((eq? cspec #t)
                       (string->symbol
                        (string-append "make-" type-name-string)))
                      ((symbol? cspec)
                       cspec)
                      ((pair? cspec)
                       (car cspec))
                      (else (complain))))
               (constructor-args
                (cond ((pair? cspec)
                       (if (not (for-all symbol? cspec))
                           (complain)
                           (list->vector (cdr cspec))))
                      (else #f)))
               (predicate-name
                (cond ((eq? pspec #f)
                       #f)
                      ((eq? pspec #t)
                       (string->symbol
                        (string-append type-name-string "?")))
                      ((symbol? pspec)
                       pspec)
                      (else (complain))))
               (field-specs
                (map (lambda (fspec)
                       (cond ((symbol? fspec)
                              (list 'immutable
                                    fspec
                                    (string->symbol
                                     (string-append
                                      type-name-string
                                      "-"
                                      (symbol->string fspec)))))
                             ((not (pair? fspec))
                              (complain))
                             ((not (list? fspec))
                              (complain))
                             ((not (for-all symbol? fspec))
                              (complain))
                             ((null? (cdr fspec))
                              (list 'mutable
                                    (car fspec)
                                    (string->symbol
                                     (string-append
                                      type-name-string
                                      "-"
                                      (symbol->string (car fspec))))
                                    (string->symbol
                                     (string-append
                                      type-name-string
                                      "-"
                                      (symbol->string (car fspec))
                                      "-set!"))))
                             ((null? (cddr fspec))
                              (list 'immutable
                                    (car fspec)
                                    (cadr fspec)))
                             ((null? (cdddr fspec))
                              (cons 'mutable fspec))
                             (else (complain))))
                     fspecs))
               (fields (list->vector (map cadr field-specs)))
               (accessor-fields
                (map (lambda (x) (list (caddr x) (cadr x)))
                     (filter (lambda (x) (>= (length x) 3))
                             field-specs)))
               (mutator-fields
                (map (lambda (x) (list (cadddr x) (cadr x)))
                     (filter (lambda (x) (= (length x) 4))
                             field-specs))))
          (datum->syntax
           #'tname
           `(,#'define-record-type-helper
              ,type-name ,fields ,parent
              ,(if constructor-args
                   (list constructor-name constructor-args)
                   constructor-name)
              ,predicate-name
              ,accessor-fields ,mutator-fields)))))))

 (include "record.scm"))
