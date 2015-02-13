;;!!! Prototype-based object system for SchemeSpheres (macros)
;; Based on Tiny Talk, by Kenneth A Dicke
;; .author Kenneth A Dicke, 2008
;; .author Álvaro Castro-Castilla, 2010-2014

(define-library (spheres/object prototype)
  (export ->
          prototype:shallow-clone
          prototype:shallow-clone-method
          prototype:deep-clone
          prototype:deep-clone-method
          string<<
          $
          <<
          define-predicate
          object
          pp<<)
  (import (spheres/core base))
  
 ;;! Send messages to objects
 (define-syntax $
   (syntax-rules ()
     ((_ ?selector ?obj ?args ...)
      (let ((obj ?obj))
        ((-> '?selector obj) obj ?args ...)))))

 ;;! Alternate syntax for sending messages to objects
 (define-syntax <<
   (syntax-rules ()
     ((_ ?obj ?selector ?args ...)
      (let ((obj ?obj))
        ((-> '?selector obj) obj ?args ...)))))

 ;;! Define a type-checking predicate
 ;; Doens't work because of hygienic ?name renaming
 ;; (define-syntax define-predicate
 ;;   (syntax-rules ()
 ;;     ((_ name)
 ;;      (define (?name obj)
 ;;        (cond ((not (prototype-object? obj)) #f)
 ;;              ((find-method '?name obj) => (lambda (m) (m obj)))
 ;;              (else #f))))))
 (define-macro (define-predicate ?name)
   `(define (,?name obj)
      (cond ((not (prototype-object? obj)) #f)
            ((prototype:find-method ',?name obj) => (lambda (m) (m obj)))
            (else #f))))

 ;; (define-syntax prototype:field-spec
 ;;   (syntax-rules ()
 ;;     ((_ <name> <val>)
 ;;      (cons '<name> (make-setter-getter <val>)))
 ;;     ((_ <name> <val> <filter-proc>)
 ;;      (cons '<name> (make-setter-getter <val> <filter-proc>)))
 ;;     ((_ <name>)
 ;;      (cons '<name> (make-setter-getter ':uninitialized)))))

 ;;! Object creation
 (define-syntax object
   (syntax-rules ()
     ((_ (?field-spec ...) ((?method-name ?method-self ?method-args ...) ?expr1 ?exprs ...) ...)
      (letrec-syntax ((%?field-spec->accessor
                       (syntax-rules ()
                         ((field-spec->accessor (?name ?val ?filter-proc))
                          (cons '?name (prototype:make-setter-getter ?val ?filter-proc)))
                         ((_ (?name ?val))
                          (cons '?name (prototype:make-setter-getter ?val)))
                         ((_ ?name)
                          (cons '?name (prototype:make-setter-getter #!void))))))
        (let* ((fields (list (%?field-spec->accessor ?field-spec) ...))
               (field-names (map car fields)))
          (make-prototype-object
           (prototype:make-dispatch-table
            (append
             fields
             (list (cons '?method-name
                         (lambda (?method-self ?method-args ...) ?expr1 ?exprs ...))
                   ...
                   ;; default behaviors          
                   (cons 'field-names (lambda (obj) field-names))
                   (cons 'shallow-clone prototype:shallow-clone-method)
                   (cons 'deep-clone prototype:deep-clone-method))))))))))

 ;;! print object
 (define-syntax pp<<
   (syntax-rules ()
     ((_ ?obj)
      (pp (string<< ?obj)))))

 (include "prototype.scm"))
