;;!!! Spheres configuration and installation prelude
;; .author Álvaro Castro-Castilla, Copyright (c) 2012-2014 All rights reserved.


(load ##syntax-case-file)

(define (expander:include file)
  (for-each eval (with-input-from-file file read-all)))

(define (expander:include-library-definition file)
  (define (filter f l)
    (let recur ((l l))
      (if (null? l) '()
          (let ((head (car l)))
            (if (f head) (cons head (filter f (cdr l)))
                (filter f (cdr l)))))))
  (let* ((file-sexps (with-input-from-file file read))
         (define-library-args (cdr file-sexps))
         (syntax-defines (filter (lambda (sexp) (eq? (car sexp) 'define-syntax))
                                 define-library-args)))
    (for-each eval syntax-defines)))

;;------------------------------------------------------------------------------
;;!! Macro utils

;;!! Define functions for usage in low-level macros (first method)
;; (define^ (f ... ) ... )

(##define-macro (eval-in-macro-environment . exprs)
  (if (pair? exprs)
      (eval (if (null? (cdr exprs)) (car exprs) (cons 'begin exprs))
            (interaction-environment))
      #f))

(##define-macro (define^ . args)
  (let ((pattern (car args))
        (body (cdr args)))
    `(eval-in-macro-environment
      (##define ,pattern ,@body))))

;;! symbol->keyword
(define^ (symbol->keyword s)
  (string->keyword (symbol->string s)))

;;! keyword->symbol
(define^ (keyword->symbol k)
  (string->symbol (keyword->string k)))

;;! Anything to symbol
(define^ (->symbol o)
  (string->symbol (object->string o)))

;;! Anything to keyword
(define^ (->keyword o)
  (string->keyword (object->string o)))

;;!! Define functions for usage in low-level macros (second method)
;; Insert your defines inside the following macros:
;; (at-expand-time-and-runtime
;;   (define ... )
;;   ... )
;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2009-August/003781.html
;; ;;! Define for both expand time and runtime
;; (##define-macro (at-expand-time-and-runtime . exprs)
;;   (let ((l `(begin ,@exprs)))
;;     (eval l)
;;     l))
;; ;;! Define for expand time
;; (##define-macro (at-expand-time . expr)
;;   (eval (cons 'begin expr)))


;;------------------------------------------------------------------------------
;;!! R7RS libraries

(define^ (%find-library lib)
  (let* ((option-here (string-append
                       "../" (symbol->string (car lib)) "/"
                       (symbol->string (cadr lib))))
         (option-system (string-append
                         (path-expand "~~lib/")
                         option-here)))
    (let ((try-options (list (string-append option-here ".sld")
                             (string-append option-here ".scm")
                             (string-append option-system ".sld")
                             (string-append option-system ".scm"))))
      (let find ((l try-options))
        (cond ((null? l) (error "Library not found"))
              ((file-exists? (car l)) (car l))
              (else (find (cdr l))))))))

;;! Throw a library format error
(define^ (%library-error lib)
  (error "Error parsing library declaration: " lib))

;;! Library format check
;; It's a list of 1 or 2 elements
(define^ (%library? lib)
  (and (pair? lib)
       (let ((len (length lib)))
         (and (<= len 2)
              (symbol? (car lib))
              (or (= len 1) (symbol? (cadr lib)))))))

(define^ (%find-library-object lib)
  (let find-with-suffix ((found #f)
                         (i 1))
    (let ((f (string-append (path-strip-extension (%find-library lib))
                            ".o" (number->string i))))
      (if (file-exists? f)
          (find-with-suffix f (+ 1 i))
          found))))

(define^ (%find-library-scm lib)
  (let ((f (string-append (path-strip-extension (%find-library lib)) ".scm")))
    (and (file-exists? f) f)))

(define^ (%find-library-sld lib)
  (let ((f (string-append (path-strip-extension (%find-library lib)) ".sld")))
    (and (file-exists? f) f)))

(define^ (%library-object-filename lib)
  (string-append (path-strip-extension (%find-library lib)) ".o1"))

(define^ (%library-c-filename lib)
  (string-append (path-strip-extension (%find-library lib)) ".c"))

(define^ %library-declaration
  (let ((library-declarations (make-table)))
    (define (foldr func end lst)
      (if (null? lst)
          end
          (func (car lst) (foldr func end (cdr lst)))))
    (define (filter pred lst)
      (foldr (lambda (x y) (if (pred x) (cons x y) y))
             '()
             lst))
    (lambda (lib)
      (let ((lib-decl (table-ref library-declarations lib #f)))
        (or lib-decl
            (let* ((filesexps (with-input-from-file (%find-library lib) read-all))
                   (full-definition (assq 'define-library filesexps)))
              (if (not full-definition) (error "Library declaration not found"))
              (let ((declaration (cons
                                  (cadr full-definition) ;; add the library name
                                  (filter
                                   (lambda (expr) (or (eq? (car expr) 'import)
                                                 (eq? (car expr) 'export)
                                                 (eq? (car expr) 'rename)
                                                 (eq? (car expr) 'cond-expand)))
                                   (cdr full-definition)))))
                (table-set! library-declarations lib
                            declaration)
                declaration)))))))

;;! Get imports of the library
(define^ (%library-imports lib)
  (define (flatten-tag tag lst)
    (let recur ((lst (if (and (not (null? lst)) (eq? (car lst) tag)) (cdr lst) lst)))
      (cond
       ((null? lst) '())
       ((and (pair? (car lst)) (eq? tag (caar lst)))
        (append (recur (cdar lst)) (recur (cdr lst))))
       ((pair? (car lst))
        (cons (recur (car lst)) (recur (cdr lst))))
       (else (cons (car lst) (recur (cdr lst)))))))
  (let ((expand-cond-features
         (lambda (form)
           (define expand-clauses
             (lambda clauses
               (define (feature-present? id)
                 (memq id (##cond-expand-features)))
               (define (eval-feature-req? feature-req)
                 (define (eval-and-clause? req-list)
                   (or (null? req-list)
                       (and (eval-feature-req? (car req-list))
                            (eval-and-clause? (cdr req-list)))))
                 (define (eval-or-clause? req-list)
                   (and (not (null? req-list))
                        (or (eval-feature-req? (car req-list))
                            (eval-or-clause? (cdr req-list)))))
                 (define (eval-not-clause? req)
                   (not (eval-feature-req? req)))
                 (cond
                  ((not (pair? feature-req))
                   (feature-present? feature-req))
                  ((eq? 'and (car feature-req))
                   (eval-and-clause? (cdr feature-req)))
                  ((eq? 'or (car feature-req))
                   (eval-or-clause? (cdr feature-req)))
                  ((eq? 'not (car feature-req))
                   (apply eval-not-clause? (cdr feature-req)))
                  (else (error "Invalid <feature requirement>"))))
               (define (do-cond-expand clauses)
                 (cond
                  ((null? clauses)  (error "Unfulfilled cond-expand"))
                  ((not (pair? (car clauses)))
                   (error "Invalid <cond-expand clause>"))
                  ((eq? 'else (caar clauses))
                   (or (null? (cdr clauses))
                       (error "else clause is not the final one"))
                   (cons '##begin (cdar clauses)))
                  ((eval-feature-req? (caar clauses))
                   (cons '##begin (cdar clauses)))
                  (else (do-cond-expand (cdr clauses)))))
               (do-cond-expand clauses)))
           (let recur ((form form))
             (cond ((null? form) '())
                   ((and (pair? form) (eq? 'cond-expand (car form)))
                    (apply expand-clauses (cdr form)))
                   ((not (pair? form)) form)
                   (else (cons (recur (car form)) (recur (cdr form))))))))
        (expand-wildcards
         (lambda (deps)
           (define map*
             (lambda (f l)
               (cond ((null? l) '())
                     ((not (pair? l)) (f l))
                     (else (cons (map* f (car l)) (map* f (cdr l)))))))

           (map* (lambda (e)
                   (if (eq? e '=)
                       (string->keyword (symbol->string sphere))
                       e))
                 deps))))
    (let ((deps-pair (assq 'import (%library-declaration lib))))
      (if deps-pair
          (expand-wildcards
           (flatten-tag '##begin
                        (expand-cond-features (cdr deps-pair))))
          '()))))

(define^ %library-imports-all
  (let ((import-seq '()))
    (lambda (lib)
      (for-each %library-imports-all (%library-imports lib))
      (or (member lib import-seq)
          (set! import-seq (cons lib import-seq)))
      (cdr import-seq))))


;;------------------------------------------------------------------------------
;;!! Including and loading

;; ;;! Include module and dependencies
;; (define ##include-module-and-dependencies #f)

;; ;;! Load module and dependencies
;; (define ##load-module-and-dependencies #f)

;; (let* ((*loaded-modules* '())
;;        (*included-modules* '())
;;        (include-single-module
;;         (lambda (module options)
;;           (let* ((verbose (and (memq 'verbose options) #t))
;;                  (sphere (%module-sphere module))
;;                  (module-name (symbol->string (%module-id module))))
;;             (if sphere
;;                 (let ((include-file (string-append (%module-path-src module)
;;                                                    (%module-filename-scm module))))                  
;;                   (if (not (member (%module-normalize module override-version: '()) *included-modules*))
;;                       (begin
;;                         (if verbose
;;                             (display (string-append "-- source included -- " (object->string module) "\n")))
;;                         (set! *included-modules* (cons (%module-normalize module override-version: '()) *included-modules*))
;;                         (expander:include include-file))))
;;                 (begin
;;                   (if (not (member (%module-normalize module override-version: '()) *included-modules*))
;;                       (begin
;;                         (if verbose
;;                             (display (string-append "-- source included -- " (object->string module) "\n")))
;;                         (set! *included-modules* (cons (%module-normalize module override-version: '()) *included-modules*))
;;                         (expander:include (%module-filename-scm module)))))))))
;;        (load-single-module
;;         (lambda (module options)
;;           (%check-module module '##load-module-and-dependencies#load-single-module)
;;           (let ((verbose (and (memq 'verbose options) #t))
;;                 (includes (and (memq 'includes options) #t))
;;                 (sphere (%module-sphere module)))
;;             (let ((header-module (%module-header module)))
;;               (if header-module
;;                   (expander:include (string-append (%module-path-src header-module)
;;                                                    (%module-filename-scm header-module))))
;;               (if includes
;;                   (for-each (lambda (m) (include-single-module m '(verbose)))
;;                             (%module-shallow-dependencies-to-include module)))
;;               (if sphere
;;                   (let ((file-o (string-append (%sphere-path sphere) (default-lib-directory) (%module-filename-o module)))
;;                         (file-scm (string-append (%sphere-path sphere) (default-source-directory) (%module-filename-scm module))))
;;                     (cond ((file-exists? file-o)
;;                            (if verbose
;;                                (display (string-append "-- object loaded -- " (object->string module) "\n")))
;;                            (load file-o)
;;                                         ;(pp file-o)
;;                            file-o)
;;                           ((file-exists? file-scm)
;;                            (load file-scm)
;;                            (if verbose
;;                                (display (string-append "-- source loaded -- " (object->string module) "\n")))
;;                            file-scm)
;;                           (else
;;                            (error (string-append "Module: "
;;                                                  (object->string module)
;;                                                  " cannot be found in current sphere's path"))))
;;                     (set! *loaded-modules* (cons (%module-normalize module override-version: '()) *loaded-modules*)))
;;                   (begin (if verbose
;;                              (display (string-append "-- object loaded -- " (object->string module) "\n")))
;;                          (load (%module-filename-scm module)))))))))
;;   (set!
;;    ##include-module-and-dependencies
;;    (lambda (root-module options)
;;      (let ((force-include (and (memq 'force options) #f)))
;;        (let recur ((module root-module))
;;          (if (or force-include
;;                  (not (member (%module-normalize module override-version: '()) *included-modules*)))
;;              (begin (for-each recur (%module-shallow-dependencies-to-include module))
;;                     (include-single-module module '(verbose #t))))))))
;;   (set!
;;    ##load-module-and-dependencies
;;    (lambda (root-module options)
;;      ;; Get options, as #t or #f
;;      (let ((omit-root (and (memq 'omit-root options) #t)))
;;        (let recur ((module root-module))
;;          (if (not (member (%module-normalize module override-version: '()) *loaded-modules*))
;;              (begin (for-each recur (%module-shallow-dependencies-to-load module))
;;                     (or (and omit-root (equal? root-module module))
;;                         (load-single-module module options)))))))))

;; ;;! import-include macro
;; (define-macro (##spheres-include . module)
;;   (cond
;;    ((string? (car module))
;;     ;; If filename given, just include it (doesn't register as loaded module)
;;     (if (string=? ".sld" (path-extension (car module)))
;;         (expander:include-library-definition (car module))
;;         (expander:include (car module))))
;;    (else
;;     (let ((module (if (null? (cdr module))
;;                       (car module)
;;                       ;; If it defines the sphere, process the sphere name to make it a keyword
;;                       (cons (let ((first (car module)))
;;                               (if (keyword? first)
;;                                   first
;;                                   (let ((str (apply string
;;                                                     (string->list
;;                                                      (symbol->string first)))))
;;                                     (string-shrink! str (- (string-length str) 1))
;;                                     (string->keyword str))))
;;                             (cdr module)))))
;;       (%check-module module '##spheres-include)
;;       `(##include-module-and-dependencies ',module '(verbose))))))

;; ;;! import macro, loads dependencies
;; (define-macro (##spheres-load . module)
;;   (let* ((module (if (null? (cdr module))
;;                      (car module)
;;                      ;; If it defines the sphere, process the sphere name to make it a keyword
;;                      (cons (let ((first (car module)))
;;                              (if (keyword? first)
;;                                  first
;;                                  (let* ((str (symbol->string first))
;;                                         (str-len (string-length str)))
;;                                    (if (not (char=? (string-ref str (- str-len 1)) #\:))
;;                                        (error "Sphere names should end with ':'"))
;;                                    (string-shrink! str (- str-len 1))                                   
;;                                    (string->keyword str))))
;;                            (cdr module)))))
;;     (%check-module module '##spheres-load)
;;     `(##load-module-and-dependencies ',module '(verbose includes))))


;;
;;
;; // REMOVE
;;
;;
(define^ ##spheres-include (lambda (x) x))
