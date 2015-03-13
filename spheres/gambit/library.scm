;;!!! Spheres configuration and installation prelude
;; .author Álvaro Castro-Castilla, 2012-2015. See LICENSE file.


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

(define^ (filter pred lst)
  (define (foldr func end lst)
    (if (null? lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))
  (foldr (lambda (x y) (if (pred x) (cons x y) y))
         '()
         lst))

(define^ %library-paths '())

(define^ (%add-library-path! path)
  (set! %library-paths
        (append %library-paths
                (list (string-append
                       (path-strip-trailing-directory-separator path)
                       "/")))))

(define^ (%find-library lib)
  (if (not (%library? lib)) (%library-error lib))
  (let ((package (symbol->string (car lib)))
        (module (and (= (length lib) 2) (cadr lib))))
    (let* ((lib-relative (string-append package "/" (symbol->string module)))
           (option-here (string-append "../" lib-relative))
           (option-system (string-append
                           (path-expand "~~lib/")
                           option-here)))
      (let ((try-options `(,(string-append option-here ".sld") ;; First option
                           ,(string-append option-here ".scm") ;; Second option
                           ,@(let recur ((paths %library-paths))
                               (if (null? paths)
                                   paths
                                   (let ((lib-path (car paths)))
                                     (cons (string-append lib-path lib-relative ".sld")
                                           (cons (string-append lib-path lib-relative ".scm")
                                                 (recur (cdr paths)))))))
                           ,(string-append option-system ".sld") ;; Third option
                           ,(string-append option-system ".scm")))) ;; Fourth option
        (let find ((l try-options))
          (cond ((null? l) (error "Library not found -" lib))
                ((file-exists? (car l)) (car l))
                (else (find (cdr l)))))))))

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

(define^ (%find-library-path lib)
  (path-directory (%find-library lib)))

(define^ (%find-library-filename-no-extension lib)
  (path-strip-extension (path-strip-directory (%find-library lib))))

(define^ (%find-library-path-root lib)
  (let* ((path (%find-library-path lib))
         (path-length (string-length path))
         (rel-path (symbol->string (car lib)))
         (rel-path-length (string-length rel-path)))
    (string-shrink!
     path
     (let recur ((i 1))
       (if (and (< i path-length)
                (< i rel-path-length)
                (char=? (string-ref path (- path-length i 1))
                        (string-ref rel-path (- rel-path-length i))))
           (recur (+ i 1))
           (- path-length i 1))))
    path))

(define^ (%library-scm-path lib)
  (string-append (path-strip-extension (%find-library lib)) ".scm"))

(define^ (%library-sld-path lib)
 (string-append (path-strip-extension (%find-library lib)) ".sld"))

(define^ (%library-c-path lib)
  (string-append (path-strip-extension (%find-library lib)) ".o1.c"))

(define^ (%library-object-path lib)
  (string-append (path-strip-extension (%find-library lib)) ".o1"))

(define^ (%library-merged-scm-path lib)
  (string-append (%find-library-path lib)
                 (%find-library-filename-no-extension lib)
                 "-%-merged.scm"))

(define^ (%find-library-scm lib)
  (let ((f (%library-scm-path lib)))
    (and (file-exists? f) f)))

(define^ (%find-library-sld lib)
  (let ((f (%library-sld-path lib)))
    (and (file-exists? f) f)))

(define^ (%find-library-c lib)
  (let ((f (%library-c-path lib)))
    (and (file-exists? f) f)))

(define^ (%find-library-object lib)
  (let ((f (%library-object-path lib)))
    (and (file-exists? f) f)))

(define^ %library-declaration
  (let ((library-declarations (make-table)))
    (lambda (lib)
      (let ((lib-decl (table-ref library-declarations lib #f)))
        (or lib-decl
            (let* ((filesexps (with-input-from-file (%find-library lib) read-all))
                   (full-definition (assq 'define-library filesexps)))
              (and full-definition
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
                     declaration))))))))

;;! Expand a form, processing its cond-expand-features
(define^ (%expand-cond-features form)
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
          (else (cons (recur (car form)) (recur (cdr form)))))))

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
  (let ((expand-wildcards
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
                 deps)))
        (remove-gambit-library
         (lambda (deps)
           (let recur ((deps deps))
             (cond ((null? deps) '())
                   ((equal? '(gambit) (car deps))
                    (recur (cdr deps)))
                   (else
                    (cons (car deps) (recur (cdr deps)))))))))
    (let* ((decl (%library-declaration lib))
           (deps-pair (and decl (assq 'import decl))))
      (if deps-pair
          (remove-gambit-library
           (expand-wildcards
            (flatten-tag '##begin
                         (%expand-cond-features (cdr deps-pair)))))
          '()))))

(define^ %library-imports-all
  (let ((import-seq '()))
    (lambda (lib)
      (for-each %library-imports-all (%library-imports lib))
      (or (member lib import-seq)
          (set! import-seq (cons lib import-seq)))
      (cdr import-seq))))

;;! Expand cond-expand-features and eval syntax definitions, return imports, exports, includes
(define^ (%library-read-syntax lib #!optional (eval? #f))
  (let* ((lib-path (%find-library-path lib))
         (file-sexps (with-input-from-file (%find-library-sld lib) read))
         (define-library-args (cdr file-sexps))
         (syntax-defines (filter (lambda (sexp) (let ((head-sexp (car sexp)))
                                             (or (eq? head-sexp 'define-syntax)
                                                 (eq? head-sexp 'define-macro)
                                                 (eq? head-sexp '##begin)
                                                 (eq? head-sexp 'begin))))
                                 (%expand-cond-features define-library-args)))
         (syntax-definitions (make-table))
         (found-exports '())
         (found-imports '())
         (found-includes '()))
    (let recur ((sexps (%expand-cond-features define-library-args)))
      (or (null? sexps)
          (let ((head (car sexps)))
            (if (and (pair? head) (not (null? head)))
                (begin (case (car head)
                         ((import)
                          (set! found-imports (append (cdr head) found-imports)))
                         ((export)
                          (set! found-exports (append (cdr head) found-exports)))
                         ((include)
                          (let* ((incl (string-append lib-path (cadr head))))
                            (if (file-exists? incl)
                                (set! found-includes (cons incl found-includes))
                                (error (string-append (object->string head) " in library "
                                                      (object->string lib) " not found")))))
                         ((define-syntax)
                          (table-set! syntax-definitions (cadr head) (list 'define-syntax))
                          (if eval? (eval head)))
                         ((define-macro)
                          (table-set! syntax-definitions (caadr head) (list 'define-macro))
                          (if eval? (eval head)))
                         ((begin ##begin)
                          (recur (cdr head))))
                       (recur (cdr sexps)))))))
    (values
     (reverse found-imports)
     (reverse found-exports)
     (reverse found-includes)
     syntax-definitions)))

;;! Call
(define^ %call-task
  (lambda (where task . arguments)
    (define (escape str)
      (list->string
       (let recur ((lst (string->list str)))
         (cond ((null? lst) '())
               ((memv (car lst) '(#\( #\)))
                (cons #\\
                      (cons (car lst)
                            (recur (cdr lst)))))
               (else (cons (car lst)
                           (recur (cdr lst))))))))
    (let* ((inner-args-str
            (let recur ((rest arguments))
              (cond ((null? rest) "")
                    ((null? (cdr rest))
                     (string-append (object->string (car rest))
                                    (recur (cdr rest))))
                    (else
                     (string-append (object->string (car rest)) ","
                                    (recur (cdr rest)))))))
           (args-str
            (string-append (symbol->string task) "[" inner-args-str "]")))
      (if #f ;; verbose
          (begin (println "Call task: " task)
                 (println "Arguments: " (object->string arguments))
                 (println "In: " where)
                 (println "Command line string: ssrun " args-str))
          (println (string-append "Call task: ssrun " args-str)))
      (process-status (open-process (list path: "ssrun"
                                          arguments: (list args-str)
                                          directory: where
                                          stdout-redirection: #f))))))

;;! Returns #t if the library needs recompilation
(define^ (%library-updated? lib)
  (define (newer-than? filename1)
    (lambda (filename2)
      (or (not (file-exists? filename1))
          (> (time->seconds (file-last-modification-time filename2))
             (time->seconds (file-last-modification-time filename1))))))
  (let* ((sld-file (%find-library-sld lib))
         (obj-file (%library-object-path lib))
         (updated-file? (newer-than? obj-file)))
    (if sld-file
        (or (updated-file? sld-file)
            (let recur ((includes (receive (_ __ includes ___) (%library-read-syntax lib)
                                           includes)))
              (cond ((null? includes) #f)
                    ((updated-file? (car includes)) #t)
                    (else (recur (cdr includes)))))))))

;; Builds a ##namespace form for the library
(define (%library-make-namespace-form lib exports macro-defs #!key (allow-empty? #f))
  (let ((non-macro-exports
         (filter (lambda (i) (not (table-ref macro-defs i #f)))
                 exports)))
    (if (and (not allow-empty?) (null? non-macro-exports))
        #!void
        `(##namespace (,(string-append
                         (symbol->string (car lib))
                         "#"
                         (symbol->string (cadr lib))
                         "#")
                       ,@non-macro-exports)))))

;; Runs the thunk within a proper namespace definition
(define (%library-with-namespaces lib imports thunk)
  ;;(define (eval/pp f) (pp f) (eval f))
  (eval `(##begin
           ,(%library-make-namespace-form lib '() '() allow-empty?: #t)
           (##include "~~/lib/gambit#.scm")
           (##namespace ("" $make-environment
                         $sc-put-cte
                         $syntax-dispatch
                         bound-identifier=?
                         datum->syntax
                         environment?
                         free-identifier=?
                         generate-temporaries
                         identifier?
                         interaction-environment
                         literal-identifier=?
                         syntax-error
                         syntax->datum
                         syntax->list
                         syntax->vector
                         $load-module
                         $update-module
                         $include-file-hook
                         $generate-id
                         syntax-case-debug))
           ,@(map
              (lambda (import-lib)
                (receive (_ exports __ macro-defs) (%library-read-syntax import-lib)
                         (%library-make-namespace-form import-lib exports macro-defs)))
              imports)
           (##namespace ("" %load-library
                         %library-loaded-libraries))))
  (thunk))

(define %library-loaded-libraries (make-table))

;;! Include and load all library files and dependencies
(define^ (%load-library root-lib #!key compile only-syntax force (silent #f))
  (let recur ((lib root-lib))
    (define (load* file)
      (parameterize
       ((current-directory (path-directory file)))
       (let ((file (if (string=? (path-extension file) "")
                       (let recur ((n 1))
                         (let ((fullname (string-append file ".o" (number->string n))))
                           (if (file-exists? fullname)
                               (recur (+ n 1))
                               (if (> n 1)
                                   (string-append file ".o" (number->string (- n 1)))
                                   (string-append file ".scm")))))
                       file)))
         (if (not silent) (println "loading: " file))
         (load file))))
    ;; The recursive loading procedure
    (for-each recur (%library-imports lib))
    (if (or force (not (table-ref %library-loaded-libraries lib #f)))
        (let ((lib-path-root (%find-library-path-root lib))
              (lib-path (%find-library-path lib))
              (library-updated? (%library-updated? lib)))
          (if (and compile library-updated?)
              (%call-task lib-path-root 'compile lib))
          (let ((sld-file (%find-library-sld lib))
                (obj-file (%find-library-object lib))
                (scm-file (%find-library-scm lib)))
            (table-set! %library-loaded-libraries lib 'auto)
            ;; If an R7RS library
            (if sld-file
                (begin
                  (if (not silent) (println "including: " (path-expand sld-file)))
                  (receive (imports exports includes macro-defs) (%library-read-syntax lib #t)
                           (if (not only-syntax)
                               (if (and obj-file (not (%library-updated? lib)))
                                   (%library-with-namespaces
                                    lib
                                    imports
                                    (lambda () (load* obj-file)))
                                   (%library-with-namespaces
                                    lib
                                    imports
                                    (lambda () (for-each (lambda (f) (load* (path-strip-extension f)))
                                                    includes)))))))
                ;; Default procedure file is only loaded if there is no *.sld
                (if (and (or obj-file scm-file)
                         (not only-syntax))
                    (load* (or obj-file scm-file))))))))
  (table-set! %library-loaded-libraries root-lib 'user)
  (eval '(##namespace ("")))
  (table-for-each
   (lambda (lib v)
     (if (eq? v 'user)
         (begin
           (receive (_ exports __ macro-defs) (%library-read-syntax lib)
                    (eval (%library-make-namespace-form lib exports macro-defs))))))
   %library-loaded-libraries))
