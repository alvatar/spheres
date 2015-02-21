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

;;! Expand cond-expand-features and eval syntax definitions
(define^ (%library-read-syntax&find-includes lib eval?)
  (define (filter f l)
    (let recur ((l l))
      (if (null? l) '()
          (let ((head (car l)))
            (if (f head) (cons head (filter f (cdr l)))
                (filter f (cdr l)))))))
  (let* ((file-sexps (with-input-from-file (%find-library-sld lib) read))
         (define-library-args (cdr file-sexps))
         (syntax-defines (filter (lambda (sexp) (let ((head-sexp (car sexp)))
                                             (or (eq? head-sexp 'define-syntax)
                                                 (eq? head-sexp 'define-macro)
                                                 (eq? head-sexp '##begin)
                                                 (eq? head-sexp 'begin))))
                                 (%expand-cond-features define-library-args)))
         (found-includes '()))
    (let recur ((sexps (%expand-cond-features define-library-args)))
      (or (null? sexps)
          (let ((head (car sexps)))
            (if (and (pair? head) (not (null? head)))
                (case (car head)
                  ((define-syntax)
                   (if eval? (eval head))
                   (recur (cdr sexps)))
                  ((define-macro)
                   (if eval? (eval head))
                   (recur (cdr sexps)))
                  ((include)
                   (set! found-includes (cons head found-includes))
                   (recur (cdr sexps)))
                  ((begin ##begin)
                   (recur (cdr head))
                   (recur (cdr sexps)))
                  (else
                   (recur (cdr sexps))))))))
    (reverse found-includes)))

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

;;! Include and load all library files and dependencies
(define^ %load-library
  (let ((loaded-libs '()))
    (lambda (lib #!key compile only-syntax force (verbose #t))
      (define (load* file)
        (parameterize
         ((current-directory (path-directory file)))
         (let ((load-result (load file)))
           (if verbose
               (println (string-append "loading: " load-result))))))
      (let recur ((lib lib))
        (for-each recur (%library-imports lib))
        (if (or force (not (member lib loaded-libs)))
            (let ((lib-path-root (%find-library-path-root lib))
                  (lib-path (%find-library-path lib)))
              (if compile (%call-task lib-path-root 'compile lib))
              (let ((sld-file (%find-library-sld lib))
                    (obj-file (%find-library-object lib))
                    (scm-file (%find-library-scm lib)))
                (set! loaded-libs (cons lib loaded-libs))
                (if sld-file
                    (begin
                      (if verbose (println "including: " (path-expand sld-file)))
                      (let ((eval&get-includes (%library-read-syntax&find-includes lib #t)))
                        (if (not only-syntax)
                            (if obj-file
                                (load* obj-file)
                                (for-each (lambda (f)
                                            (load* (path-strip-extension
                                                    (string-append lib-path (cadr f)))))
                                          eval&get-includes)))))
                    ;; Default procedure file is only loaded if there is no *.sld
                    (if (and (or obj-file scm-file)
                             (not only-syntax))
                        (begin (if verbose (println "loading: " procedures-file))
                               (load (or obj-file scm-file))))))))))))
