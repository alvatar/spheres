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

(define^ (%library:filter pred lst)
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

;;! Throw a library parse error
(define^ (%library-error-parse lib)
  (error "Error parsing library declaration: " lib))

;;! Throw a library not found error
(define^ (%library-error-not-found lib)
  (error "Library not found: " lib))

(define^ (%find-library lib)
  (if (not (%library? lib)) (%library-error-parse lib))
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
          (cond ((null? l) (%library-error-not-found lib))
                ((file-exists? (car l)) (car l))
                (else (find (cdr l)))))))))

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
                                       (%library:filter
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

;;! Expand cond-expand-features and eval syntax definitions, return imports, exports, includes, macro name definitions
(define^ (%library-read-syntax lib #!key (eval? #f))
  (let ((lib-path (%find-library-path lib))
        (lib-sld (%find-library-sld lib)))
    (if lib-sld
        (let* ((file-sexps (with-input-from-file lib-sld read))
               (define-library-args (cdr file-sexps))
               (expanded-cond-features (%expand-cond-features define-library-args))
               (syntax-defines (%library:filter
                                (lambda (sexp) (let ((head-sexp (car sexp)))
                                            (or (eq? head-sexp 'define-syntax)
                                                (eq? head-sexp 'define-macro)
                                                (eq? head-sexp '##begin)
                                                (eq? head-sexp 'begin))))
                                expanded-cond-features))
               (syntax-definitions (make-table))
               (found-imports '())
               (found-exports '())
               (found-includes '()))
          (let recur ((sexps expanded-cond-features))
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
           syntax-definitions))
        (values #f #f #f #f))))

;;! Get only the imports
(define^ (%library-imports lib)
  (receive (imports _ __ ___) (%library-read-syntax lib) imports))

;;! Get only the exports
(define^ (%library-exports lib)
  (receive (_ exports __ ___) (%library-read-syntax lib) exports))

;;! Get only the includes
(define^ (%library-includes lib)
  (receive (_ __ includes ___) (%library-read-syntax lib) includes))

;; Get the full tree of imports as a flattened list
(define^ %library-imports-all
  (let ((import-seq '()))
    (lambda (lib)
      (for-each %library-imports-all (%library-imports lib))
      (or (member lib import-seq)
          (set! import-seq (cons lib import-seq)))
      (cdr import-seq))))

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
            (let recur ((includes (%library-includes lib)))
              (cond ((null? includes) #f)
                    ((updated-file? (car includes)) #t)
                    (else (recur (cdr includes))))))
        (updated-file? (%find-library-scm lib)))))

;; Builds a ##namespace form for the library
(define^ (%library-make-namespace-form lib exports macro-defs #!key (allow-empty? #f))
  (let ((non-macro-exports
         (%library:filter (lambda (i) (not (table-ref macro-defs i #f)))
                          exports)))
    (if (and (not allow-empty?) (null? non-macro-exports))
        '()
        `((##namespace (,(string-append
                          (symbol->string (car lib))
                          "#"
                          (symbol->string (cadr lib))
                          "#")
                        ,@non-macro-exports))))))

;; Create all necessary namespace definitions for a library
(define^ (%library-make-prelude lib #!optional (imports (%library-imports lib)))
  (if (%find-library-sld lib)
      `(,@(%library-make-namespace-form lib '() '() allow-empty?: #t)
        (##include "~~lib/gambit#.scm")
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
        (##namespace ("" %load-library
                      %library-loaded-libraries))
        ,@(apply append (map
                         (lambda (import-lib)
                           (receive (_ exports __ macro-defs) (%library-read-syntax import-lib)
                                    (%library-make-namespace-form import-lib exports macro-defs)))
                         imports)))
      '(#!void)))

;;! Hash table containing info about loaded libraries
(define^ %library-loaded-libraries (make-table))

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

;;! Include and load all library files and dependencies.
;; Returns generated expansion-time code
(define^ (%load-library root-lib #!key compile only-syntax force)
  (define output-code '())
  (define (emit-code! code) (set! output-code (append output-code code)))
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
         (emit-code! `((load ,file))))))
    ;; Recursively load if imports found
    (let ((imports (%library-imports lib)))
      (if imports (for-each recur imports)))
    ;; Loading and including
    (if (or force (not (table-ref %library-loaded-libraries lib #f)))
        (let ((lib-path-root (%find-library-path-root lib))
              (lib-path (%find-library-path lib))
              (library-updated? (%library-updated? lib)))
          (if (and compile library-updated?)
              (or (zero? (%call-task lib-path-root 'compile lib))
                  (error "in task" 'compile lib)))
          (let ((sld-file (%find-library-sld lib))
                (obj-file (%find-library-object lib))
                (scm-file (%find-library-scm lib)))
            (table-set! %library-loaded-libraries lib 'auto)
            ;; If an R7RS library
            (if sld-file
                ;; Read and eval the syntax
                (receive (imports exports includes macro-defs)
                         (%library-read-syntax lib eval?: #t)
                         (if (not only-syntax)
                             (begin (emit-code!
                                     (%library-make-prelude
                                      lib
                                      imports))
                                    (if (and obj-file (not (%library-updated? lib)))
                                        (load* obj-file)
                                        (for-each (lambda (f) (load* (path-strip-extension f)))
                                                  includes)))))
                ;; Default procedure file is only loaded if there is no *.sld
                (if (and (or obj-file scm-file)
                         (not only-syntax))
                    (load* (or obj-file scm-file))))))))
  (if (not only-syntax)
      (begin
        (if (%find-library-sld root-lib)
            (table-set! %library-loaded-libraries root-lib 'user))
        (emit-code! '((##namespace (""))))
        (table-for-each
         (lambda (lib v)
           ;; Only libraries loaded by the user should be visible. Unfortunately,
           ;; libraries that expand into code used by other libraries don't work
           ;; this way. To solve this, we need to implement a full syntactic tower.
           ;; To make visible only the libraries loaded by the user: (eq? v 'user)
           (if (or (eq? v 'auto) (eq? v 'user))
               (receive (_ exports __ macro-defs) (%library-read-syntax lib)
                        (emit-code!
                         (%library-make-namespace-form lib exports macro-defs)))))
         %library-loaded-libraries)))
  (if #f (pp (cons '##begin output-code)))
  (cons '##begin output-code))
