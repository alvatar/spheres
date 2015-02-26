;;! Initialize SchemeSpheres environment
(define (##spheres-init)
  ;; Load syntax-case
  (println "*** INFO -- loading syntax expander for iOS")
  (load "spheres/core/lib/syntax-case.o1"))

;;! Include-eval: makes modules available in a spawned REPL
(define (##spheres-include-eval modules)
  (define eval-file
    (lambda (file)
      (for-each eval (with-input-from-file file read-all))))
  (for-each
   (lambda (m)
     (eval-file
      (string-append
       "spheres/" (let ((sphere (car m)))
                    (if (eq? '= sphere) "__local" (keyword->string sphere)))
       "/src/" (symbol->string (cadr m)) ".scm")))
   modules))

;;! Load modules from a network address
(define (##spheres-load-remote server modules)
  (define local-sources-suffix "__local/")
  (unless (file-exists? "spheres/") (create-directory "spheres/"))
  (unless (file-exists? (string-append "spheres/" local-sources-suffix))
          (create-directory (string-append "spheres/" local-sources-suffix)))
  (unless (file-exists? (string-append "spheres/" local-sources-suffix "/src"))
          (create-directory (string-append "spheres/" local-sources-suffix "/src")))
  ;; (SDL_Log "passed creating directories")
  (for-each
   (lambda (m)
     (let* ((sphere (car m))
            (local-directory
             (string-append "spheres/"
                            (if (eq? '= sphere) local-sources-suffix (keyword->string sphere))
                            "src/"))
            (filename (string-append (symbol->string (cadr m)) ".scm"))
            (local-file-path (string-append local-directory filename)))
       ;;(SDL_Log local-file-path)
       (if (eq? '= sphere)
           (begin
             ;;(SDL_Log (string-append "Retrieving file: " filename))
             (if (zero? (parameterize
                         ((current-directory local-directory))
                         (shell-command (string-append "wget -nv -N " server "/" filename))))
                 (begin (load local-file-path)
                        ;;(SDL_Log (string-append "Remote file loaded: " filename))
                        )
                 (let ((message (string-append filename " could not be retrieved")))
                   ;; (SDL_Log message)
                   (println message))))
           (error-log "only local modules can be loaded remotely"))))
   modules))
