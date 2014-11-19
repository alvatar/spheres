;;
;; a module loader for psyntax and git
;;
;; Copyright (c) 2013, Matt Hastie.
;; Permission to copy this software, in whole or in part, to use this
;; software for any lawful purpose, and to redistribute this software
;; is granted subject to the restriction that all copies made of this
;; software must include this copyright notice in full.  This software
;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;; NATURE WHATSOEVER.
;;
(module scsc/module
  (system-root
   current-module-paths
   submodule-paths
   debug)
  
  (define debug
    (make-parameter #f))

  (define system-root
    (lambda ()
      (let ((root (with-input-from-process
                   (list path: "git"
                         arguments: '("rev-parse" "--show-toplevel"))
                   read-line)))
        (if (string? root)
            root
            #f))))
        
  (define submodule-file
    (lambda () 
      (let ((root (system-root)))
        (if root
            (path-expand ".gitmodules" root)
            #f))))
  
  (define parse-submodules
    (lambda (module-file)
      (define content
        (with-exception-catcher
         (lambda (e) 
           (or (and (no-such-file-or-directory-exception? e) '())
               (raise e)))
         (lambda ()
           (or (and module-file (with-input-from-file module-file read-all))
               '()))))
      (let next ((content content)
                 (modules '()))
        (cond ((null? content)
               modules)
              ((and (pair? (car content))
                    (eq? (caar content) 'submodule))
               (let ((name (string->symbol (cadar content)))
                     (path (symbol->string (caddr (memq 'path content)))))
                 (next (cdr content)
                       (cons `(,name . ,path) modules))))
              (else
               (next (cdr content) modules))))))

  (define current-module-paths
    (make-parameter (lambda ()
                      (or (and (system-root)
                               (cons (system-root) (submodule-paths)))
                          '()))))
  
  (define submodule-paths
    (lambda ()
      (map (lambda (relative-path)
             (path-expand relative-path (system-root)))
           (map cdr (parse-submodules (submodule-file))))))

  (define module-hash
    (lambda ()
      (with-input-from-process
       (list path: "git"
             arguments: '("rev-parse" "--short" "HEAD"))
       read-line)))

  (define make-id-source
    (lambda ()
      (let ((i -1)
            (delimiter "."))
        (lambda (symbol-maybe)
          (set! i (+ 1 i))
          (string->symbol
           (string-append
            (module-hash) delimiter
            (number->string i 16)
            (if symbol-maybe
                (string-append delimiter (symbol->string symbol-maybe))
                (string))))))))

  (define module-file?
    (lambda (file)
      (and (string=? (path-extension file) ".scm")
           (or (string=? (path-strip-extension file) "module")
               (string=? (path-extension (path-strip-extension file)) ".module")))))
  
  (define mid->update! (make-table))
  
  (define current-mid (make-parameter #f))

  (module (change-time begin-change-time-analysis! extend-change-time-analysis!)
    (define mid->change-time (make-table))
    (define change-time
      (lambda (mid) ((table-ref mid->change-time mid))))
    (define make-change-time
      (lambda (file path)
        (lambda () (time->seconds (file-info-last-change-time
                              (file-info (path-expand file path)))))))
    (define begin-change-time-analysis!
      (lambda (mid file path)
        (table-set! mid->change-time mid (make-change-time file path))))
    (define extend-change-time-analysis!
      (lambda (mid file path)
        (let ((change-time (table-ref mid->change-time mid)))
          (table-set! mid->change-time mid
                      (lambda () (let ((change-time* (make-change-time file path)))
                              (let ((t (change-time)) (t* (change-time*)))
                                (if (< t t*) t* t)))))))))

  (define or/load/continue
    (lambda (mid path file continue)
      (define module-file-mids
        (let* ((module-file-mids '())
               (cons-module-file-mids!
                (lambda (id b top-token)
                  (set! module-file-mids
                        (cons (syntax->datum id) module-file-mids)))))
          (and (module-file? file)
               (parameterize (($sc-put-cte cons-module-file-mids!))
                 (let ((load-module $load-module))
                   (set! $load-module (lambda (mid ctem rtem)
                                        'ignore))
                   (eval ((sc#make-expander '(V) '())
                          (##source-code (##read-all-as-a-begin-expr-from-path
                                          (path-expand file path)
                                          (##current-readtable)
                                          ##wrap-datum
                                          ##unwrap-datum))))
                   (set! $load-module load-module))))
          module-file-mids))

      (let ((mid (syntax->datum mid)))
        (if (memq mid module-file-mids)
            (let reload ((load-time (time->seconds (current-time))))
              (begin-change-time-analysis! mid file path)
              (parameterize ((current-mid mid)
                             ($generate-id (make-id-source)))
                (load (path-expand file path)))
              (table-set! mid->update! mid (lambda ()
                                             (if (< load-time (change-time mid))
                                                 (reload (time->seconds (current-time)))
                                                 (begin (if (debug)
                                                            (pp 'current))
                                                        'current))))
              'success)
            (continue)))))

  (set! $load-module
        (lambda (mid ctem rtem)
          (define (scan-paths paths)
            (if (null? paths)
                'not-found
                (let ((p (car paths))
                      (ps (cdr paths)))
                  (let ((files (or (and (file-exists? p)
                                        (directory-files p))
                                   (scan-paths ps))))
                    (define (scan-files files)
                      (if (null? files)
                          (scan-paths ps)
                          (let ((f (car files))
                                (fs (cdr files)))
                            (if (debug) (pp `(path ,p files ,files)))
                            (or/load/continue mid p f (lambda () (scan-files fs))))))
                    (scan-files files)))))
          (scan-paths ((current-module-paths)))))
  
  (set! $update-module
        (lambda (mid ctem rtem)
          (with-exception-catcher
           (lambda (e) (if (unbound-table-key-exception? e)
                      'unknown
                      (raise e)))
           (lambda ()
             (let ((update! (table-ref mid->update! (syntax->datum mid))))
               (update!)
               'success)))))
  
  (set! $include-file-hook
        (lambda (file)
          (if (current-mid)
              (extend-change-time-analysis! (current-mid) file (current-directory))))))
