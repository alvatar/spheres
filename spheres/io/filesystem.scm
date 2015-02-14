;;!!! Filesets and combinators
;; .author Alvaro Castro Castilla, 2012-2015


(define (filesystem:info . args)
  (println (append-strings (cons "*** INFO: " args))))

(define (filesystem:warn . args)
  (println (append-strings (cons "*** WARNING: " args))))

(define (extension=? ext)
  (ends-with? ext))

(define (ends-with? end)
  (lambda (name)
    (and (>= (string-length name) (string-length end))
         (string=? (substring name (- (string-length name) (string-length end)) (string-length name))
                   end))))

(define* (newer-than? ext (dir: (current-directory)))
  (lambda (name)
    (let ((name0 (string-append 
                  dir
                  (path-strip-extension (path-strip-directory name))
                  ext)))
      (or (not (file-exists? name0))
          (>= (time->seconds (file-last-modification-time name))
              (time->seconds (file-last-modification-time name0)))))))

(define (shift fn)
  (lambda (t)
    (lambda (name)
      (fn (t name)))))

(define (f-and . ts)
  (lambda (name)
    (let f-and ((ts ts))
      (or (null? ts)
          (and ((car ts) name)
               (f-and (cdr ts)))))))

(define (f-or . ts)
  (lambda (name)
    (let f-or ((ts ts))
      (and (pair? ts)
           (or ((car ts) name)
               (f-or (cdr ts)))))))
    
(define f-not (shift not))

(define (any? name) #t)
(define (none? name) #f)

(define* (fileset (dir: (current-directory))
                  (test: any?)
                  (recursive: #f))
  (define (reduce f i l)
    (let reduce ((i i) (l l))
      (if (null? l) i
          (reduce (f i (car l)) (cdr l)))))
  (let ((dir (path-add-trailing-directory-separator dir)))
    (reduce append '() 
            (map (lambda (name) 
                   (let* ((f (string-append dir name))
                          (childs (if (and recursive (directory? f))
                                      (fileset dir: (path-add-trailing-directory-separator f)
                                               test: test
                                               recursive: recursive)
                                      '())))
                     (if (test f)
                         (cons f childs)
                         childs)))
                 (directory-files `(path: ,dir ignore-hidden: dot-and-dot-dot))))))

;;------------------------------------------------------------------------------
;;!! File handling

(define (path-add-trailing-directory-separator dir)
  (string-append (path-strip-trailing-directory-separator dir) "/"))

(define (directory? name)
  (eq? (file-type name) 'directory))

(define (regular? name)
  (eq? (file-type name) 'regular))

;;! Make directory
(define (make-directory dir)
  (let ((dir0 (path-strip-trailing-directory-separator dir)))
    (if (file-exists? dir0) #t
        (begin
          (make-directory (path-directory dir0))
          (create-directory dir0)))))

;;! Improved delete-file
(define* (delete-file file (recursive: #f) (force: #t))
  (let ((file (path-expand file)))
    (filesystem:info "deleting " file)
    (cond
     ((not (file-exists? file)) 'ok)
     ((directory? file)
      (delete-directory file recursive: recursive force: force))
     (else
      (##delete-file file)))))

;;! Improved delete-directory
(define* (delete-directory dir (recursive: #f) (force: #t))
  (if force (for-each ##delete-file (fileset dir: dir recursive: #f test: regular?)))
  (if recursive (for-each (lambda (dir) (delete-file
                                    (path-add-trailing-directory-separator dir)
                                    recursive: recursive
                                    force: force))
                          (fileset dir: dir recursive: #f test: directory?)))
  (if (null? (fileset dir: dir recursive: #t test: any?)) 
      (##delete-directory dir)
      (filesystem:warn dir " is not empty")))

;;! Delete a list of files
(define (delete-files files)
  (for-each (lambda (f) (delete-file f recursive: #t)) files))

;;! Improved copy-file
(define* (copy-file file dest (force: #f))
  (let ((file (path-expand file)))
    (cond
     ((directory? file)
      (filesystem:info "copying " file " to " dest)
      (copy-directory file dest force: force))
     ((and force (file-exists? dest))
      (delete-file dest recursive: #t)
      (copy-file file dest force: #f))
     ((not (file-exists? dest))
      (filesystem:info "copying " file " to " dest)
      (##copy-file file dest))
     (else
      (filesystem:warn dest " already exists")))))

;;! Copy a directory
(define* (copy-directory file dest (force: #f) (merge: #t))
  (if (and force merge)
      (begin (filesystem:warn "You can't both force and merge when copying directories, merge chosen for safety.")
             (set! force #f)))
  (cond
   ((and force (file-exists? dest))
    (delete-file dest recursive: #t force: #t)
    (copy-directory file dest force: force))
   ((or merge (not (file-exists? dest)))
    (if (not (file-exists? dest)) (create-directory dest))
    (for-each
     (lambda (filename)
       (copy-file filename
                  (string-append
                   (path-strip-trailing-directory-separator dest)
                   "/" (path-strip-directory filename))))
     (fileset dir: file recursive: #f)))
   (else
    (filesystem:warn dest " already exists"))))

;;! Copy a list of files and directories
(define* (copy-files files dest (force: #f))
  (for-each
   (lambda (file) 
     (copy-file file
                (string-append (path-strip-trailing-directory-separator dest)
                               "/"
                               (path-strip-directory file))
                force: force))
   files))

(define (read-file file)
  (call-with-input-file (path-expand file)
    (lambda (in) (read-line in #f))))

(define (read-files files)
  (call-with-output-string
   ""
   (lambda (out)
     (for-each (lambda (file) (display (read-file file) out)) files))))

(define (append-files files dest)
  (call-with-output-file dest
    (lambda (out)
      (for-each (lambda (file) (display (read-file file) out)) files))))
