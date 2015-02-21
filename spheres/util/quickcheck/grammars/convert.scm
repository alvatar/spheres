;; Internal grammar converter. Currently not supported (depends on the Futhark Ansuz project)

(include "grammar.scm")

(define (convert #!optional (dir (current-directory) ))
  (for-each (lambda (file)
	      (if (grammar-file? file) 
		  (convert-grammar-file file)))
	    (directory-files dir)))


(define (grammar-file? file)
  (and (char=? (string-ref file (- (string-length file) 2)) #\.)
       (char=? (string-ref file (- (string-length file) 1)) #\g)))

(define (convert-grammar-file file)
  (let*((name (substring file 0 (- (string-length file) 2)))
	(out (string-append name ".scm")))
    (call-with-output-file out
	(lambda (port)
	  (pp `(define ,(string->symbol name)
		 (quote ,(call-with-input-file file read-grammar)))
	      port)))))

(convert)


	    
