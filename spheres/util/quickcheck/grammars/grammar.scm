;; Internal grammar converter. Currently not supported (depends on the Futhark Ansuz project)

(include "~~futhark/ansuz/sources/port#.scm")
(include "~~futhark/ansuz/char-stream-parser#.scm")
(include "~~futhark/ansuz/re#.scm")

(define-parser (nonterminal)
  (get #\<)
  (<- s (regexp "[a-zA-z0-9\\-_]*"))
  (get #\>)
  (return (if (equal? s "start") 'S 
              (string->symbol s))))

(define-parser (grammar-rule)
  (spaces)
  (get #\{)
  (spaces)
  (<- symbol (nonterminal))
  (spaces)
  (<- productions (list-of-productions))
  (spaces)
  (get #\})
  (return (map (lambda (p) `(,symbol ,@p)) productions)))

(define-parser (list-of-productions) 
  (kleene
   (>> (<- p (production))
       (get #\;)
       (spaces)
       (return p))))

(define-parser (production) 
  (kleene (>> (<- x 
                  (<> (terminal)
                      (nonterminal)))
              (spaces)
              (return x))))

(define-parser (terminal)
  (<- str (regexp "[~;\\<\\}]+"))
  (return (replace-tabs! str)))

(define-parser (comment)
  (regexp "[~\\{]*"))

(define (replace-tabs! str)
  (let repl ((j 0))
    (if (< j (string-length str))
        (let((c (string-ref str j)))
          (if (char=? c #\tab) (string-set! str j #\space))
          (repl (+ j 1)))
        str)))

(define-parser (grammar-rules)
  (<- rules 
      (kleene (>> (comment) 
                  (<- rule (grammar-rule)) 
                  (comment)
                  (return rule))))
  (eos)
  (return (apply append rules)))

(define-parser (spaces) 
  (regexp "[\t\n ]*"))

(define (read-grammar #!optional (port (current-input-port)))
  (run (grammar-rules) port))

	    

