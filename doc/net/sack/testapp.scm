(import src/server)

(sack-start!
 (lambda (env)
   (let ((ret #t))
     (values 200
             '(("Content-Type" . "text/plain"))
             (lambda ()
               (and
                ret
                (begin
                  (set! ret #f)
                  (with-output-to-u8vector
                   (list char-encoding: 'UTF-8)
                   (lambda ()
                     (write (env 'sack:uri)) (print "Hello, world!")))))))))
 port-number: 3333)

(import src/server)

(sack-start!
 (lambda (env)
   (let ((ret #t))
     (values 200
             '(("Content-Type" . "text/html"))
             (lambda ()
               (and
                ret
                (begin
                  (set! ret #f)
                  (with-output-to-u8vector
                   (list char-encoding: 'UTF-8)
                   (lambda ()
                     (print "<pre>\n") (write (##vector->list (env 'sack:uri))) (print "</pre>\n")
                     (print "<form action=. method=get>"
                            "<textarea name=c></textarea><input type=submit />"
                            "</form>")))))))))
 port-number: 3334)

; Test of reading the HTTP request body. This is done with the 'sack:body environment variable, as follows:
(import src/server)
(define console-output-port (current-output-port))
(sack-start!
 (lambda (env)
   (let ((ret #t))
     (with-output-to-port console-output-port (lambda () (print "Testapp got from HTTP request URI:") (write (env 'sack:uri)) (print "\n")))
     ((env 'sack:body) (lambda (u8v)
                         (with-output-to-port console-output-port (lambda () (print "Testapp got from HTTP request u8v: ") (write u8v) (print "\n")))
                         ; [Returning #f here would make sack:body cancel.]
                         ))
     ; Another way of invoking sack:body would be this:
     ; ((env 'sack:body) (lambda (u8v len)
     ;                     ; [We now have access to the bytes 0 to len - 1 in u8v, up to the point that this procedure returns.
     ;                     ;  unlike in the sack:body use example above, Sack did not make a special u8vector for our procedure
     ;                     ;  here, but we just got a copy of Sack's internal read buffer as to read out all we want from it,
     ;                     ;  up to and only up to that we return back from this procedure.]
     ;                     )
     ;                   copying?: #f)
     (values 200
             '(("Content-Type" . "text/html"))
             (lambda ()
               (and
                ret
                (begin
                  (set! ret #f)
                  (with-output-to-u8vector
                   (list char-encoding: 'UTF-8)
                   (lambda ()
                     ; (write (env 'sack:uri))
                     (print "<form action=. method=post>"
                            "<textarea name=c></textarea><input type=submit />"
                            "</form>")))))))))
 port-number: 3335)

; Test file upload using the form decoding mechanism.
; Is form decoding mature for production use?
(import src/server src/mime2 src/form (std string/util misc/u8v srfi/13))
(define console-output-port (current-output-port))
(define (form-post-decode* thunk)
  (lambda (env)
    (form-post-decode env thunk)))
(sack-start!
 (form-post-decode*
  (lambda (env)
    (let ((ret #t))
     (values 200
             '(("Content-Type" . "text/html"))
             (lambda ()
               (and
                ret
                (begin
                  (set! ret #f)
                  (with-output-to-u8vector
                   (list char-encoding: 'UTF-8)
                   (lambda ()
                     (print port: console-output-port "Successfully reached \n")
                     ; (write (env 'sack:uri))
                     (print "Form data: ") (write (env 'form:data))
                     (print "<hr/><form action=\".\" method=\"post\" enctype=\"multipart/form-data\">"
                            "<textarea name=c></textarea><input type=file name=filen /><input type=submit />"
                            "</form>"))))))))))
 port-number: 3336)
