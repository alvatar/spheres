;;!!! SRFI-28 Basic Format Strings

;;! srfi-28/format
;; .author Scott G. Miller, 2002. Reference implementation
;; .author Jürgen Geßwein, 2014. Current implementation, with bug fixes
;; .notes Optimization: The literal parts of the format string could be
;; written using write-substring. This could save some "expensive" trips
;; to the port functions.  I did not implement this yet.
(define (format format-string . objects)
  ;; Indexed-object always uses the original list of arguments to retrieve an
  ;; object by index.  This makes it easier to write error messages.
  (define (indexed-object i format-length)
    (let posloop ((j (+ i 1)))
      (if (>= j format-length)
          (error 'format "Incomplete escape sequence")
          (let ((d (string-ref format-string j)))
            (cond
             ((char-numeric? d) (posloop (+ j 1)))
             ((char=? d #\@)
              (if (and (< (+ j 1) format-length)
                       (char=?
                        (string-ref format-string (+ j 1))
                        #\*))
                  (values (+ j 2)
                          (list-ref objects
                                    (string->number
                                     (substring format-string i j))))
                  (error 'format "Incomplete escape sequence")))
             (else (error "Malformed argument index")))))))
  (define (consume objs obj proc)
    (cond
     (obj (proc obj) objs)
     ((null? objs) (error 'format "No value for escape sequence"))
     (else (proc (car objs))
           (cdr objs))))
  (with-output-to-string
    '()
    (lambda ()
      (let ((format-length (string-length format-string)))
        (let loop ((i 0)
                   (objects objects)
                   (obj #f))
          (if (>= i format-length)
              #t
              (let ((c (string-ref format-string i)))
                (if (char=? c #\~)
                    (if (>= (+ i 1) format-length)
                        (error 'format "Incomplete escape sequence")
                        (let ((c (string-ref format-string (+ i 1))))
                          (if (char-numeric? c)
                              (call-with-values
                                  (lambda ()
                                    (indexed-object (+ i 1) format-length))
                                (lambda (idx obj)
                                  (loop idx objects obj)))
                              (case c
                                ((#\a)
                                 (loop (+ i 2)
                                       (consume objects obj display)
                                       #f))
                                ((#\s)
                                 (loop (+ i 2)
                                       (consume objects obj write)
                                       #f))
                                ((#\x)
                                 (loop (+ i 2)
                                       (consume objects obj
                                                (lambda (n)
                                                  (display
                                                   (number->string
                                                    (car objects) 16))))
                                       #f))
                                ((#\%)
                                 (newline)
                                 (loop (+ i 2) objects obj))
                                ((#\~)
                                 (write-char #\~)
                                 (loop (+ i 2) objects obj))
                                (else
                                 (error 'format "Unrecognized escape sequence:"
                                        (string-ref format-string (+ i 1))))))))
                    (begin
                      ;; Pass any indexed object along so that it can be used
                      ;; by the next argument (cf. ~~ and ~% above).  This
                      ;; makes writing error messages less error-prone.
                      (write-char c)
                      (loop (+ i 1) objects obj))))))))))
