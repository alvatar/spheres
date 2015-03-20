;;!!! x-www-form-urlencoded encoding and decoding.
;; .author Marc Feeley, 2005-2006
;; .author Per Eckerdal, 2008-2009
;; .author Mikael More, 2010-2012
;; .author Alvaro Castro-Castilla, 2015
;;
;; Performs (urldecode) and (urlencode) string operations for the UTF-8 and ISO8859 charsets,
;; and performs encoding and decoding of x-www-form-urlencoded HTTP POST forms.
;;
;; Written by Marc Feeley for http-server, made to a separate module
;; and refactored by Per Eckerdal. Minor tweaks and improvements by Mikael More.
;;
;; Copyright (c) 2008-2009 Per Eckerdal, 2005-2007 Marc Feeley,
;; 2010, 2012 Mikael More, All Rights Reserved.
;;
;; ## Exports
;; # Urlencoding
;; (urlencode str) => str
;; Performs a UTF-8 urlencoding of the input string, and returns the encoded version in string format.
;;
;; (urlencode-ISO8859 str) => str
;; Urlencodes the passed string and returns the urlencoded form. Has sth to do with ISO8859 encoding, but what?
;;
;; (write-urlencoded str) => #!void
;; (write-urlencoded-ISO8859 str) => #!void
;; Write the passed string in urlencoded format to the curent output port. Has sth to do with ISO8859 encoding, but what?
;;
;; (write-urlencoded-u8vector u8v) => #!void
;; Write the passed u8vector in urlencoded format to the current output port. Does encoding from the source on a per-byte
;; basis, so effectively that means ISO8859 encoding.
;;
;; (write-urlencoded-u8vector-lambda #!optional dontencode-clause)
;; Macro that defines a write-urlencoded-u8vector procedure. |dontencode-clause| is a cond test clause
;; where the current byte |b| is processed, and if #t is returned, the byte is not encoded.
;;
;; This is of use at least in the uri module, where a custom urlencoder is implemented.
;;
;; (urlencode->u8vector str) => u8vector
;; UTF-8 urlencodes the input string and returns the urlencoded output as a u8vector.
;; (This one actually uses write-urlencoded-u8vector internally for the urlencoding work.)
;;
;; # Urldecoding
;; (urldecode str #!optional (start 0) end) => str
;; Takes an urlencoded string as input, and returns the UTF8-urldecoded result as a string.
;;
;; (urldecode-ISO8859 str) => str
;; Takes an urlencoded string as input, and returns the ISO8859-urldecoded result.
;;
;; (urldecode->u8vector str #!optional (start 0) end) => u8v
;; Takes an urlencoded string as input, and returns the urldecoded result as an u8vector.
;; Note that since output is u8vector there's no regard within this routine to char encoding of the encoded material
;; (UTF-8/ISO8859 etc.).
;;
;; # x-www-form-urlencoded form encoding
;; (write-x-www-form-urlencoded/ISO-8859-1 fields) => #!void
;; (write-x-www-form-urlencoded/UTF-8 fields) => #!void
;; Encodes fields (a-list with string keys and values) and writes the result to the current output.
;;
;; (encode-x-www-form-urlencoded/ISO-8859-1 fields) => string
;; Encodes fields (a-list with string keys and values) and returns the result as a string, e.g.:
;; (encode-x-www-form-urlencoded/UTF-8 '(("a" . "b") ("c" . "d\345\344\366"))) => "a=b&c=d%C3%A5%C3%A4%C3%B6"
;;
;; # x-www-form-urlencoded form decoding
;; (decode-x-www-form-urlencoded/ISO-8859-1 string) => fields
;; (decode-x-www-form-urlencoded/UTF-8 string) => fields
;; Decodes string, being in x-www-form-urlencoded format, and returns the contained fields as an alist of string keys
;; and values, e.g.:
;; (decode-x-www-form-urlencoded/ISO-8859-1 "a=b&c=d%E5%E4%F6"         ) => '(("a" . "b") ("c" . "d\345\344\366"))
;; (decode-x-www-form-urlencoded/UTF-8      "a=b&c=d%C3%A5%C3%A4%C3%B6") => '(("a" . "b") ("c" . "d\345\344\366"))
;;
;; (sack-app-give-x-www-form-urlencoded/ISO-8859-1-form-post-decoder sack-app-thunk #!optional (threadsafe? #t))
;; => sack-app-thunk
;; (sack-app-give-x-www-form-urlencoded/UTF-8-form-post-decoder      sack-app-thunk #!optional (threadsafe? #t))
;; => sack-app-thunk
;; Extends the environment (|env|) of the passed sack-app with a 'form:data property, which returns the fields of
;; the form passed. The form data is processed lazily, on the first get of the 'form:data property. Please note that
;; this environment extension is not combinable with any other use of the 'sack:body environment property.
;;
;; threadsafe? regards if the lazy get mechanism should be surrounded by a mutex. In particular considering this is
;; quite inexpensive anyhow (costing a mutex allocation per sack app invocation), it's justified enough to turn on
;; by default as a cheap insurance (just in case the form would be read doubly internally, I suppose there'd be
;; an exception or at least invalid output data), though threadsafe? can safely be turned off for any ordinary
;; sack app that gets 'form:data from one thread only, thus saving the mutex allocation per sack app invocation and
;; the two mutex lock ops on the actual get.
;;      A really performance hungry application might just choose to invoke
;; application/x-www-form-urlencoded/*-data-form-post-decode only on the HTTP requests where it's needed of course.
;;
;; (application/x-www-form-urlencoded/ISO8859-data-form-post-decode env)
;; => fields, or #f if there was no x-www-form-urlencoded form data (as identified by content-type)
;; (application/x-www-form-urlencoded/UTF-8-data-form-post-decode   env) => fields
;; => fields, or #f if there was no x-www-form-urlencoded form data (as identified by content-type)
;; Performs the actual reading of application/x-www-form-urlencoded HTTP request body data and processes it into a
;; fields value, which is returned. Note that this procedure can only be invoked once during the course of a sack-app
;; and is not combinabel with any other use of the 'sack:body environment property.
;;
;; ## TODO / Possible developments
;;  * Possible development would be increased speed by making |urlencode| not use any intermediary format in the
;;    encoding process.
;;  * Possible performance improvement possible for decode-x-www-form-urlencoded/UTF-8 , see its comments for more info.
;;  * Make urldecode support the "%u1234" format as reflected on
;;    http://www.skorks.com/2009/08/different-types-of-encoding-schemes-a-primer/ .
;;


(declare (block)
         (standard-bindings)
         (extended-bindings)
         (mostly-fixnum))


;; Util routine.
(define (hex str n i)
  (if (fx< (fx+ i 1) n)
      (let ((h1 (nibble str i))
            (h2 (nibble str (fx+ i 1))))
        (and h1 h2 (fx+ (fx* h1 16) h2)))
      #f))

;; Util routine.
(define (nibble str i)
  (let ((c (string-ref str i)))
    (cond ((and (char>=? c #\0) (char<=? c #\9))
           (fx- (char->integer c) (char->integer #\0)))
          ((and (char>=? c #\a) (char<=? c #\f))
           (fx+ 10 (- (char->integer c) (char->integer #\a))))
          ((and (char>=? c #\A) (char<=? c #\F))
           (fx+ 10 (- (char->integer c) (char->integer #\A))))
          (else
           #f))))

;; write ISO8859-characters string str to ISO8859-urlencoded format to current-output-port
(define (write-urlencoded-ISO8859 str)
  (define (write-nibble n)
    (write-char (##string-ref "0123456789ABCDEF" n)))
  (let loop ((i 0))
    (if (< i (string-length str))
        (let ((c (string-ref str i)))
          (cond ((or (and (char>=? c #\a) (char<=? c #\z))
                     (and (char>=? c #\A) (char<=? c #\Z))
                     (and (char>=? c #\0) (char<=? c #\9))
                     (char=? c #\_))
                 (write-char c))
                ((char=? c #\space)
                 (write-char #\+))
                (else
                 (let ((n (char->integer c)))
                   (write-char #\%)
                   (write-nibble
                    (bitwise-and (arithmetic-shift n -4) 15))
                   (write-nibble (bitwise-and n 15)))))
          (loop (+ i 1))))))

(define write-urlencoded-u8vector (write-urlencoded-u8vector-lambda))

(define (urlencode->u8vector str)
  (with-output-to-u8vector
   '()
   (lambda () (write-urlencoded-u8vector (string->utf8-u8vector str)))))

;; Performs an UTF8-urlencoding, which is the common standard on the Internet today.
(define (urlencode str)
  (utf8-u8vector->string (urlencode->u8vector str)))

(define (write-urlencoded str)
  (display (urlencode str)))

;; urlencode ISO8859-characters string str to ISO8859-urlencoded format.
;; This is not the common standard on the Internet today, but is used by some parties for instance
;; with legacy non-Unicode systems.
(define (urlencode-ISO8859 str)
  (with-output-to-string
    ""
    (lambda () (write-urlencoded-ISO8859 str))))

;; urldecode an utf8-encoded string, to string
(define* (urldecode str (start 0) (end #f))
  (utf8-u8vector->string (urldecode->u8vector str start end)))

;; urldecode an iso8859-encoded string to string
(define (urldecode-ISO8859 str)
  (let* ((len (string-length str))
         (ret (make-string len))
         (strpos 0))
    (let loop ((i 0))
      (if (not (fx>= i len))
          (let ((chr (string-ref str i)))
            (if (eq? chr #\%)
                (begin
                  (string-set! ret
                               strpos
                               (integer->char (hex str len (fx+ i 1))))
                  (set! strpos (fx+ strpos 1))
                  (loop (fx+ i 3)))
                (begin
                  (string-set! ret strpos chr)
                  (set! strpos (fx+ strpos 1))
                  (loop (fx+ i 1)))))))
    (substring ret 0 strpos)))

;; urldecode to u8vector
(define* (urldecode->u8vector str (start 0) (end #f))
  (let* ((end (or end (string-length str)))
         (len (fx- end start))
         ;; The idea here is that the output length for sure not will be larger
         ;; than the input length, as one byte input always results in *at most*
         ;; one byte output.
         (ret (make-u8vector len)))
    (let loop ((str-idx start) (u8vpos 0))
      (if (fx< str-idx end) ; Expresses the original (not (fx>= str-idx len)) but better.
                                        ; Continue iterating:
          (let ((chr (string-ref str str-idx)))
            (if (eq? chr #\%)
                (begin
                  (u8vector-set! ret
                                 u8vpos
                                 (hex str len (fx+ str-idx 1)))
                  (loop (fx+ str-idx 3) (fx+ u8vpos 1)))
                (begin
                  (u8vector-set! ret u8vpos (char->integer chr))
                  (loop (fx+ str-idx 1) (fx+ u8vpos 1)))))
                                        ; Return:
          (if (eq? u8vpos len)
              ret
              (subu8vector ret 0 u8vpos))))))

(define (write-x-www-form-urlencoded fields urlencode-proc)
  (define (write-field field)
    (urlencode-proc (car field))
    (write-char #\=)
    (urlencode-proc (cdr field)))
  (if (not (null? fields))
      (begin
        (let ((field1 (car fields)))
          (write-field field1)
          (for-each (lambda (field)
                      (write-char #\&)
                      (write-field field))
                    (cdr fields))))))

(define (write-x-www-form-urlencoded/ISO-8859-1 fields)
  (write-x-www-form-urlencoded fields write-urlencoded-ISO8859))

(define (write-x-www-form-urlencoded/UTF-8 fields)
  (write-x-www-form-urlencoded fields write-urlencoded))

(define (encode-x-www-form-urlencoded/ISO-8859-1 fields)
  (if (null? fields)
      ""
      (with-output-to-string
        ""
        (lambda ()
          (write-x-www-form-urlencoded fields write-urlencoded-ISO8859)))))

(define (encode-x-www-form-urlencoded/UTF-8 fields)
  (if (null? fields)
      ""
      (with-output-to-string
        ""
        (lambda ()
          (write-x-www-form-urlencoded fields write-urlencoded)))))

(define decode-x-www-form-urlencoded/ISO-8859-1
  (lambda (str)
    (let ((strlen (string-length str)))
      ;; This is the procedure for extracting a field key or field value, and returning a string for it.
      (define extract
        (lambda (start len)
          (let ((s (make-string len)))
            (let loop ((i start) (j 0))
              (if (fx< j len)
                  (let ((c (string-ref str i)))
                    (cond ((char=? c #\%)
                           (cond ((hex (fx+ i 1))
                                  =>
                                  (lambda (x)
                                    (string-set! s j (integer->char x))
                                    (loop (fx+ i 3) (fx+ j 1))))
                                 (else
                                  #f)))
                          ((char=? c #\+)
                           (string-set! s j #\space)
                           (loop (fx+ i 1) (fx+ j 1)))
                          (else
                           (string-set! s j c)
                           (loop (fx+ i 1) (fx+ j 1)))))
                  s)))))
      (define hex
        (lambda (i)
          (if (fx< (fx+ i 1) strlen)
              (let ((h1 (nibble i))
                    (h2 (nibble (fx+ i 1))))
                (and h1 h2 (fx+ (fx* h1 16) h2)))
              #f)))
      (define nibble
        (lambda (i)
          (let ((c (string-ref str i)))
            (cond ((and (char>=? c #\0) (char<=? c #\9))
                   (fx- (char->integer c) (char->integer #\0)))
                  ((and (char>=? c #\a) (char<=? c #\f))
                   (fx+ 10 (- (char->integer c) (char->integer #\a))))
                  ((and (char>=? c #\A) (char<=? c #\F))
                   (fx+ 10 (- (char->integer c) (char->integer #\A))))
                  (else
                   #f)))))
      ;; Right at the beginning of the string
      ;; (Why there's an else clause here is quite funny, as rev-fields will always be empty just in case it's
      ;; executed.)
      (define state0
        (lambda (i rev-fields)
          (if (fx< i strlen)
              (state1 i
                      i
                      0
                      rev-fields)
              (reverse rev-fields))))
      ;; In field name. Essentially this procedure counts the characters that belong to the field name,
      ;; and when the counting is done it calls |extract| to extract the field name, and then passes
      ;; on execution to state2.
      (define state1
        (lambda (i start len rev-fields)
          (if (< i strlen)
              (let ((c (string-ref str i)))
                (cond ((char=? c #\=)
                       (state2 (fx+ i 1)
                               (fx+ i 1)
                               0
                               (extract start len)
                               rev-fields))
                      ((char=? c #\%)
                       (and (hex (fx+ i 1))
                            (state1 (fx+ i 3)
                                    start
                                    (fx+ len 1)
                                    rev-fields)))
                      (else
                       (state1 (fx+ i 1)
                               start
                               (fx+ len 1)
                               rev-fields))))
              #f)))
      ;; In field value. Essentially this procedure counts the characters that belong to the field value,
      ;; and then when the counting is done it calls |extract| to extract the field value, inserts this into
      ;; the produced results (using the |end-of-field| to cons on the result to rev-fields).
      (define state2
        (lambda (i start len name rev-fields)
          (define end-of-field
            (lambda ()
              (cons (cons name (extract start len))
                    rev-fields)))
          (if (fx< i strlen)
              (let ((c (string-ref str i)))
                (cond ((char=? c #\&)
                       (state1 (fx+ i 1)
                               (fx+ i 1)
                               0
                               (end-of-field)))
                      ((char=? c #\%)
                       (and (hex (fx+ i 1))
                            (state2 (fx+ i 3)
                                    start
                                    (fx+ len 1)
                                    name
                                    rev-fields)))
                      (else
                       (state2 (fx+ i 1)
                               start
                               (fx+ len 1)
                               name
                               rev-fields))))
              (reverse (end-of-field)))))
      (state0 0 '()))))

;; Currently we keep this one's implementation really basic: We basically imitate decode-x-www-form-urlencoded/UTF-8
;; but just output an u8vector instead, which we then run u8vector UTF8-urldecode on.
;;      This could be made much faster, by performing the actual UTF-8 decoding work in this procedure. The starting
;; point for this would be that state2 and state1 would count the string length (i.e. the len variable) in UTF-8
;; characters instead, and that extract then would start out with knowing the destination string length and it would
;; then just stream-decode into it.
;;      For implementing this, there would be benefit in clarifying the following questions:
;;  * Constituent of a field key or value's basis for UTF8-urldecoding may be *any* character except for & and = which
;;    are counted as control characters.
;;  * In case of broken encoding, we need an argument to specify behavior: Return #f or automatically switch to ISO-8859-1
;;    decoding.
;;  * Try to get a behavior such that *if there's UTF-8 chars already in the source text*, then let them remain intact in
;;    the result.
(define decode-x-www-form-urlencoded/UTF-8
  (lambda (str)
    (let ((strlen (string-length str)))
      ;; This is the procedure for extracting a field key or field value, and returning a string for it.
      (define extract
        (lambda (start len)
          (let ((s (make-u8vector len)))
            (let loop ((i start) (j 0))
              (if (fx< j len)
                  (let ((c (string-ref str i)))
                    (cond ((char=? c #\%)
                           (cond ((hex (fx+ i 1))
                                  =>
                                  (lambda (x)
                                    (u8vector-set! s j x)
                                    (loop (fx+ i 3) (fx+ j 1))))
                                 (else
                                  #f)))
                          ((char=? c #\+)
                           (u8vector-set! s j 32) ; #\space)
                           (loop (fx+ i 1) (fx+ j 1)))
                          (else
                           (u8vector-set! s j (char->integer c))
                           (loop (fx+ i 1) (fx+ j 1)))))
                  (utf8-u8vector->string s))))))
      (define hex
        (lambda (i)
          (if (fx< (fx+ i 1) strlen)
              (let ((h1 (nibble i))
                    (h2 (nibble (fx+ i 1))))
                (and h1 h2 (fx+ (fx* h1 16) h2)))
              #f)))
      (define nibble
        (lambda (i)
          (let ((c (string-ref str i)))
            (cond ((and (char>=? c #\0) (char<=? c #\9))
                   (fx- (char->integer c) (char->integer #\0)))
                  ((and (char>=? c #\a) (char<=? c #\f))
                   (fx+ 10 (- (char->integer c) (char->integer #\a))))
                  ((and (char>=? c #\A) (char<=? c #\F))
                   (fx+ 10 (- (char->integer c) (char->integer #\A))))
                  (else
                   #f)))))
      ;; Right at the beginning of the string
      ;; (Why there's an else clause here is quite funny, as rev-fields will always be empty just in case it's
      ;; executed.)
      (define state0
        (lambda (i rev-fields)
          (if (fx< i strlen)
              (state1 i
                      i
                      0
                      rev-fields)
              (reverse rev-fields))))
      ;; In field name. Essentially this procedure counts the characters that belong to the field name,
      ;; and when the counting is done it calls |extract| to extract the field name, and then passes
      ;; on execution to state2.
      (define state1
        (lambda (i start len rev-fields)
          (if (< i strlen)
              (let ((c (string-ref str i)))
                (cond ((char=? c #\=)
                       (state2 (fx+ i 1)
                               (fx+ i 1)
                               0
                               (extract start len)
                               rev-fields))
                      ((char=? c #\%)
                       (and (hex (fx+ i 1))
                            (state1 (fx+ i 3)
                                    start
                                    (fx+ len 1)
                                    rev-fields)))
                      (else
                       (state1 (fx+ i 1)
                               start
                               (fx+ len 1)
                               rev-fields))))
              #f)))
      ;; In field value. Essentially this procedure counts the characters that belong to the field value,
      ;; and then when the counting is done it calls |extract| to extract the field value, inserts this into
      ;; the produced results (using the |end-of-field| to cons on the result to rev-fields).
      (define state2
        (lambda (i start len name rev-fields)
          (define end-of-field
            (lambda ()
              (cons (cons name (extract start len))
                    rev-fields)))
          (if (fx< i strlen)
              (let ((c (string-ref str i)))
                (cond ((char=? c #\&)
                       (state1 (fx+ i 1)
                               (fx+ i 1)
                               0
                               (end-of-field)))
                      ((char=? c #\%)
                       (and (hex (fx+ i 1))
                            (state2 (fx+ i 3)
                                    start
                                    (fx+ len 1)
                                    name
                                    rev-fields)))
                      (else
                       (state2 (fx+ i 1)
                               start
                               (fx+ len 1)
                               name
                               rev-fields))))
              (reverse (end-of-field)))))
      (state0 0 '()))))

(define (application/x-www-form-urlencoded-data-form-post-decode decode-x-www-form-urlencoded-procedure env)
  ;; (print port: console-output-port "form-post: Into. Thread=#" (object->serial-number (current-thread)) "\n")
  ;; (print port: console-output-port "Content-Type: " content-type "\n")
  (let* ((http-request-contents-s
          (let ((p (open-output-u8vector)))
            ((env 'sack:body) (lambda (chunk)
                                ;; (dbg "form-post-decode data handler received from client u8vector packet: " chunk)
                                (write-subu8vector chunk 0 (u8vector-length chunk) p)))
            (let ((v (get-output-u8vector p)))
              (close-port p)
              ;; (print port: console-output-port "form-post-decode transfer data handler received from client: ")
              ;; (write v console-output-port)
              ;; (print port: console-output-port "\n")
              (utf8-u8vector->string v))))
         (content-type ((env 'sack:headers) "content-type")))
    (if (null? content-type)
        #f
        (let* ((content-type (car content-type))
               (parts (string-split-char #\; content-type)))
          (if ; (or (< (length parts) 1) - I have no clue what this conditional was for - the result of string-split-char cannot be
                                        ; zero so it's a pointless condition.
           (not (equal? "application/x-www-form-urlencoded" (car parts)))
           #f
           (let* ((args (map
                         (lambda (v)
                           (or (let ((v (string-split-at-first-nice #\= v)))
                                 (set-car! v (string-trim (car v) #\space))
                                 v)
                               (cons #f #f)))
                         ;; (boundary (assoc "boundary" args))
                         (cdr parts))))
             ;; (print port: console-output-port "args=")
             ;; (write args console-output-port)
             ;; (print port: console-output-port" boundary=" boundary "\n")
             ;; (if (not boundary)
             ;;     (continue)
             (let* ( ;; (boundary (cdr boundary))
                    (request-body-s http-request-contents-s)
                    ;; (request-body (u8vector-ensure-ends-by-cr-lf request-body))
                    ;; (mime-data
                    ;;  (mime-decode-multipart-with-boundary
                    ;;   request-body 0 (u8vector-length request-body) boundary #t))
                    (r (decode-x-www-form-urlencoded-procedure request-body-s)))
               ;; (display "form-post: Successfully parsed.\n" console-output-port)
               ;; (display "Input: request-body: " console-output-port)
               ;; (write request-body console-output-port)
               ;; (display " boundary: " console-output-port)
               ;; (write boundary console-output-port)
               ;; (print port: console-output-port "\n")
               ;; (display "Output: " console-output-port)
               ;; (write form-data console-output-port)
               ;; (print port: console-output-port "\n")
               r)))))))

(define (application/x-www-form-urlencoded/ISO8859-data-form-post-decode env)
  (application/x-www-form-urlencoded-data-form-post-decode decode-x-www-form-urlencoded/ISO-8859-1 env))

(define (application/x-www-form-urlencoded/UTF-8-data-form-post-decode env)
  (application/x-www-form-urlencoded-data-form-post-decode decode-x-www-form-urlencoded/UTF-8 env))

(define (sack-app-deliver-x-www-form-urlencoded-data-form-post-decoding decode-x-www-form-urlencoded-procedure env thunk threadsafe?)
  (let* ((form-data #f)
         (read-form-data-mutex (and threadsafe? (make-mutex)))
         (env (lambda a
                (if (equal? a '(form:data))
                    (or form-data
                        (let ((r (or (and threadsafe?
                                          (begin
                                            (mutex-lock! read-form-data-mutex)
                                            form-data))
                                     (begin
                                       (set! form-data (application/x-www-form-urlencoded-data-form-post-decode decode-x-www-form-urlencoded-procedure env))
                                       form-data))))
                          (if threadsafe? (mutex-unlock! read-form-data-mutex))
                          r))
                    (apply env a)))))
    (thunk env)))

(define* (sack-app-give-x-www-form-urlencoded/ISO-8859-1-form-post-decoder thunk (threadsafe? #t))
  (lambda (env)
    (sack-app-deliver-x-www-form-urlencoded-data-form-post-decoding decode-x-www-form-urlencoded/ISO-8859-1 env thunk threadsafe?)))

(define* (sack-app-give-x-www-form-urlencoded/UTF-8-form-post-decoder thunk (threadsafe? #t))
  (lambda (env)
    (sack-app-deliver-x-www-form-urlencoded-data-form-post-decoding decode-x-www-form-urlencoded/UTF-8 env thunk threadsafe?)))
