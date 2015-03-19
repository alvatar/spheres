;;!! HTTP protocol utilities
;; .author Marc Feeley, 2005-2007
;; .author Per Eckerdal, 2008-2010
;; .author Mikael More, 2010-2013
;; .author Alvaro Castro-Castilla, 2015


(declare (block)
         (standard-bindings)
         (extended-bindings)
         (fixnum))


(define console-output-port (current-output-port))

(define crlf-s "\r\n")

;; This sequence is sent at the end of every chunked encoding submission
;; = (string->u8vector (string-append "0" crlf crlf))
(define ending-chunk-u8v '#u8(48 13 10 13 10))
(define ending-chunk-u8v-len 5)

;; Joins two alists of headers, the first one having precedence over
;; the other. TODO Right now the implementation rather quick&dirty.
(define (headers-join one two)
  (table->list (table-merge!
                (list->table one)
                (list->table two))))


;;-------------------------------------------------------------------------------
;; Status codes

(define http-status-code
  (let ((http-status-codes
         (list->table '((100 . "Continue")
                        (101 . "Switching Protocols")
                        (200 . "OK")
                        (201 . "Created")
                        (202 . "Accepted")
                        (203 . "Non-Authoritative Information")
                        (204 . "No Content")
                        (205 . "Reset Content")
                        (206 . "Partial Content")
                        (300 . "Multiple Choices")
                        (301 . "Moved Permanently")
                        (302 . "Found")
                        (303 . "See Other")
                        (304 . "Not Modified")
                        (305 . "Use Proxy")
                        (307 . "Temporary Redirect")
                        (400 . "Bad Request")
                        (401 . "Unauthorized")
                        (402 . "Payment Required")
                        (403 . "Forbidden")
                        (404 . "Not Found")
                        (405 . "Method Not Allowed")
                        (406 . "Not Acceptable")
                        (407 . "Proxy Authentication Required")
                        (408 . "Request Timeout")
                        (409 . "Conflict")
                        (410 . "Gone")
                        (411 . "Length Required")
                        (412 . "Precondition Failed")
                        (413 . "Request Entity Too Large")
                        (414 . "Request-URI Too Long")
                        (415 . "Unsupported Media Type")
                        (416 . "Requested Range Not Satisfiable")
                        (417 . "Expectation Failed")
                        (500 . "Internal Server Error")
                        (501 . "Not Implemented")
                        (502 . "Bad Gateway")
                        (503 . "Service Unavailable")
                        (504 . "Gateway Timeout")
                        (505 . "HTTP Version Not Supported"))
                      test: eq?)))   ; eq? for fixnum HTTP status code
    (lambda (num)
      (table-ref http-status-codes num))))


;;-------------------------------------------------------------------------------
;;!! Header writing functions.

;; Translates from our internal lower-case symbol representation of a HTTP header key to string form
;; with the ordinarily applied casing.
(define (header-key->string key)
  (case key
    ((accept           ) "Accept"           )
    ((accept-charset   ) "Accept-Charset"   )
    ((accept-encoding  ) "Accept-Encoding"  )
    ((accept-language  ) "Accept-Language"  )
    ((cache-control    ) "Cache-Control"    )
    ((content-length   ) "Content-Length"   )
    ((content-type     ) "Content-Type"     )
    ((cookie           ) "Cookie"           )
    ((host             ) "Host"             )
    ((if-modified-since) "If-Modified-Since")
    ((if-none-match    ) "If-None-Match"    )
    ((pragma           ) "Pragma"           )
    ((referer          ) "Referer"          )
    ((user-agent       ) "User-Agent"       )
    ((symbol? key)       (symbol->string key))
    (else                key)))

(define (display-crlf io-primitives . msgs)
  (let ((display (io-primitives-display io-primitives)))
    (if (not (null? msgs))
        (for-each (lambda (msg)
                    (display msg))
                  msgs))
    (display "\r\n")))

(define (display-header io-primitives pair)
  (display-crlf io-primitives (car pair) ": " (cdr pair)))

(define (display-headers io-primitives hs)
  (for-each (lambda (x)
              (display-header io-primitives x))
            hs))

(define hex "0123456789abcdef")

(define (http-chunk-hex-formatter num)
  (let ((gssymlen 16))                  ; = (string-length hex)
    (reverse-list->string
     (let loop ((num num))
       (let ((idx (modulo num gssymlen)))
         (cons (string-ref hex idx)
               (if (eq? idx num)
                   '()
                   (loop (/ (- num idx) gssymlen)))))))))

;; Procedure writing a HTTP body in chunked encoding.
;; Used by |handle-sack-response| of the server module to write HTTP response body in chunked encoding.
;; Used by http-client module to write HTTP request body in chunked encoding.
;;
;; => boolean, #t = written successfully
(define (http-write-with-chunked-encoding io-primitives first-chunk get-chunk)
  (define display           (io-primitives-display           io-primitives))
  (define write-subu8vector (io-primitives-write-subu8vector io-primitives))
  (define force-output      (io-primitives-force-output      io-primitives))
  ;; => #t = written successfully
  (define (send! chunk not-first?)
    (http-util#chunk-return-value->u8v&u8v-length
     chunk
     ;; We must not send a chunk of zero length,
     ;; since that means the end of the response.
     (if (not (eq? 0 u8v-length))
         (and (if not-first?
                  ;; Putting it here conditioned like this instead of after writing the chunk, without
                  ;; a condition, saves us of doing a superfluous force-output call at the end
                  ;;
                  ;; XXX TODO When we discontinue the use of io-primitives, then add an 1 argument here
                  (force-output)
                  #t)
              (force-output)
              (display (string-append (http-chunk-hex-formatter u8v-length) crlf-s))
              (write-subu8vector u8v 0 u8v-length)
              (display crlf-s))
         #t)))
  (and
   ;; Write all chunks.
   ;; => boolean, #t = all chunks to be written were written.
   (let loop ((chunk first-chunk) (not-first? #f))
     (if chunk
         (and (send! chunk not-first?)
              (loop (get-chunk) #t))
         #t))
   ;; Send the last chunk
   (write-subu8vector ending-chunk-u8v 0 ending-chunk-u8v-len) ; = (display (string-append "0" crlf crlf))
   ;; XXX TODO When we discontinue the use of io-primitives, then add an 1 argument here
   (force-output)))


;;-------------------------------------------------------------------------------
;;!! Http reading functions.

(define (find-char-pos str char)
  (let loop ((i 0))
    (if (< i (string-length str))
        (if (char=? char (string-ref str i))
            i
            (loop (+ i 1)))
        #f)))

;; (split-attribute-line "Content-Length: 10") => '("content-length" . "10")
(define (split-attribute-line line)
  (let ((pos (find-char-pos line #\:)))
    (and pos
         (< pos (string-length line))
         (cons (let ((str (substring line 0 pos)))
                 (string-downcase! str)
                 str)
               (string-strip
                (substring line (+ pos 1) (string-length line)))))))

;; => string = Non-empty line read
;;    #f     = Reached EOF.
(define (read-line-until-crlf/skip-empty-line/-s io-primitives)
  ;; The #f tells IO primitives to return #f instead of the line as far
  (let ((read-line-until-crlf ((io-primitives-read-line-until-crlf io-primitives) #f)))
    ;; as it was read, in case of EOF.
    (let loop ()
      (let ((line (read-line-until-crlf)))
        (cond ((not line)
               #f)
              ((eq? 0 (string-length line))
               (loop))
              (else line))))))

;; Reads one line from port, using the byte reading functions and not read-line. Safe to US ASCII
;; only. This function also treats a CRLF followed by a space or a tab as whitespace, not a newline.
;;
;; Used when parsing headers, by |read-headers| only.
;;
;; => '(done . "") = Read an empty line (this is how a headers block ends)
;;    '(last-peeked-byte . line-read-s) = Read a line, and we know this was not at the end of the stream (EOF is not reached right after it ends)
;;    '(#f . line-read-s) = Read a line and we know this was the end of the stream (we reached EOF right after the line ended).
;; The name of |read-line-until-crlf| used to be |permissive-read-line| so this name was made in relation with that.
(define (permissive-read-line-skip-lws io-primitives peek-char)
  (let ((read-u8 (io-primitives-read-u8 io-primitives)))
    (define (eof? byte) (memq byte '(#f #!eof))) ; TODO clarify as Gambit ports get |io-error| attribute and we use it.
    (let loop ((lst '())
               (current-char peek-char)
               (peek-byte (read-u8)))
      (cond
       ;; If we hit a blank line (i.e. this line started with \r\n), return that we're done.
       ((and (null? lst)
             (eq? 10 peek-byte)           ;; #\newline
             (eq? #\return current-char)) ; = 13
        '(done . ""))

                                        ; If we ran into EOF now, then return `(#!eof . ,the-last-line-read/without-crlf)
       ((eof? peek-byte)
        (cons #f
              (reverse-list->string
               (if (and (eq? #\newline current-char)
                        (pair? lst)
                        (eq? #\return (car lst)))
                   (cdr lst)
                   (cons current-char lst)))))
       ;; If we reached a newline (\r\n) and the new line does not start with a tab or space,
       ;; then return '(peek-byte . line-read)
       ((and (eq? current-char #\newline)
             (pair? lst)
             (eq? (car lst) #\return)
             (not (or (eq? 9 peek-byte)     ;; #\tab
                      (eq? 32 peek-byte)))) ;; #\space
        (cons peek-byte
              (reverse-list->string (cdr lst))))
       ;; Continue reading.
       (else
        (let ((chr (integer->char peek-byte)))
          (loop (cons current-char lst)
                chr
                (read-u8))))))))

;; => alist of (downcased-header-key-s . header-value-s) = Headers block was successfully read.
;;    #f = Reached EOF before headers were read completely, or reached an invalid header row.
(define (read-headers io-primitives)
  ;; (print port: console-output-port "Into read-headers\n") (force-output console-output-port 1)
  (define (eof? byte) (memq byte '(#f #!eof))) ; TODO clarify as Gambit ports get |io-error| attribute and we use it.
  (let ((read-u8 (io-primitives-read-u8 io-primitives)))
    (let loop ((attributes '()) (peek-byte (read-u8)))
      ;; (print port: console-output-port "Got byte " peek-byte "\n") (force-output console-output-port 1)
      (and (not (eof? peek-byte))
           (let* ((peek-byte/line-pair
                   (permissive-read-line-skip-lws io-primitives (integer->char peek-byte)))
                  (new-peek-byte (car peek-byte/line-pair))
                  (line          (cdr peek-byte/line-pair)))
             ;; (print "peek-byte/line-pair " peek-byte/line-pair "\n")
             (cond
              ;; If we reached the end of the headers block (represented by an empty line), success. Return the headers read.
              ((eq? 'done new-peek-byte) attributes)
              ;; If we reached EOF while still reading headers, failure. Return #f.
              ((not new-peek-byte) #f)
              ;; We read another line of headers. Add it to the list of read headers and recur, unless the header line
              ;; was invalid in content (did not conform to the Key [Colon] Value format), in which case we return #f.
              (else (let ((attribute (split-attribute-line line)))
                      (if attribute
                          (loop (cons attribute attributes) new-peek-byte)
                          #f)))))))))

(define (read-content-chars port attributes)
  (let ((cl
         (cond ((assoc "content-length" attributes)
                =>
                (lambda (x)
                  (let ((n (string->number (cdr x))))
                    (and n (integer? n) (exact? n) n))))
               (else
                #f))))
    (if cl
        (let ((str (make-string cl)))
          (let ((n (read-substring str 0 cl port)))
            (if (= n cl)
                str
                "")))
        "")))

;; HTTP 1.1's chunked encoding means that the request or response body is transmitted in "chunks",
;; of which each starts with a text input line which starts with the chunk's size in bytes in HEX,
;; and the line is then followed by the chunk contents as binary data.
;;
;; This procedure extracts this chunk byte size number from an input chunk header line string.
;;
;; The chunked encoding specification supports sending inline chunked encoding arguments, by adding
;; a ; or space after the hexadecimal number and inserting those arguments there. Not clear right
;; now what they are for. Anyhow they are not relevant for our primary purposes and this procedure
;; ignores them.
;;
;; => fixnum = The read number.
;;    #f = Broken input data
;;
;; This procedure expects the str argument to be a string and will throw exception if it's not.
;;
;; (Currently we return 0 for empty input line, is this what we want?)
(define chunked-coding-read-hex
  (let ( ;; We keep these global as to be sure they're not re-evaluated and
        (zero (char->integer #\0))
        ;; re-allocated on each invocation, that would be a waste as this
        (nine (char->integer #\9))
        (a    (char->integer #\a)) ; procedure is invoked very frequently.
        (A    (char->integer #\A))
        (f    (char->integer #\f))
        (F    (char->integer #\F)))
    (lambda (str)
      (let* ((str-len (string-length str))
             (chr-lst (let loop ((lst '()) (idx 0))
                        ;; If we reached the end or string or a ; or space, return lst.
                        (if (or (>= idx str-len)
                                (let ((chr (string-ref str idx)))
                                  (or (char=? #\; chr)
                                      (char=? #\space chr))))
                            lst
                            ;; Otherwise, add current char to beginning of lst add go to next idx.
                            (loop (cons (char->integer (string-ref str idx)) lst)
                                  (+ 1 idx))))))
        (let loop ((lst chr-lst) (multiple 1))
          (if (null? lst)
              0 ; Is this really the best way to handle it, shouldn't we return #f here?
              (let* ((chr (car lst))
                     (current-digit (cond
                                     ((and (>= chr zero)
                                           (<= chr nine))
                                      (- chr zero))
                                     ((and (>= chr a)
                                           (<= chr f))
                                      (+ 10 (- chr a)))
                                     ((and (>= chr A)
                                           (<= chr F))
                                      (+ 10 (- chr A)))
                                     (else
                                      ;; We getting invalid input data is a completely ordinary event, that we should
                                      ;; respond to by returning #f, which is a very reasonable way to signal that
                                      ;; the input was of invalid format.
                                      ;; (error "Invalid character in hex string" str)
                                      #f))))
                (and current-digit ; #f current digit means reading error, means return.
                     (+ (loop (cdr lst) (* multiple 16))
                        (* multiple current-digit))))))))))
