;;;============================================================================

;;; File: "mime.scm", Time-stamp: <2007-04-05 00:52:32 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Contains procedures to decode and encode data using the MIME format.

;;; See the following RFCs:
;;;
;;;   RFC 2045 - Multipurpose Internet Mail Extensions (MIME) Part One:
;;;   Format of Internet Message Bodies
;;;
;;;   RFC 2046 - Multipurpose Internet Mail Extensions (MIME) Part Two:
;;;   Media Types
;;;
;;;   RFC 2047 - MIME (Multipurpose Internet Mail Extensions) Part Three:
;;;   Message Header Extensions for Non-ASCII Text
;;;
;;;   RFC 2048 - Multipurpose Internet Mail Extensions (MIME) Part Four:
;;;   Registration Procedures
;;;
;;;   RFC 2049 - Multipurpose Internet Mail Extensions (MIME) Part Five:
;;;   Conformance Criteria and Examples
;;;
;;;   RFC 2822 - Internet Message Format


;;;============================================================================

(define-macro (ch-tab)         9)
(define-macro (ch-linefeed)    10)
(define-macro (ch-return)      13)
(define-macro (ch-space)       32)
(define-macro (ch-rubout)      127)

;; Because of Scheme48's encoding of characters we can't use this:
;;
;; (define-macro (ch-doublequote) (char->integer #\"))
;; (define-macro (ch-backslash)   (char->integer #\\))
;; (define-macro (ch-dot)         (char->integer #\.))
;; (define-macro (ch-colon)       (char->integer #\:))
;;
;; (define-macro (ch-exclam)      (char->integer #\!))
;; (define-macro (ch-sharp)       (char->integer #\#))
;; (define-macro (ch-dollar)      (char->integer #\$))
;; (define-macro (ch-percent)     (char->integer #\%))
;; (define-macro (ch-ampersand)   (char->integer #\&))
;; (define-macro (ch-quote)       (char->integer #\'))
;; (define-macro (ch-star)        (char->integer #\*))
;; (define-macro (ch-plus)        (char->integer #\+))
;; (define-macro (ch-minus)       (char->integer #\-))
;; (define-macro (ch-slash)       (char->integer #\/))
;; (define-macro (ch-equal)       (char->integer #\=))
;; (define-macro (ch-question)    (char->integer #\?))
;; (define-macro (ch-hat)         (char->integer #\^))
;; (define-macro (ch-underscore)  (char->integer #\_))
;; (define-macro (ch-backquote)   (char->integer #\`))
;; (define-macro (ch-lbrace)      (char->integer #\{))
;; (define-macro (ch-rbrace)      (char->integer #\}))
;; (define-macro (ch-vbar)        (char->integer #\|))
;; (define-macro (ch-tilde)       (char->integer #\~))
;;
;; (define-macro (ch-lower-a)     (char->integer #\a))
;; (define-macro (ch-upper-a)     (char->integer #\A))
;; (define-macro (ch-lower-f)     (char->integer #\f))
;; (define-macro (ch-upper-f)     (char->integer #\F))
;; (define-macro (ch-lower-z)     (char->integer #\z))
;; (define-macro (ch-upper-z)     (char->integer #\Z))
;; (define-macro (ch-digit-0)     (char->integer #\0))
;; (define-macro (ch-digit-9)     (char->integer #\9))
;;
;; (define-macro (ch-lbracket)    (char->integer #\[))
;; (define-macro (ch-rbracket)    (char->integer #\]))
;; (define-macro (ch-lparen)      (char->integer #\())
;; (define-macro (ch-rparen)      (char->integer #\)))
;; (define-macro (ch-langle)      (char->integer #\<))
;; (define-macro (ch-rangle)      (char->integer #\>))
;; (define-macro (ch-at)          (char->integer #\@))
;; (define-macro (ch-comma)       (char->integer #\,))
;; (define-macro (ch-semicolon)   (char->integer #\;))

(define-macro (ch-doublequote) 34)
(define-macro (ch-backslash)   92)
(define-macro (ch-dot)         46)
(define-macro (ch-colon)       58)

(define-macro (ch-exclam)      33)
(define-macro (ch-sharp)       35)
(define-macro (ch-dollar)      36)
(define-macro (ch-percent)     37)
(define-macro (ch-ampersand)   38)
(define-macro (ch-quote)       39)
(define-macro (ch-star)        42)
(define-macro (ch-plus)        43)
(define-macro (ch-minus)       45)
(define-macro (ch-slash)       47)
(define-macro (ch-equal)       61)
(define-macro (ch-question)    63)
(define-macro (ch-hat)         94)
(define-macro (ch-underscore)  95)
(define-macro (ch-backquote)   96)
(define-macro (ch-lbrace)      123)
(define-macro (ch-rbrace)      125)
(define-macro (ch-vbar)        124)
(define-macro (ch-tilde)       126)

(define-macro (ch-lower-a)     97)
(define-macro (ch-upper-a)     65)
(define-macro (ch-lower-f)     102)
(define-macro (ch-upper-f)     70)
(define-macro (ch-lower-z)     122)
(define-macro (ch-upper-z)     90)
(define-macro (ch-digit-0)     48)
(define-macro (ch-digit-9)     57)

(define-macro (ch-lbracket)    91)
(define-macro (ch-rbracket)    93)
(define-macro (ch-lparen)      40)
(define-macro (ch-rparen)      41)
(define-macro (ch-langle)      60)
(define-macro (ch-rangle)      62)
(define-macro (ch-at)          64)
(define-macro (ch-comma)       44)
(define-macro (ch-semicolon)   59)

(define eol-str
  (string (integer->char 13)
          (integer->char 10)))

(define (mime-eol-str)
  eol-str)

(define eol
  (ISO-8859-1-string->u8vector (mime-eol-str)))
        
(define dash-dash
  (ISO-8859-1-string->u8vector "--"))
        
(define (ttext? c)
  (if (or (<= c (ch-space))
          (>= c (ch-rubout))
          (= c (ch-lparen))
          (= c (ch-rparen))
          (= c (ch-langle))
          (= c (ch-rangle))
          (= c (ch-at))
          (= c (ch-comma))
          (= c (ch-semicolon))
          (= c (ch-colon))
          (= c (ch-backslash))
          (= c (ch-doublequote))
          (= c (ch-slash))
          (= c (ch-lbracket))
          (= c (ch-rbracket))
          (= c (ch-question))
          (= c (ch-equal)))
      #f
      c))

(define (atext? c)
  (if (or (and (>= c (ch-lower-a)) (<= c (ch-lower-z)))
          (and (>= c (ch-upper-a)) (<= c (ch-upper-z)))
          (and (>= c (ch-digit-0)) (<= c (ch-digit-9)))
          (= c (ch-exclam))
          (= c (ch-sharp))
          (= c (ch-dollar))
          (= c (ch-percent))
          (= c (ch-ampersand))
          (= c (ch-quote))
          (= c (ch-star))
          (= c (ch-plus))
          (= c (ch-minus))
          (= c (ch-slash))
          (= c (ch-equal))
          (= c (ch-question))
          (= c (ch-hat))
          (= c (ch-underscore))
          (= c (ch-backquote))
          (= c (ch-lbrace))
          (= c (ch-rbrace))
          (= c (ch-vbar))
          (= c (ch-tilde)))
      c
      #f))

(define (wsp? c)
  (if (or (= c (ch-space))
          (= c (ch-tab)))
      c
      #f))

(define (*wsp? doc start end)
  (and start
       (let ((s (if (pair? start) (car start) start)))
         (let loop ((i s))
           (if (and (< i end)
                    (wsp? (u8vector-ref doc i)))
               (loop (+ i 1))
               i)))))

(define (fws? doc start end)
  (and start
       (let ((s (if (pair? start) (car start) start)))
         (let ((i (*wsp? doc s end)))
           (if (and (<= (+ i 3) end)
                    (= (u8vector-ref doc i) (ch-return))
                    (= (u8vector-ref doc (+ i 1)) (ch-linefeed))
                    (wsp? (u8vector-ref doc (+ i 2))))
               (+ i 3)
               i)))))

(define (cfws? doc start end)
  ;; TODO: implement comments
  (fws? doc start end))

(define (parse-quoted-string doc start end)
  (and start
       (let ((s (if (pair? start) (car start) start)))
         (if (and (< s end)
                  (= (u8vector-ref doc s) (ch-doublequote)))
             (let loop ((i (+ s 1))
                        (rev-chars '()))
               (if (< i end)
                   (let ((next (u8vector-ref doc i)))
                     (cond ((= next (ch-doublequote))
                            (cons (+ i 1)
                                  (list->string (reverse rev-chars))))
                           ((= next (ch-backslash))
                            (let ((i (+ i 1)))
                              (if (< i end)
                                  (let ((next (u8vector-ref doc i)))
                                    (loop (+ i 1)
                                          (cons (integer->char next)
                                                rev-chars)))
                                  #f))) ;; end of input after backslash
                           (else
                            (loop (+ i 1)
                                  (cons (integer->char next) rev-chars)))))
                   #f)) ;; closing doublequote missing
             #f))))

(define (format-quoted-string str)
  (let loop1 ((i 0) (result ""))
    (let loop2 ((j i))
      (if (< j (string-length str))
          (let ((c (string-ref str j)))
            (if (or (char=? c #\") (char=? c #\\))
                (loop1 (+ j 1)
                       (string-append
                        result
                        (substring str i j)
                        (string #\\ c)))
                (loop2 (+ j 1))))
          (string-append
           "\""
           result
           (substring str i j)
           "\"")))))

(define (parse-symbol doc start end constituent?)
  (and start
       (let ((s (if (pair? start) (car start) start)))
         (if (< s end)
             (let ((first (u8vector-ref doc s)))
               (if (constituent? first)
                   (let loop ((i (+ s 1))
                              (rev-chars (list (integer->char first))))
                     (cond ((and (< i end)
                                 (constituent? (u8vector-ref doc i)))
                            =>
                            (lambda (next)
                              (loop (+ i 1)
                                    (cons (integer->char next) rev-chars))))
                           (else
                            (cons i
                                  (list->string (reverse rev-chars))))))
                   #f)) ;; illegal character
             #f)))) ;; end of input

(define (parse-token doc start end)
  (parse-symbol doc start end ttext?))

(define (parse-token-as-symbol doc start end)
  (let ((x (parse-symbol doc start end ttext?)))
    (and x
         (cons (car x)
               (string->symbol
                (list->string (map char-downcase (string->list (cdr x)))))))))

(define (parse-atom doc start end)
  (parse-symbol doc start end atext?))

(define (parse-dot-atom doc start end)
  (let ((first (parse-atom doc start end)))
    (and first
         (let loop ((i (car first))
                    (dot-atom (cdr first)))
           (if (and (< i end)
                    (= (u8vector-ref doc i) (ch-dot)))
               (let ((next (parse-atom doc (+ i 1) end)))
                 (if next
                     (loop (car next)
                           (string-append dot-atom "." (cdr next)))
                     #f)) ;; atom missing after dot
               (cons i dot-atom))))))

(define (parse-char doc start end code)
  (and start
       (let ((s (if (pair? start) (car start) start)))
         (and (< s end)
              (= (u8vector-ref doc s) code)
              (+ s 1)))))

(define (parse-eol? doc start end)
  (and start
       (let ((s (if (pair? start) (car start) start)))
         (and (<= (+ s 2) end)
              (= (u8vector-ref doc s) (ch-return))
              (= (u8vector-ref doc (+ s 1)) (ch-linefeed))
              (+ s 2)))))

(define (parse-attribute-value doc start end)
  (or (parse-token doc start end)
      (parse-quoted-string doc start end)))

(define (parse-parameters doc start end)
  (let loop ((i start)
             (rev-params '()))
    (let* ((i (cfws? doc i end))
           (semicolon (parse-char doc i end (ch-semicolon))))
      (if semicolon
          (let* ((i (cfws? doc semicolon end))
                 (attribute (parse-token-as-symbol doc i end))
                 (i (cfws? doc attribute end))
                 (i (parse-char doc i end (ch-equal)))
                 (i (cfws? doc i end))
                 (value (parse-attribute-value doc i end)))
            (and value
                 (loop (car value)
                       (cons (cons (cdr attribute)
                                   (cdr value))
                             rev-params))))
          (and i
               (cons i (reverse rev-params)))))))

(define (format-parameters params)
  (let loop ((lst params)
             (result ""))
    (if (pair? lst)
        (let* ((av
                (car lst))
               (attribute
                (car av))
               (value
                (cdr av)))
          (loop (cdr lst)
                (string-append
                 result
                 "; "
                 (symbol->string attribute)
                 "="
                 (format-quoted-string value)
                 (if (pair? (cdr lst))
                     " "
                     ""))))
        (string-append result (mime-eol-str)))))

(define (parse-header-field doc start end)
  (let* ((field-name (parse-token-as-symbol doc start end))
         (i (*wsp? doc field-name end)) ;; be lenient with whitespace
         (colon (parse-char doc i end (ch-colon))))
    (and colon
         (let ((x (assq (cdr field-name)
                        header-field-parsers-formatters)))
           (if x
               (let* ((field ((cadr x) doc colon end))
                      (i (parse-eol? doc field end)))
                 (and i
                      (cons i
                            (cons (cdr field-name) (cdr field)))))
               (let loop ((i colon))
                 (if (< i end)
                     (let ((eol (parse-eol? doc i end)))
                       (if (not eol)
                           (loop (+ i 1))
                           (cons eol
                                 (list #f
                                       (cdr field-name)
                                       (u8vector->ISO-8859-1-string
                                        (subu8vector doc colon i))))))
                     #f)))))))

(define (mime-parse-header doc start end)
  (let loop ((i start)
             (rev-fields '()))
    (let ((field (parse-header-field doc i end)))
      (if field
          (loop (car field)
                (cons (cdr field) rev-fields))
          (let ((i (parse-eol? doc i end)))
            (and i
                 (cons i (reverse rev-fields))))))))

(define (parse-field-content-type doc start end)
  (let* ((i (cfws? doc start end))
         (type (parse-token-as-symbol doc i end))
         (i (cfws? doc type end))
         (i (parse-char doc i end (ch-slash)))
         (i (cfws? doc i end))
         (subtype (parse-token-as-symbol doc i end))
         (i (cfws? doc subtype end))
         (params (parse-parameters doc i end)))
    (and params
         (cons (car params)
               (cons (cons (cdr type) (cdr subtype))
                     (cdr params))))))

(define (format-field-content-type field)
  (let* ((ts
          (cadr field))
         (type
          (car ts))
         (subtype
          (cdr ts))
         (params
          (cddr field)))
    (ISO-8859-1-string->u8vector
     (string-append
      "Content-Type: "
      (symbol->string type)
      "/"
      (symbol->string subtype)
      (format-parameters params)))))

(define (parse-field-content-length doc start end)
  (let* ((i (cfws? doc start end))
         (len (parse-token doc i end)))
    (and len
         (let ((n (string->number (cdr len) 10)))
           (and (integer? n)
                (exact? n)
                (not (negative? n))
                (list (car len) n))))))

(define (format-field-content-length field)
  (let ((len (cadr field)))
    (ISO-8859-1-string->u8vector
     (string-append
      "Content-Length: "
      (number->string len 10)
      (mime-eol-str)))))

(define (parse-field-content-disposition doc start end)
  (let* ((i (cfws? doc start end))
         (type (parse-token-as-symbol doc i end))
         (params (parse-parameters doc type end)))
    (and params
         (cons (car params)
               (cons (cdr type)
                     (cdr params))))))

(define (format-field-content-disposition field)
  (let* ((type
          (cadr field))
         (params
          (cddr field)))
    (ISO-8859-1-string->u8vector
     (string-append
      "Content-Disposition: "
      (symbol->string type)
      (format-parameters params)))))

(define (parse-field-content-transfer-encoding doc start end)
  (let* ((i (cfws? doc start end))
         (mechanism (parse-token-as-symbol doc i end)))
    mechanism))

(define (format-field-content-transfer-encoding field)
  (let ((mechanism
         (cadr field)))
    (ISO-8859-1-string->u8vector
     (string-append
      "Content-Transfer-Encoding: "
      (symbol->string mechanism)))))

(define header-field-parsers-formatters
  (list (list 'content-type
              parse-field-content-type
              format-field-content-type)
        (list 'content-length
              parse-field-content-length
              format-field-content-length)
        (list 'content-disposition
              parse-field-content-disposition
              format-field-content-disposition)
        (list 'content-transfer-encoding
              parse-field-content-transfer-encoding
              format-field-content-transfer-encoding)))

(define (mime-decode doc start end header-fields)
  (let* ((x
          (or (assq 'content-type header-fields)
              '(content-type (text . plain))))
         (ts
          (cadr x))
         (type
          (car ts))
         (subtype
          (cdr ts)))
    (case type

      ((text)
       ;; TODO: use the "charset" parameter to determine how to decode
       (mime-decode-text doc start end))

      ((application)
       (case subtype
         ((x-www-form-urlencoded)
          (mime-decode-x-www-form-urlencoded doc start end))
         (else ;; (octet-stream x-compressed x-gzip x-gzip-compressed)
          (mime-decode-application doc start end))))

      ((multipart)
       (let ((y (assq 'boundary (cddr x))))
         (if (not y)
             (error "multipart boundary missing")
             (let ((parts
                    (mime-decode-multipart-with-boundary
                     doc
                     start
                     end
                     (cdr y))))
               (and parts
                    (case subtype
                      ((form-data)
                       (map (lambda (x)
                              (let* ((fields (car x))
                                     (disp (assq 'content-disposition fields)))
                                (cond ((and disp
                                            (eq? (cadr disp) 'form-data)
                                            (assq 'name (cddr disp)))
                                       =>
                                       (lambda (name-binding)
                                         (cons (string->symbol
                                                (cdr name-binding))
                                               (cdr x))))
                                      (else
                                       (error "form-data syntax error")))))
                            parts))
                      (else
                       parts)))))))

      (else
       ;; TODO: handle more content types
       (error "unsupported content type")))))

(define (alist? obj)
  (and (pair? obj)
       (pair? (car obj))
       (symbol? (car (car obj)))))

(define (mime-encode doc)
  (cond ((string? doc)
         (mime-encode-text doc))
        ((u8vector? doc)
         (mime-encode-application doc))
        ((alist? doc)
         (mime-encode-multipart doc))
        (else
         ;; TODO: handle other types
         (error "type cannot be encoded"))))

(define (mime-decode-text doc start end)
  (subu8vector->ISO-8859-1-string doc start end))

(define (mime-encode-text doc)
  (cons '(content-type (text . plain))
        (ISO-8859-1-string->u8vector doc)))

(define (mime-decode-application doc start end)
  (subu8vector doc start end))

(define (mime-encode-application doc)
  (cons '(content-type (application . octet-stream))
        doc))

(define boundary-counter 31415926)

(define (generate-boundary)
  ;; TODO: replace with random number generator
  (set! boundary-counter (+ boundary-counter 1))
  (substring
   (number->string (+ 100000000 (modulo boundary-counter 100000000)) 10)
   1
   9))


(define (multipart-boundaries doc start end boundary-str)
  (let ((boundary
         (ISO-8859-1-string->u8vector
          (string-append "--" boundary-str))))

    (define (add-part eop prev-sop rev-parts)
      (if prev-sop
          (cons (cons prev-sop eop) rev-parts)
          rev-parts))

    (define (search-boundary i prev-sop rev-parts)
      (let ((sol
             (let loop ((i (+ i 2)))
               (if (<= i end)
                   (if (and (= (u8vector-ref doc (- i 2))
                               (ch-return))
                            (= (u8vector-ref doc (- i 1))
                               (ch-linefeed)))
                       i
                       (loop (+ i 1)))
                   #f))))
        (and sol
             (start-of-line sol prev-sop rev-parts))))

    (define (start-of-line sol prev-sop rev-parts)
      (if (<= (+ sol (+ 4 (u8vector-length boundary))) end)
          (let loop ((j 0))
            (if (< j (u8vector-length boundary))
                (if (= (u8vector-ref doc (+ sol j))
                       (u8vector-ref boundary j))
                    (loop (+ j 1))
                    (search-boundary (+ sol j) prev-sop rev-parts))
                (let ((sop (+ sol (+ j 2))))
                  (let ((c1 (u8vector-ref doc (- sop 2)))
                        (c2 (u8vector-ref doc (- sop 1))))
                    (cond ((and (= c1 (ch-return))
                                (= c2 (ch-linefeed)))
                           (start-of-line
                            sop
                            sop
                            (add-part (- sol 2) prev-sop rev-parts)))
                          ((and (= c1 (ch-minus))
                                (= c2 (ch-minus))
                                (= (u8vector-ref doc sop)
                                   (ch-return))
                                (= (u8vector-ref doc (+ sop 1))
                                   (ch-linefeed)))
                           (cons (+ sop 2)
                                 (reverse
                                  (add-part (- sol 2) prev-sop rev-parts))))
                          (else
                           #f))))))
          (search-boundary sol prev-sop rev-parts)))

    (start-of-line start #f '())))

(define (encode-multipart-with-boundary doc boundary-str)
  (let ((boundary
         (ISO-8859-1-string->u8vector boundary-str)))
    (let loop ((lst doc)
               (rev-parts '()))
      (if (pair? lst)
          (let* ((x (car lst))
                 (name (symbol->string (car x)))
                 (data (mime-encode (cdr x)))
                 (content-type (car data))
                 (content (cdr data))
                 (header
                  (mime-format-header
                   (list (list 'content-disposition
                               'form-data
                               (cons 'name name))
                         content-type))))
            (loop (cdr lst)
                  (cons eol
                        (cons content
                              (cons header
                                    (cons eol
                                          (cons boundary
                                                (cons dash-dash
                                                      rev-parts))))))))
          (apply-u8vector-append
           (reverse
            (cons eol
                  (cons dash-dash
                        (cons boundary
                              (cons dash-dash
                                    rev-parts))))))))))

(define (mime-encode-multipart doc)
  (let loop () ;; find a boundary that does not clash with data
    (let* ((boundary-str
            (generate-boundary))
           (x
            (encode-multipart-with-boundary doc boundary-str))
           (len
            (u8vector-length x))
           (b
            (multipart-boundaries x 0 len boundary-str)))
      (if (not (and (= len (car b)) ;; clash with data?
                    (= (length doc) (length (cdr b)))))
          (loop)
          (cons (list 'content-type
                      '(multipart . form-data)
                      (cons 'boundary boundary-str))
                x)))))

(define (mime-format-header fields)
  (let loop ((lst fields)
             (rev-parts '()))
    (if (pair? lst)
        (let* ((field (car lst))
               (field-name (car field)))
          (if field-name
              (let ((x (assq field-name
                             header-field-parsers-formatters)))
                (if x
                    (let ((part ((caddr x) field)))
                      (loop (cdr lst)
                            (cons part rev-parts)))
                    (error "unknown header field type")))
              (loop (cdr lst)
                    (cons (ISO-8859-1-string->u8vector
                           (string-append
                            (symbol->string (cadr field))
                            ":"
                            (caddr field)
                            (mime-eol-str)))
                          rev-parts))))
        (apply-u8vector-append
         (reverse
          (cons eol rev-parts))))))

(define (mime-decode-multipart-with-boundary doc start end boundary-str)
  (let ((boundaries
         (multipart-boundaries doc start end boundary-str)))
    (and boundaries
         (let loop ((lst (cdr boundaries))
                    (rev-parts '()))
           (if (pair? lst)
               (let* ((b (car lst))
                      (s (car b))
                      (e (cdr b))
                      (h (mime-parse-header doc s e)))
                 (and h
                      (let* ((i (car h))
                             (fields (cdr h))
                             (data (mime-decode doc i e fields)))
                        (and data
                             (loop (cdr lst)
                                   (cons (cons fields data) rev-parts))))))
               (reverse rev-parts))))))

(define (mime-encode-x-www-form-urlencoded fields)

  (define (add-substring str start end lst)
    (cons (ISO-8859-1-substring->u8vector str start end)
          lst))

  (define (add-urlencoded str lst)

    (define (nibble n)
      (string-ref "0123456789ABCDEF" (fxand n 15)))

    (let loop1 ((i 0) (lst lst))
      (let loop2 ((j i))
        (if (< j (string-length str))
            (let ((c (string-ref str j)))
              (if (or (and (char>=? c #\a) (char<=? c #\z))
                      (and (char>=? c #\A) (char<=? c #\Z))
                      (and (char>=? c #\0) (char<=? c #\9))
                      (char=? c #\-)
                      (char=? c #\_)
                      (char=? c #\.)
                      (char=? c #\~))
                  (loop2 (+ j 1))
                  (loop1 (+ j 1)
                         (add-substring
                          (let ((n (char->integer c)))
                            (string #\%
                                    (nibble (fxarithmetic-shift-right n 4))
                                    (nibble n)))
                          0
                          3
                          (add-substring str i j lst)))))
            (add-substring str i j lst)))))

  (define (add-field field lst)
    (add-urlencoded (cdr field)
                    (add-substring
                     "="
                     0
                     1
                     (add-urlencoded (symbol->string (car field))
                                     lst))))

  (apply-u8vector-append
   (if (null? fields)
       '()
       (let loop ((fields (cdr fields))
                  (lst (add-field (car fields) '())))
         (if (pair? fields)
             (loop (cdr fields)
                   (add-field (car fields) (add-substring "&" 0 1 lst)))
             (reverse lst))))))

(define (mime-decode-x-www-form-urlencoded doc start end)

  (define (extract start len)
    (let ((str (make-string len)))
      (let loop ((i start) (j 0))
        (if (< j len)
            (let ((c (u8vector-ref doc i)))
              (cond ((= c (ch-percent))
                     (cond ((hex (+ i 1))
                            =>
                            (lambda (x)
                              (string-set! str j (integer->char x))
                              (loop (+ i 3) (+ j 1))))
                           (else
                            #f)))
                    ((= c (ch-plus))
                     (string-set! str j #\space)
                     (loop (+ i 1) (+ j 1)))
                    (else
                     (string-set! str j (integer->char c))
                     (loop (+ i 1) (+ j 1)))))
            str))))

  (define (hex i)
    (if (< (+ i 1) end)
        (let ((h1 (nibble i))
              (h2 (nibble (+ i 1))))
          (and h1 h2 (+ (* h1 16) h2)))
        #f))

  (define (nibble i)
    (let ((c (u8vector-ref doc i)))
      (cond ((and (>= c (ch-digit-0)) (<= c (ch-digit-9)))
             (- c (ch-digit-0)))
            ((and (>= c (ch-lower-a)) (<= c (ch-lower-f)))
             (+ 10 (- c (ch-lower-a))))
            ((and (>= c (ch-upper-a)) (<= c (ch-upper-f)))
             (+ 10 (- c (ch-upper-a))))
            (else
             #f))))

  (define (state0 i rev-fields) ;; at beginning
    (if (< i end)
        (state1 i
                i
                0
                rev-fields)
        (reverse rev-fields)))

  (define (state1 i start len rev-fields) ;; in field name
    (if (< i end)
        (let ((c (u8vector-ref doc i)))
          (cond ((= c (ch-equal))
                 (state2 (+ i 1)
                         (+ i 1)
                         0
                         (extract start len)
                         rev-fields))
                ((= c (ch-percent))
                 (and (hex (+ i 1))
                      (state1 (+ i 3)
                              start
                              (+ len 1)
                              rev-fields)))
                (else
                 (state1 (+ i 1)
                         start
                         (+ len 1)
                         rev-fields))))
        #f))

  (define (state2 i start len name rev-fields) ;; in field value

    (define (end-of-field)
      (cons (cons (string->symbol name)
                  (extract start len))
            rev-fields))

    (if (< i end)
        (let ((c (u8vector-ref doc i)))
          (cond ((= c (ch-ampersand))
                 (state1 (+ i 1)
                         (+ i 1)
                         0
                         (end-of-field)))
                ((= c (ch-percent))
                 (and (hex (+ i 1))
                      (state2 (+ i 3)
                              start
                              (+ len 1)
                              name
                              rev-fields)))
                (else
                 (state2 (+ i 1)
                         start
                         (+ len 1)
                         name
                         rev-fields))))
        (reverse (end-of-field))))

  (state0 start '()))

(define (mime-string->content-type str)
  (let* ((doc (ISO-8859-1-string->u8vector str))
         (len (u8vector-length doc))
         (ct (parse-field-content-type doc 0 len)))
    (if (and ct
             (= (car ct) len))
        (cons 'content-type (cdr ct))
        #f)))

;;;============================================================================
