;;!!! Decode and encode data using the MIME format.
;; .author Marc Feeley, 2006-2007
;; .author Mikael More, 2013
;; .author Alvaro Castro-Castilla, 2015
;; .license lgpl/v2.1
;;
;; Copyright (C) 2006-2007 by Marc Feeley, All Rights Reserved.
;; Copyright (C) 2013 Mikael More
;;
;; Contains procedures to decode and encode data using the MIME format.
;;
;; This is a general purpose MIME library. It does not regard details about particular header types
;; used in emails or multipart/mixed HTML form encoding - such aspects can easily be addressed by
;; a separate library that abstracts upon this one.
;;
;; Marc released under LGPL 2.1 (and I would guess, as dual license with Apache).
;; Mikael's contributions released under MIT.
;;
;; The original version by Marc is at http://snow.iro.umontreal.ca/?viewpkg=mime .
;;
;; ## General description of the MIME format
;; The MIME format consists of two types of "entity":ies, namely Multiparts and Body parts.
;;
;;  * "Multipart"s contain Body parts or other Multiparts.
;;  * "Body part"s are the actual content carriers, each conveying a text, HTML, image etc.
;;
;; (Multipart is sometimes just called "message" in the MIME RFC:s.)
;;
;; Each entity has a "header" and a "body", quite the same in convention as that of HTTP.
;;
;; An entity being a multipart or a body part is defined by its Content-Type header.
;;
;; Multipart Content-Type headers are either of the following "Multipart subtypes":
;;
;;  * "multipart/mixed", meaning contained entities are of mixed types. This one is generally used
;;    for a Multipart containing an email body content entity (represented as a multipart/alternative
;;    Multipart or as a Body part) and then attachments.
;;
;;  * "multipart/alternate" means that each contained entity is a complementing version of the other
;;    contained ones and the agent is free to take either one in use.
;;
;;  * "multipart/digest" meaning each contained entity is like a separate message
;;
;;  * "multipart/related" meaning that the contained entities are needed together to be shown right
;;
;;  * "multipart/signed" to attach a digital signature to a message. Has two body parts: body and
;;    signature. The whole of the first is used for the production of the signature part.
;;    See http://tools.ietf.org/html/rfc1847#section-2.1 for more info.
;;
;;  * "multipart/encrypted", first part contains the info needed to decrypt the second part (PGP,
;;    PKCS etc.).
;;
;;  * "multipart/form-data" is the HTML or other form submission type to transmit a file upload etc.
;;
;;  * Other:
;;    "multipart/x-mixed-replace" when a new part is received it should replace the previous,
;;    for example used for MJPEG transmission in IP cameras
;;
;;    "multipart/report" is sent to a mail server, for some purpose
;;
;;    "multipart/byteranges" for describing noncontiguous byte ranges of same message
;;
;; The Multipart Content-Type header contains a boundary attribute e.g.
;; "Content-Type: multipart/mixed; boundary=frontier".
;;
;; Note that the toplevel message (i.e. the email or HTTP request or response at its top level) must
;; have, in addition to a Multipart Content-Type, a "MIME-Version: 1.0" header.
;;
;; A typical structure for a hybrid plaintext-HTML email is:
;;
;;      multipart/mixed
;;        multipart/alternative
;;          text/plain
;;          multipart/related
;;            text/html
;;            [html dependencies i.e. image/jpeg etc.]
;;      [file attachments e.g. document/pdf etc.]
;;
;; The plaintext part is put first as to show up at the top in email agents that do not support
;; MIME.
;;
;; In addition to Content-Type, MIME brings two more header values of interest:
;;
;;  * "Content-Disposition": Primarily describes if a file is part of the message to show right off,
;;    or is an attachment, as in that additional user action should be required to access it.
;;    Alternatively there's a third description type here that simply says that the entity is a
;;    part of a web page form submission.
;;
;;    The first of these three are called "inline", the second "attachment", and the third
;;    "form-data".
;;
;;    Please note that several email agents including Thunderbird not considers this type descriptor
;;    but instead looks at other things like content-type and filename, as to make up its mind about
;;    whether a file should be considered inline or attachment.
;;
;;    If it's "inline" or "attachment" or a file upload within a "form-data", then there may be a
;;    "filename" parameter describing the entity's filename.
;;
;;    If it's "form-data", then there is a "name" parameter containing the form's name.
;;
;;    Furthermore, this header element can describe the body part's modification date and creation
;;    date.
;;
;;    Example:
;;    Content-Disposition: attachment; filename=a.jpeg; modification-date="Wed, 12 Feb 1997 16:29:51 -0500"
;;
;;  * "Content-Transfer-Encoding": Describing the coding of the content. Typically "base64" or
;;    "7bit". (There's also "quoted-printable" which is an extension to 7bit, and "8bit" and
;;    "binary". Seems Thunderbird prefers to call it 8bit rather than binary.) (Can also describe
;;    whether a "binary-to-text encoding scheme" has been used apparently, no idea what that is.)
;;
;;    Example:
;;    Content-Transfer-Encoding: base64
;;
;; ## Scheme mapping of the MIME format
;; We define Multipart as an a-list where each element represents an entity contained in the
;; Multipart, where car = alist with the entity's headers and cdr = the entity's contents, which can
;; be either of
;;
;;  * String or u8vector = Body part
;;  * Alist              = Multipart
;;
;; Please note that the passing of Content-Type header tag must be done under the name
;; 'content-type , for the overriding of the internal default setting to be done properly.
;;
;; The headers have the general format key = header key as string or symbol and value = string,
;; with the exceptions of the following header keys:
;;
;;  * 'content-type              : Value is '(content-type (type . subtype) . params-alist)
;;                                 Example: '(content-type (text . html) (charset . "utf-8")))
;;                                 => "Content-Type: text/html; charset=\"utf-8\"\r\n"
;;
;;                                 For non-multipart content types, there is also the following
;;                                 definition: '(content-type content-type-s . params-alist)
;;                                 Example:
;;                                 '(content-type "text/plain" (charset . "utf-8")))
;;                                 => "Content-Type: text/plain; charset=\"utf-8\"\r\n"
;;                                 '(content-type "text/plain; charset=\"utf-8\"")
;;                                 => "Content-Type: text/plain; charset=\"utf-8\"\r\n"
;;
;;  * 'content-length            : Value is '(content-length length)
;;                                 Example: '(content-length 10))
;;                                 => as u8vector: "Content-Length: 10\r\n"
;;
;;  * 'content-disposition       :
;;
;;  * 'content-transfer-encoding : Can be base64.
;;
;;  * 'location                  :
;;
;; ## References
;; https://en.wikipedia.org/wiki/MIME
;;
;; For MIME RFC standard references, see:
;;
;; * RFC 2045 - Multipurpose Internet Mail Extensions (MIME) Part One:
;;   Format of Internet Message Bodies
;;   https://tools.ietf.org/html/rfc2045
;;
;; * RFC 2046 - Multipurpose Internet Mail Extensions (MIME) Part Two:
;;   Media Types
;;   https://tools.ietf.org/html/rfc2046
;;
;; * RFC 2047 - MIME (Multipurpose Internet Mail Extensions) Part Three:
;;   Message Header Extensions for Non-ASCII Text
;;   https://tools.ietf.org/html/rfc2047
;;
;; * RFC 2048 - Multipurpose Internet Mail Extensions (MIME) Part Four:
;;   Registration Procedures
;;   https://tools.ietf.org/html/rfc2048
;;
;; * RFC 2049 - Multipurpose Internet Mail Extensions (MIME) Part Five:
;;   Conformance Criteria and Examples
;;   https://tools.ietf.org/html/rfc2049
;;
;; * RFC 2822 - Internet Message Format
;;   https://tools.ietf.org/html/rfc2822
;;
;; * RFC 2183 - The Content-Disposition Header Field
;;   https://tools.ietf.org/html/rfc2183
;;
;; For other MIME implementations, see
;;
;;  * http://planet.racket-lang.org/display.ss?package=mime.plt&owner=bzlib
;;
;;  * https://github.com/billitch/cl-mime
;;
;;  * https://github.com/mikel/mail
;;
;;    http://mime.rubyforge.org/
;;    http://rubyforge.org/projects/mime/
;;
;;  * http://www.vmime.org/documentation.html
;;    http://www.codesink.org/data/mimetic/docs/html/index.html
;;
;;  * https://developer.gnome.org/gmime/stable/
;;
;;  * http://docs.python.org/2/library/email.generator.html
;;    http://docs.python.org/2/library/email.mime.html
;;
;;  * http://msdn.microsoft.com/en-us/library/system.net.mime.aspx
;;
;;  * http://pear.php.net/package/Mail_Mime/docs
;;
;; ## Exports
;; Below, start and end regard string indexes at which the respective processor will start and end
;; processing.
;;
;; (mime-encode-multipart #!optional (content-type-s "multipart/mixed"))
;; => list (body-content-u8v content-type-header-value-s boundary-s)
;; doc = Document to MIME-encode.
;; content-u8v = The mime-encoded content, u8vector.
;; content-type-header-value-s = The content-type header value e.g.
;;                               "Content-Type: multipart/mixed; boundary=123456789"
;; boundary-s = String to add to "Content-Type: multipart/mixed; boundary=" header included in the HTTP
;;              request or response or email body where the content is used.
;;
;; (mime-decode-multipart-with-boundary doc start end boundary-str #!optional all-as-u8vector)
;; => list of pair (headers-list . section-data) for each of the contained sections.
;; Decodes a MIME document such as the HTTP request body of a file upload, which is always sent as
;; "multipart/form-data" (with Content-Type "multipart/form-data; boundary=[boundary-str here]".
;;
;; (mime-encode-x-www-form-urlencoded fields)
;; To be documented. Doublecheck it's intended for external use.
;;
;; (mime-decode-x-www-form-urlencoded doc start end)
;; To be documented. Doublecheck it's intended for external use.
;;
;; ## Internal
;; (generate-boundary)
;; Generate a MIME boundary, string. Just any string that doesn't clash with content lines would
;; work, other than this arbitrary choice.
;;
;; (encode-multipart-with-boundary doc boundary-str) => content-u8v
;; Like |mime-encode-multipart| but does not return values (boundary-str content-u8v) but just
;; content-u8v , and, does not check for boundary-str clashes with the document contents.
;;
;; (multipart-boundaries content-u8v start end boundary-str)
;; Scan content-u8v for clases of boundary-str with encoded contents. ( * I don't currently see how
;; this procedure actually works, but it's clear from the code context that this is what it does. *)
;;
;; (mime-format-header fields)
;;
;; (mime-parse-header doc start end)
;;
;; (mime-decode-text doc start end)
;; Alias for |subu8vector->ISO-8859-1-string|.
;;
;; (mime-encode-text headers content)
;;
;; (mime-encode-application headers content)
;;
;; (mime-decode-application doc start end)
;; Alias for |subu8vector|.
;;
;; (mime-encode doc)
;; Internal hub for recursing the MIME encoding process, universal mime-encode of an entity. Passes
;; on work to another export: If doc is string then to |mime-encode-text|, if u8vector then to
;; |mime-encode-application|, if alist then to |mime-encode-multipart|.
;;
;; Please note that |mime-encode-application| sends each element back to |mime-encode| for encoding,
;; and that way recurses and provides MIME encoding in a tree structure.
;;
;; (mime-decode doc start end header-fields)
;;
;; (mime-string->content-type str)
;;
;; (mime-eol-str)
;;
;; ## TODO
;;  * The all-as-u8vector argument and involved code to mime-decode-application should not be
;;    necessary but to remove. Evaulate the form module's use of this and conclude.
;;    2013-07-29 update on this one: The multipart_form_data module uses it, and it works with
;;    all browsers tested. We need to review actual formal specification documents to see
;;    if it's really valid.
;;
;; ## History
;; 2013-03-13: Commented all the module, before there were more or less no comments.
;;
;;             Made the MIME encoder general-purpose by bringing support for custom headers to all
;;             entities and making the header format more normal. Before it was limited to
;;             the multipart/form-data HTML form post convention only.
;; 2013-03-14: Switched the text encoding applied from ISO-8859-1 to UTF-8 (by using
;;             |string->utf8-u8vector| instead of |ISO-8859-1-string->u8vector|). This saves us
;;             some odd presentation of Unicode chars. Wisest would be autodetection and choose
;;             based on that.
;;
;;             Implemented quoted-printable coding, |mime-encode-quoted-printable|.
;;
;; ## Example use
;;
;; Generating an email.
;;
;; We want a multipart/mixed output which is |mime-encode-multipart|'s default, so we do not specify
;; a second argument.
;;


;; ;#||
;; (import (std misc/u8v) mime)
;; (define s (mime-encode-multipart
;;            '(
;;              ; Main content entity, the text content. A multipart/alternative Multipart as to
;;              ; make it hybrid plaintext-HTML.
;;              (; Headers:
;;               ; (Note that the library automatically adds Content-Disposition: Inline .)
;;               ((content-type (multipart . alternative)))
;;               .
;;               ; Content:
;;               (; First element, the text version as a body part.
;;                (() ; No headers
;;                 .
;;                 "Hello world!")
;;
;;                ; Second element, the HTML version as a multipart/related Multipart.
;;                (; Headers:
;;                 ((content-type (multipart . related)))
;;                 .
;;                 (; The HTML text body part
;;                  (; Headers:
;;                   ((content-type (text . html)))
;;                   .
;;                   ; Content:
;;                   "<h1>Hello world!</h1>"))
;;                 )
;;                ))
;;
;;              ; Second entity, an attached text file
;;              (; Headers:
;;               ((content-disposition attachment (filename . "a.txt")))
;;               .
;;               ; Content:
;;               "Attached file contents."))))
;; (print (utf8-u8vector->string (car s)))
;; ||#

(declare (block)
         (standard-bindings) (extended-bindings)
         (fixnum))

(define mime-text-default-content-type
  '(content-type (text . plain)
                 ;; We set the charset to UTF-8 by default as it's UTF-8 we encode in.
                 (charset . "UTF-8")))

(define mime-text-default-content-type-wrapped-in-a-list
  `(,mime-text-default-content-type))

(define = eq?) ; Tweak that might save us a couple of clock cycles as this module is compiled in safe mode.

(define boundary-counter #f)    ; Current index of (generate-boundary)

;; As long as the underlying platform does not risk doing SIGSEGV because of concurrent reads and writes,
;; it's fine that this procedure contains no locking - the way it works is that to the caller within its
;; thread, the result will always be different from the one given on the previous call.
(define (mime-generate-boundary)
  (set! boundary-counter (+ (or boundary-counter (random-integer #x100000000))
                            1))
  (number->string (+ #x100000000 (modulo boundary-counter #x100000000)) 16))

(define-macro (ch-tab)         9)
(define-macro (ch-linefeed)    10)
(define-macro (ch-return)      13)
(define-macro (ch-space)       32)
(define-macro (ch-rubout)      127)

(define-macro (ch-doublequote) (char->integer #\"))
(define-macro (ch-backslash)   (char->integer #\\))
(define-macro (ch-dot)         (char->integer #\.))
(define-macro (ch-colon)       (char->integer #\:))

(define-macro (ch-exclam)      (char->integer #\!))
(define-macro (ch-sharp)       (char->integer #\#))
(define-macro (ch-dollar)      (char->integer #\$))
(define-macro (ch-percent)     (char->integer #\%))
(define-macro (ch-ampersand)   (char->integer #\&))
(define-macro (ch-quote)       (char->integer #\'))
(define-macro (ch-star)        (char->integer #\*))
(define-macro (ch-plus)        (char->integer #\+))
(define-macro (ch-minus)       (char->integer #\-))
(define-macro (ch-slash)       (char->integer #\/))
(define-macro (ch-equal)       (char->integer #\=))
(define-macro (ch-question)    (char->integer #\?))
(define-macro (ch-hat)         (char->integer #\^))
(define-macro (ch-underscore)  (char->integer #\_))
(define-macro (ch-backquote)   (char->integer #\`))
(define-macro (ch-lbrace)      (char->integer #\{))
(define-macro (ch-rbrace)      (char->integer #\}))
(define-macro (ch-vbar)        (char->integer #\|))
(define-macro (ch-tilde)       (char->integer #\~))

(define-macro (ch-lower-a)     (char->integer #\a))
(define-macro (ch-upper-a)     (char->integer #\A))
(define-macro (ch-lower-f)     (char->integer #\f))
(define-macro (ch-upper-f)     (char->integer #\F))
(define-macro (ch-lower-z)     (char->integer #\z))
(define-macro (ch-upper-z)     (char->integer #\Z))
(define-macro (ch-digit-0)     (char->integer #\0))
(define-macro (ch-digit-9)     (char->integer #\9))

(define-macro (ch-lbracket)    (char->integer #\[))
(define-macro (ch-rbracket)    (char->integer #\]))
(define-macro (ch-lparen)      (char->integer #\())
(define-macro (ch-rparen)      (char->integer #\)))
(define-macro (ch-langle)      (char->integer #\<))
(define-macro (ch-rangle)      (char->integer #\>))
(define-macro (ch-at)          (char->integer #\@))
(define-macro (ch-comma)       (char->integer #\,))
(define-macro (ch-semicolon)   (char->integer #\;))

;; = (string->u8vector "0123456789ABCDEF")
(define hex-chars-u8v '#u8(48 49 50 51 52 53 54 55 56 57 65 66 67 68 69 70))

(define eol-str
  (string (integer->char 13)
          (integer->char 10)))

(define (mime-eol-str)
  eol-str)

(define eol
  (string->u8vector (mime-eol-str)))

(define dash-dash
  (string->u8vector "--"))

;; Encodes an u8vector into the quoted-printable encoding. This is an encoding suitable for text
;; only. Bytes >127 occupy three characters, and line breaks are automatically enforced when a
;; line grows 76 bytes wide.
;;
;; The output is really a perfectly valid ASCII string, though since we ultimately produce a
;; u8vector by this and because strings cost more memory, we generate to u8vector.
(define (mime-encode-quoted-printable u8v)
  (let* ((u8v-l (u8vector-length u8v))
         ;; Output mechanism. A bit crude but makes sense for our purposes. Gambit's string
         ;; IO, if getting higher speed, would make sense to use also.
         (at-row-idx 0)
         ;; Each input byte may become up to 3 output bytes, plus every 76 bytes may add two chars,
         ;; so that means something like 3.1x. 4x gives us an even number.
         (r (make-u8vector (* u8v-l 4)))
         (r-at 0)
         (r! (lambda (b)
               (u8vector-set! r r-at b)
               (set! r-at       (fx+ r-at       1))
               (set! at-row-idx (fx+ at-row-idx 1))))
         (r3! (lambda (b1 b2 b3)
                (u8vector-set! r      r-at    b1)
                (u8vector-set! r (fx+ r-at 1) b2)
                (u8vector-set! r (fx+ r-at 2) b3)
                (set! r-at       (fx+ r-at       3))
                (set! at-row-idx (fx+ at-row-idx 3)))))
    (let loop ((u8v-at 0))
      (if (fx< u8v-at u8v-l)
          (let ((b (u8vector-ref u8v u8v-at)))
            ;; Reset row position if at newline
            (if (eq? b 13) (set! at-row-idx 0))
            ;; If row overflows, output trampoline to new one and start new one
            (if (fx>= at-row-idx 75)
                (begin
                  (r3! (char->integer #\=) 13 10)
                  (set! at-row-idx 0)))
            ;; Output current char, encode if needed.
            (if (or (fx>= b 127)
                    (eq? b (char->integer #\=)))
                (r3! (char->integer #\=)
                     (##u8vector-ref hex-chars-u8v (fxarithmetic-shift-right b 4))
                     (##u8vector-ref hex-chars-u8v (fxand b 15)))
                (r! b))
            ;; Continue
            (loop (fx+ u8v-at 1)))
          (subu8vector r 0 r-at)))))

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
                       (string-append result (substring str i j) (string #\\ c)))
                (loop2 (+ j 1))))

          (string-append "\"" result (substring str i j) "\"")))))

(define (format-quoted-string-if-needed str)
  (let ((str-l (string-length str)))
    (let loop ((at 0))
      (if (fx< at str-l)
          (let ((c (string-ref str at)))
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (eq? c #\-)
                    (eq? c #\_)) ;; There may be more that are OK.
                (loop (fx+ at 1))
                ;; Needs quoting.
                (format-quoted-string str)))
          ;; Does not need quoting.
          str))))

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
             #f))))     ;; end of input

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

;; (format-parameters '((charset . "UTF-8") (sth-else . "value")))
;; => "; charset=\"UTF-8\" ; sth-else=\"value\"\r\n"
(define (format-parameters params #!optional (include-eol? #t))
  (let loop ((lst params) (result ""))
    (if (pair? lst)
        (let* ((av        (car lst))
               (attribute (car av))
               (value     (cdr av)))
          (loop (cdr lst)
                (string-append
                 result "; "
                 (symbol->string attribute)
                 "="
                 ;; For the charset parameter, Thunderbird does not put quotes around "UTF-8",
                 ;; so we put quotes here only if needed.
                 ;; (format-quoted-string-if-needed value)
                 ;; Safer.
                 (format-quoted-string value)
                 (if (pair? (cdr lst))
                     " "
                     ""))))
        (if include-eol?
            (string-append result (mime-eol-str))
            result))))

(define (parse-header-field doc start end)
  (let* ((field-name (parse-token-as-symbol doc start end))
         (i (*wsp? doc field-name end)) ;; be lenient with whitespace
         (colon (parse-char doc i end (ch-colon))))
    (and colon
         (let ((colon (+ colon 1)) ; To get around the leading space in all strings.
               (x     (assq (cdr field-name) header-field-parsers-formatters)))
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
  (let loop ((i start) (rev-fields '()))
    (let ((field (parse-header-field doc i end)))
      (if field
          (loop (car field)
                (cons (cdr field) rev-fields))
          (let ((i (parse-eol? doc i end)))
            (and i
                 (cons i (reverse rev-fields))))))))

;; Content-Type header field handling
(define (parse-field-content-type doc start end)
  (let* ((i       (cfws? doc start end))
         (type    (parse-token-as-symbol doc i end))
         (i       (cfws? doc type end))
         (i       (parse-char doc i end (ch-slash)))
         (i       (cfws? doc i end))
         (subtype (parse-token-as-symbol doc i end))
         (i       (cfws? doc subtype end))
         (params  (parse-parameters doc i end)))
    (and params
         (cons (car params)
               (cons (cons (cdr type) (cdr subtype))
                     (cdr params))))))

;; field = '(content-type (type . subtype) . params-alist)
;;         or
;;         '(content-type content-type-s . params-alist)
;;
;; Please note that for it to work out with the rest of this module, the form with content-type-s
;; may not be used for multipart content types.
;;
;; (format-field-content-type '(content-type (text . html) (charset . "utf-8")))
;; => as u8vector: "Content-Type: text/html; charset=\"utf-8\"\r\n"
;;
;; (format-field-content-type '(content-type "text/plain" (charset . "utf-8")))
;; => as u8vector: "Content-Type: text/plain; charset=\"utf-8\"\r\n"
;;
;; (format-field-content-type '(content-type "text/plain; charset=\"utf-8\""))
;; => as u8vector: "Content-Type: text/plain; charset=\"utf-8\"\r\n"
;;
;; Procedure's code partially mirrored by |content-type-header-value-to-string|.
(define (format-field-content-type field)
  (let* ((ts      (cadr field))
         (params  (cddr field)))
    (ISO-8859-1-string->u8vector
     (apply string-append `("Content-Type: "
                            ,@(if (pair? ts)
                                  (let ((type    (car ts))
                                        (subtype (cdr ts)))
                                    `(,(symbol->string type) "/" ,(symbol->string subtype)))
                                  `(,ts))
                            ,(format-parameters params)))))) ; (Note, format-parameters ends its output with (mime-eol-str).)

;; Procedure code mirrors |format-field-content-type|.
(define (content-type-header-value-to-string field)
  (let* ((ts      (cadr field))
         (params  (cddr field)))
    (apply string-append `(,@(if (pair? ts)
                                 (let ((type    (car ts))
                                       (subtype (cdr ts)))
                                   `(,(symbol->string type) "/" ,(symbol->string subtype)))
                                 `(,ts))
                           ,(format-parameters params #f)))))

;; Content-Length header field handling
(define (parse-field-content-length doc start end)
  (let* ((i   (cfws? doc start end))
         (len (parse-token doc i end)))
    (and len
         (let ((n (string->number (cdr len) 10)))
           (and (integer? n)
                (exact? n)
                (not (negative? n))
                (list (car len) n))))))

;; field = '(content-length length)
;; (format-field-content-length '(content-length 10))
;; => as u8vector: "Content-Length: 10\r\n"
(define (format-field-content-length field)
  (let ((len (cadr field)))
    (ISO-8859-1-string->u8vector
     (string-append "Content-Length: " (number->string len 10) (mime-eol-str)))))

;; Content-Disposition header field handling
(define (parse-field-content-disposition doc start end)
  (let* ((i      (cfws? doc start end))
         (type   (parse-token-as-symbol doc i end))
         (params (parse-parameters doc type end)))
    (and params
         (cons (car params)
               (cons (cdr type)
                     (cdr params))))))

;; field = '(content-disposition type . params) where type is either of 'inline 'attachment 'form-data
;; (format-field-content-disposition '(content-disposition inline (name . "a.jpeg")))
;; => as u8vector: "Content-Disposition: inline; name=\"a.jpeg\"\r\n"
(define (format-field-content-disposition field)
  (let* ((type   (cadr field))
         (params (cddr field)))
    (ISO-8859-1-string->u8vector
     (string-append "Content-Disposition: "
                    (symbol->string type)
                    ;; (Note, format-parameters ends its output with (mime-eol-str).)
                    (format-parameters params)))))

;; Content-Transfer-Encoding header field handling
(define (parse-field-content-transfer-encoding doc start end)
  (let* ((i (cfws? doc start end))
         (mechanism (parse-token-as-symbol doc i end)))
    mechanism))

(define (format-field-content-transfer-encoding field)
  (let ((mechanism (cadr field)))
    (ISO-8859-1-string->u8vector
     (string-append "Content-Transfer-Encoding: " (symbol->string mechanism) (mime-eol-str)))))

(define (parse-field-location doc start end)
  (let* ((e (fws? doc start end))
         ;; (end (+ start e))
         (v (parse-symbol doc start end (lambda (c) (if (not (= c (ch-return))) c #f))))
         ;; (x (parse-parameters doc start end))
         ;;(c (u8vector->ISO-8859-1-string (subu8vector doc start end)))
         )
    ;; (print "e is " e " . x is "  ". v is ") (print ".\n")
    ;; (print "Location parsing: ") (display v) (print " (end)\n")
    (list (car v) (cdr v))))

(define (format-field-location field)
  (let ((url (cadr field)))
    (ISO-8859-1-string->u8vector
     (string-append "Location: " url (mime-eol-str)))))

(define header-field-parsers-formatters:parse-field-proc  cadr )
(define header-field-parsers-formatters:format-field-proc caddr)

(define header-field-parsers-formatters
  `(; (header-name-sy          parse-field-proc                       format-field-proc                      )
    (content-type              ,parse-field-content-type              ,format-field-content-type             )
    (content-length            ,parse-field-content-length            ,format-field-content-length           )
    (content-disposition       ,parse-field-content-disposition       ,format-field-content-disposition      )
    (content-transfer-encoding ,parse-field-content-transfer-encoding ,format-field-content-transfer-encoding)
    (location                  ,parse-field-location                  ,format-field-location                 )))

(define (mime-decode doc start end header-fields #!optional all-as-u8vector)
  (if all-as-u8vector
      ;; XXX Check that this one really has a point here. It has been used.
      ;;     The user of this feature is the multipart_form_data module; what we are seeing is that
      ;;     web browsers always pass the actual file content within the multipart/form-data
      ;;     as raw byte data, so that therefore ensuring this is the decoding method we use
      ;;     (i.e. none) provides a robustness guarantee on our side. I did not see any spec
      ;;     documents say anything about how this should work.
      (mime-decode-application doc start end)
      (let* ((x       (or (assq 'content-type header-fields) '(content-type (text . plain))))
             (ts      (cadr x))
             (type    (car ts))
             (subtype (cdr ts)))
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
           (error "unsupported content type"))))))

(define (mime-encode doc)
  (let ((headers (car doc))
        (content (cdr doc)))
  (cond ((string?   content) (mime-encode-text               headers content))
        ((u8vector? content) (mime-encode-application        headers content))
        ((list?     content) (mime-encode-multipart-internal headers content))
        (else                (error "type cannot be encoded"))))) ;; TODO: handle other types

(define (mime-decode-text doc start end)
  (subu8vector->ISO-8859-1-string doc start end))

;; headers = alist, doc = string
;;
;; We apply quoted-printable here by default. We do this because this is an encoding that's
;; supported everywhere and implies no risk of not being decodable by any agent, and,
;; it matches the string input format we take to this procedure - if anyone has a
;; binary blob, they would not feed it to this module as string but as u8vector.
(define (mime-encode-text headers doc)
  (cons `(,@(and (not (assq 'content-type headers)) mime-text-default-content-type-wrapped-in-a-list)
          ,@headers
          (content-transfer-encoding quoted-printable))
        (mime-encode-quoted-printable (string->utf8-u8vector doc))))

(define (mime-decode-application doc start end)
  (subu8vector doc start end))

;; headers = alist, doc = u8vector
;;
;; We apply base64 encoding here by default.
(define (mime-encode-application headers doc)
  (cons `(,@(and (not (assq 'content-type headers)) '((content-type (application . octet-stream))))
          ,@headers
          (content-transfer-encoding base64))
        (string->u8vector (u8vector->base64-string doc width: 75))))

;; Scan for occurrences of boundary-str in the form "--[boundary-str]\r\n" and
;; "--[boundary-str]--\r\n" in content-u8v, and presuming that the organization of these occurrences
;; in the way sought for are found, return the index intervals for the payloads whose locations they
;; demarcate.
;;
;; The structure is:
;; "--[boundary-str]\r\n[Payload 1 here]--[boundary-str]\r\n[Payload 2 here]--[boundary-str]--\r\n"
;;
;; => #f = The sought for boundaried structure was not found, or,
;;    pair (scanning-ended-at-content-u8v-index . results) where results is a list of
;;    pair (start-of-payload-index end-of-payload-index)
(define (multipart-boundaries content-u8v start end boundary-str)
  (let* ((doc content-u8v)
         (boundary (ISO-8859-1-string->u8vector (string-append "--" boundary-str)))
         (boundary-len (u8vector-length boundary)))
    (define (add-part eop               ; end-of-payload
                      prev-sop          ; previous-start-of-payload
                      rev-parts)        ; previous-parts
      ;; (print "(add-part " eop " " prev-sop " " rev-parts ")\n")
      (if prev-sop
          (cons (cons prev-sop eop) rev-parts)
          rev-parts))
    ;; Search operation traversing content-u8v up to the next newline.
    ;;
    ;; If no newline is found, makes the entire |multipart-boundaries| call return #f.
    ;;
    ;; If a newline is found, continues with |start-of-line| to search for the boundary at that line.
    (define (search-boundary i prev-sop rev-parts)
      ;; (print "(search-boundary " i " " prev-sop " " rev-parts ")\n")
      (let ((sol
             (let loop ((i (+ i 2)))
               (if (<= i end)
                   (if (and (= (u8vector-ref doc (- i 2)) (ch-return  ))
                            (= (u8vector-ref doc (- i 1)) (ch-linefeed)))
                       i
                       (loop (+ i 1)))
                   #f))))
        ;; (print "search-boundary resolved sol " sol "\n")
        (and sol
             (start-of-line sol prev-sop rev-parts))))
    ;; Search operation checking for the boundary in the form "--[boundary-str]" or "--[boundary-str]--"
    ;; starting at the beginning of the line in content-u8v that is currently being searched.
    (define (start-of-line sol ; start-of-line: Index in content-u8v where line we scan now starts
                           prev-sop     ; previous start-of-payload?
                           rev-parts)
      ;; (print "(start-of-line " sol " " prev-sop " " rev-parts ")\n")
      ;; *** Let's see, what sense does
      (if (<= (+ sol (+ 4 boundary-len)) end) ; *** This (+ a (+ b c)) is that for performance or what??
          (let loop ((j 0))
            ;; Check if the current line in content-u8v is equivalent with the boundary.
            (if (< j boundary-len)
                (if (= (u8vector-ref doc (+ sol j))
                       (u8vector-ref boundary j))
                    (loop (+ j 1)) ; (Byte matched, continue matching on next byte position)
                    ;; If the current line in content-u8v is not equivalent with boundary,
                    ;; then call |search-boundary| as to traverse content-u8v up to the beginning
                    ;; of its next line, and then continue with another search for the boundary at that line.
                    ;; If no newline is found, this procedure returns #f.
                    (search-boundary (+ sol j) prev-sop rev-parts))
                ;; It matched: This line is the same as the boundary.
                ;;
                ;; We relate to this as that the index we are at now, is the start of a new payload.
                (let ((sop (+ sol (+ j 2)))) ; start-of-payload ; *** This (+ a (+ b c)) is that for performance or what??
                  ;; Now, check what the last two characters that directly precede start of the new payload are.
                  (let ((c1 (u8vector-ref doc (- sop 2)))
                        (c2 (u8vector-ref doc (- sop 1))))
                    (cond
                     ;; If they're a newline (i.e. CR LF), then register this match in the result
                     ;; variable and start a new scan at this line we got to now.
                     ((and (= c1 (ch-return))
                           (= c2 (ch-linefeed)))
                      ;; Set start-of-line to the start-of-payload we found now, i.e.
                      ;; make another search, with start at the beginning of this payload found.
                      (start-of-line sop
                                     ;; Also register this place as prev-sop (previous-start-of-payload)
                                     sop
                                     (add-part (- sol 2) ; End index of payload we register now
                                               prev-sop ; Start index of payload we register now
                                               rev-parts)))
                     ;; If they're two minuses ("--") and then a newline (CR LF), then register this match
                     ;; in the result variable and then return this |multipart-boundaries| procedure call.
                     ;;
                     ;; Note that two minuses means we got to a line "--[boundary-str]--".
                     ((and (= c1 (ch-minus))
                           (= c2 (ch-minus))
                           (= (u8vector-ref doc sop      ) (ch-return  ))
                           (= (u8vector-ref doc (+ sop 1)) (ch-linefeed)))
                      (cons (+ sop 2) ; = scanning-ended-at-content-u8v-index
                            (reverse (add-part (- sol 2) ; End index of payload we register now
                                               prev-sop ; Start index of payload we register now
                                               rev-parts))))
                     ;; No occurrence of the boundary was found.
                     (else
                      #f))))))
          (search-boundary sol prev-sop rev-parts)))
    (start-of-line start #f '())))

;; Mime-encode document |doc| using |boundary-str| for boundary separator.
;; => u8vector
(define (encode-multipart-with-boundary doc boundary-str)
  (let ((boundary (ISO-8859-1-string->u8vector boundary-str)))
    (let loop ((lst doc) (rev-parts '()))
      (if (pair? lst)
          (let* ((x            (car lst)))
            ;; # For each element in doc do and accumulate the result in rev-parts :
            ;; (pp (list "encode-multipart-with-boundary processing entity " x))
            ;; Note that |mime-encode| can take a new MIME document as argument,
            ;; and thus returning a MIME document here as data, thus leading us to
            ;; create a tree structure here.
            (let* ((data (mime-encode x))
                   (headers (car data))
                   (content (cdr data)))
              ;; (pp (list "mime-encode on entity was done, and headers is: " headers
              ;;           " and content: " content))
              (let* ((headers-u8v  (mime-format-header ;`(
                                    ;; It appears that Content-Disposition headers are not so usual afterall.
                                    ;; ; This may be more or less meaningful: If there's no 'content-disposition
                                    ;; ; header specified by the caller, then make one that says it's an inline
                                    ;; ; element, as this ought to be the generally desired thing.
                                    ;; ;
                                    ;; ; However, don't add this if we're in a multipart.
                                    ;; ,@(and
                                    ;;    ; The check goes:
                                    ;;    ; If there's no content-disposition header already
                                    ;;    (not (assq 'content-disposition headers))
                                    ;;    ; And also we're clear that this is not a multipart entity
                                    ;;    ; (As we clarified in the convention in the header comments, the content-type header element
                                    ;;    ; may be '(content-type . content-type-header-value-s) when it's *not* a multipart.
                                    ;;    ; We need to check for this here though as not to presume the cell structure is a way
                                    ;;    ; it's not.)
                                    ;;    (let ((content-type-header (cadr (or (assq 'content-type headers)
                                    ;;                                         ; (Note, the (#f . #f) only becomes foor for the eq? with 'multipart .)
                                    ;;                                         '(content-type (#f . #f))))))
                                    ;;      (or (not (pair? content-type))
                                    ;;          (not (eq? 'multipart (car content-type-header)))))
                                    ;;    ; Then put a Content-Disposition header.
                                    ;;    '((content-disposition inline)))
                                    ;;
                                    ;; . ,
                                    headers)))
                ;; (display "Now processed Multipart element (car=headers, cdr=content): ") (pp x)
                (loop (cdr lst) `(,eol ; (Note that at the end the list is reversed so read this in reverse order)
                                  ,content     ;
                                  ,headers-u8v ; =
                                  ,eol         ; = "\r\n" as u8v
                                  ,boundary
                                  ,dash-dash        ; = "--" as u8v
                                  . ,rev-parts))))) ; (=The previous content)
          ;; # And when processed all doc elements,
          (begin
            ;; (print "encode-multipart-with-boundary invoked for doc ") (write doc) (print " boundary " boundary ", now rev-parts ")
            ;; (write rev-parts) (print ".\n")
            (apply-u8vector-append (reverse `(,eol ; (Note that the list is reversed after this so read this in reverse order)
                                              ,dash-dash
                                              ,boundary
                                              ,dash-dash
                                              . ,rev-parts)))))))) ; (=The previous content)

;; Internal logics for MIME multipart encoding.
;;
;; This procedure is essentially a wrapper for |encode-multipart-with-boundary|, adding to it a mechanism
;; to guarantee that the boundary does not clash with the contents.
(define (mime-encode-multipart-perform doc)
  ;; (display "mime-encode-multipart-perform invoked to encode doc: ") (pp doc)
  ;; Find a boundary that does not clash with data
  (let loop ()
    (let* ((boundary-str (mime-generate-boundary)) ; Generate a new boundary
           (x (encode-multipart-with-boundary doc boundary-str)) ; Encode the message
           (len (u8vector-length x))
           (b (multipart-boundaries x 0 len boundary-str))) ; Search for clash
      ;; (display "mime-encode-multipart-perform encoded to: ") (pp x)
      ;; (display "mime-encode-multipart-perform got boundaries result: ") (pp b)
      ;; Generated boundary clashes with document content?
      (if (not (and (= len (car b))
                    (= (length doc) (length (cdr b)))))
          ;; Yes it clashed, reiterate.
          (loop)
          ;; No clash, i.e. the encoded content is good.
          (list boundary-str x)))))

;; For internal use, called by |mime-encode| as part of recursive MIME encoding originating from
;; |mime-encode-multipart|.
(define (mime-encode-multipart-internal headers doc)
  ;; (pp (list "mime-encode-multipart-internal invoked with headers: " headers " doc: " doc))
  (apply (lambda (boundary-s body-content-u8v)
           (cons
            ;; Headers:
            ;; What we do here is to search through headers for the content-type , and add
            ;; the boundary parameter to it. If no content-type header is found, then one
            ;; for multipart/mixed is added.
            (let* ((found-content-type? #f)
                   (process-content-type-header (lambda (header)
                                                  `(,@header (boundary . ,boundary-s))))
                   (headers (map
                             (lambda (header)
                               (if (eq? (car header) 'content-type)
                                   (begin
                                     (set! found-content-type? #t)
                                     (process-content-type-header header))
                                   header))
                             headers)))
              (if found-content-type?
                  ;; A content-type header was found and a boundary parameter has now been added to it
                  ;; so the headers structure is updated with it.
                  headers
                  ;; A content-type was not found, create one now, add a boundary parameter and add the result to headers.
                  (cons (process-content-type-header '(content-type (multipart . mixed)))
                        headers)))
            ;; Content:
            body-content-u8v))
         (mime-encode-multipart-perform doc)))

;; MIME multipart encoding procedure for use by other modules.
;; => list (body-content-u8v content-type-header-value-s boundary-s)
(define (mime-encode-multipart doc #!optional (content-type-s "multipart/mixed"))
  (apply (lambda (boundary-s body-content-u8v)
           (list body-content-u8v (string-append content-type-s "; boundary=" boundary-s) boundary-s))
         (mime-encode-multipart-perform doc)))

(define (mime-format-header fields)
  (let loop ((lst fields) (rev-parts '()))
    (if (pair? lst)
        (let* ((field (car lst))
               ;; # For each input element |field| do;
               (field-name (car field)))
          (loop (cdr lst)
                (cons (let ((x (assq field-name header-field-parsers-formatters)))
                        (if x
                            ((header-field-parsers-formatters:format-field-proc x) field)
                            (ISO-8859-1-string->u8vector
                             (string-append (if (symbol? field-name) (symbol->string field-name) field-name)
                                            ":"
                                            (cdr field)
                                            (mime-eol-str)))))
                      rev-parts)))
        ;; # And when done return..
        (begin
          ;; (print "mime-format-header got ") (write fields) (print " and now has rev-parts") (write rev-parts) (print "\n")
          (apply-u8vector-append (reverse (cons eol rev-parts)))))))

;; (set! mime-format-header
;;       (let ((o mime-format-header))
;;         (lambda (fields)
;;           (with-exception-catcher
;;            (lambda (exc)
;;              ;; (dbg "mime#mime-format-header: Failed to format fields \""
;;              ;;      fields "\". Rethrowing exception. Exception: "
;;              ;;      (exception/continuation->string exc))
;;              ;; (##repl)
;;              (error "mime-format-header failure" exc))
;;            (lambda () (o fields))))))

(define (mime-decode-multipart-with-boundary doc start end boundary-str #!optional all-as-u8vector)
  (let ((boundaries (multipart-boundaries doc start end boundary-str)))
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
                             (data (mime-decode doc i e fields all-as-u8vector)))
                        (and data
                             (loop (cdr lst)
                                   (cons (cons fields data)
                                         rev-parts))))))
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

