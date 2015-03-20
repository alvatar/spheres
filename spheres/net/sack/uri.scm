;;!!! URI parsing
;; .author Marc Feeley, 2005-2007
;; .author PerEckerdal, 2008-2009
;; .author Mikael More, 2012-2013
;; .author Alvaro Castro-Castilla, 2015
;;
;; Written by Marc Feeley for http-server, made to a separate module and refactored by Per Eckerdal,
;; and improved on by Mikael More.
;;
;; Copyright (C) 2008-2009 Per Eckerdal, 2005-2007 Marc Feeley, 2012, 2013 Mikael More.
;; All Rights Reserved.
;;
;; ## This module's take on Unicode in URI <-> string serialization
;; This module is generally used in a context where it can be presumed that any URI processed has
;; its query and path parts UTF8-urlencoded.
;;
;; This is as any modern-enough web browser should provide query and path contents to us
;; UTF8-urlencoded, and we can presume that any non-web browser access follows this behavior too.
;;
;; This delivers partially as any user of this URI module, such as a HTTP server or client,
;; expectably wants to follow this convention - it's simply so general and logical that it makes the
;; most sense to go with.
;;
;; For this reason, the URI object's query and path parts are by default in standard Gambit string
;; plaintext unicode format, and URI serialization takes and gives URI strings that have
;; utf8-urlencoded query keys and values and paths.
;;
;; This behavior is implemented in the uri<->string routines which are:
;;  * string->uri
;;  * uri->string
;;  * uri-path&query->string
;;
;; Please note that generally URI modules do not have this default behavior, but simply pass around
;; query keys and values and paths in the exactly same representation as they were plaintext in the
;; URI string representation.
;;      This makes them more universal, at the cost of conveniency for the general reasonable kind
;; of use cases that this module generally expectably is used for.
;;
;; Please note though that this default behavior we have is not a universal solution - if working in
;; a context where this cannot be presumed, then we might as well run into a URL that cannot be
;; mapped to our internal format, like "http://host/%C3" which contains a broken UTF-8 sequence, or
;; that produces a strange result in our internal format, like "http://host/%D8." which is really
;; ISO 8859-1-encoded.
;;
;; For those use cases, the parameters |uri-query-string:use-char-encoding-in-urlencoding| and
;; |uri:use-char-encoding-in-urldecoding| are to be set to 'ISO-8859-1 .
;;      On the TODO is to add a mode where no urlencoding is done at all.
;;
;; # Special urlencoding of URI path
;; This default behavior requires a special URI encoding scheme to be applied to the serialization
;; of the path URI fragment.
;;
;; This is required for the two directions of URI <-> string serialization to be symmetric.
;;
;; A simple and quite fundamental example of when this symmetry is required, is in a HTTP client
;; where the HTTP client first ensures it has an URI object as destination uri, then possibly using
;; string->uri , and then when making the HTTP request it uses |uri-path&query->string| to get the
;; path line for outputting in the HTTP request line. This line is sent in an 8bit ANSI-encoded
;; format and must not have any >=127 bytes. Thus, sending Unicode characters here non-urlencoded
;; would be information destructive (or anyhow an error).
;;
;; At the same time, the urlencoding used for the URI path cannot be the ordinary one, as the
;; ordinary urlencode encodes fundamental URI features like "/", leading to all kinds of undefined
;; behavior with other servers.
;;
;; Therefore, essentially we want path urlencoding to encode only the characters that absolutely
;; require encoding.
;;
;; Therefore, we encode all characters except the ASCII printable characters, and there with the
;; exception of the characters that very plausibly generally may have a control character function,
;; namely #\space #\% #\? .
;;
;; Note that this way, (uri->string (string->uri x)) may very well return a URI object not equal to
;; x , however, our serialization logics do have a normalizing effect.
;;
;; ## References
;; RFC 3986: http://www.rfc-editor.org/rfc/rfc3986.txt
;;
;; Unicode handling of the host and path fragments are dealt with in
;; http://www.w3.org/International/articles/idn-and-iri/ .
;;
;; For a general discourse see http://www.skorks.com/2010/05/what-every-developer-should-know-about-urls/ .
;;
;;  * Racket
;;    The uri* files in https://github.com/plt/racket/tree/master/collects/net .
;;    Note that Racket's url<->string procedures do not do any urlen-/decoding.
;;    This is done in url.rkt . Docs at http://docs.racket-lang.org/net/url.html .
;;
;;  * Java
;;    http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/6-b14/java/net/URI.java
;;    (or http://javasourcecode.org/html/open-source/jdk/jdk-6u23/java/net/URI.java.html )
;;    on new URI(String) and URI.toString() time no urlen-/decoding of path is done.
;;
;; ## History
;; 2012-01-18: Made parse-uri-query more liberal through making it use the newly introduced
;;             excluded-char?/liberal-for-http-get-values routine.
;;                  Until now parsing an URI such as "/doc?abc=de[fg" would produce a query
;;             '(("abc" . "def")) i.e. cut off at the [ . The same would apply for all the other
;;             control chars matched by excluded-char? , including [ ] < > ^ .
;;                  excluded-char?/liberal-for-http-get-values matches only chars higher than
;;             #\x7f i.e. clear indication of broken char encoding. The current handling of this
;;             is just to stop parsing though, better would be to give #f uri.
;; 2013-03-22: Made string->uri automatically lowercase the URI scheme, as for us to have as
;;             internal convention that URI schemes are lowercase strings. Uses SRFI 13's ASCII-only
;;             |string-downcase|, which makes good sense as all standardized URI schemes use latin
;;             characters only.
;;
;; ## TODO
;;  * Provide a way for URI<->string serialization not to do any urlencoding at all.
;;



(declare
  (standard-bindings)
  (extended-bindings)
  (block))

(define-type uri
  id: 62788556-c247-11d9-9598-00039301ba52
  constructor: make-uri/internal
  scheme
  userinfo
  host
  port
  path
  query
  fragment)

(define (make-uri scheme userinfo host port path query fragment)
  (if (not
       (and (or (not scheme)
                (string? scheme))
            (or (not userinfo)
                (string? userinfo))
            (or (not host)
                (string? host))
            (or (not port)
                (integer? port))
            (or (not path)
                (string? path))
            (or (not query)
                (string? query) ;; I'm not sure that query should be
                ;; allowed to be a string.
                (null? query)
                (pair? query))
            (or (not fragment)
                (string? fragment))))
      (error "Invalid argument"))
  (make-uri/internal scheme
                     userinfo
                     host
                     port
                     path
                     query
                     fragment))

(define (clone-uri uri)
  (make-uri (uri-scheme uri)
            (uri-userinfo uri)
            (uri-host uri)
            (uri-port uri)
            (uri-path uri)
            (uri-query uri)
            (uri-fragment uri)))

(define (uri-scheme-set uri scheme)
  (make-uri scheme
            (uri-userinfo uri)
            (uri-host uri)
            (uri-port uri)
            (uri-path uri)
            (uri-query uri)
            (uri-fragment uri)))

(define (uri-userinfo-set uri userinfo)
  (make-uri (uri-scheme uri)
            userinfo
            (uri-host uri)
            (uri-port uri)
            (uri-path uri)
            (uri-query uri)
            (uri-fragment uri)))

(define (uri-host-set uri host)
  (make-uri (uri-scheme uri)
            (uri-userinfo uri)
            host
            (uri-port uri)
            (uri-path uri)
            (uri-query uri)
            (uri-fragment uri)))

(define (uri-port-set uri port)
  (make-uri (uri-scheme uri)
            (uri-userinfo uri)
            (uri-host uri)
            port
            (uri-path uri)
            (uri-query uri)
            (uri-fragment uri)))

(define (uri-path-set uri path)
  (make-uri (uri-scheme uri)
            (uri-userinfo uri)
            (uri-host uri)
            (uri-port uri)
            path
            (uri-query uri)
            (uri-fragment uri)))

(define (uri-query-set uri query)
  (make-uri (uri-scheme uri)
            (uri-userinfo uri)
            (uri-host uri)
            (uri-port uri)
            (uri-path uri)
            query
            (uri-fragment uri)))

(define (uri-fragment-set uri fragment)
  (make-uri (uri-scheme uri)
            (uri-userinfo uri)
            (uri-host uri)
            (uri-port uri)
            (uri-path uri)
            (uri-query uri)
            fragment))

;; This is an internal utility method
(define (uri-authority-set! uri authority)
  (uri-userinfo-set! uri (and authority (car authority)))
  (uri-host-set! uri (and authority (cadr authority)))
  (uri-port-set! uri (and authority (caddr authority))))

(define (uri-authority uri)
  (and (uri-host uri)
       (list (uri-userinfo uri)
             (uri-host uri)
             (uri-port uri))))

(define (uri-authority-set uri authority)
  (make-uri (uri-scheme uri)
            (and authority (car authority))
            (and authority (cadr authority))
            (and authority (caddr authority))
            (uri-path uri)
            (uri-query uri)
            (uri-fragment uri)))

;; Some nonstandard web servers require urlencoding to be done in another char encoding than
;; UTF-8, which is the commonly accepted standard all across the Internet.
(define uri-query-string:use-char-encoding-in-urlencoding (make-parameter 'UTF-8))
(define uri-query-string:use-char-encoding-in-urldecoding (make-parameter 'UTF-8))

(define (uri-query-string uri)
  (let ((urlencode (case (uri-query-string:use-char-encoding-in-urlencoding)
                     ((UTF-8     ) urlencode)
                     ((ISO-8859-1) urlencode-ISO8859)
                     (else (error "Unknown charset for uri query encoding" (uri-query-string:use-char-encoding-in-urlencoding)))))
        (q (uri-query uri)))
    (if (string? q)
        q
        (with-output-to-string
          ""
          (lambda ()
            (cond
             ((string? q)
              ;; (display "?")
              (display q))
             ((pair? q)
              (let ((first #t))
                (for-each
                 (lambda (pair)
                   (if (not first)
                       (display "&"))
                   (set! first #f)
                   (if (cdr pair)
                       (print (list
                               (urlencode (car pair))
                               "="
                               (urlencode (cdr pair))))
                       (display (urlencode (car pair)))))
                 q)))))))))

(define (uri-path&query->string uri)
  (let ((path (uri-path uri))
        (query-string (uri-query-string uri)))
    (string-append (if (zero? (string-length path)) "/" (urlencode-uripath path))
                   (if (zero? (string-length query-string)) "" "?") query-string)))

(define (hex-digit str i)
  (let ((n (char->integer (string-ref str i))))
    (cond ((and (>= n 48) (<= n 57))
           (- n 48))
          ((and (>= n 65) (<= n 70))
           (- n 55))
          ((and (>= n 97) (<= n 102))
           (- n 87))
          (else
           #f))))

(define (hex-octet str i)
  (let ((n1 (hex-digit str i)))
    (and n1
         (let ((n2 (hex-digit str (+ i 1))))
           (and n2
                (+ (* n1 16) n2))))))

(define (plausible-hex-escape? str end j strict)
  (and (< (+ j 2) end)
       (not (control-or-space-char? (string-ref str (+ j 1)) strict))
       (not (control-or-space-char? (string-ref str (+ j 2)) strict))))

(define (control-or-space-char? c strict)
  (or (not ((if strict
                char<?
                char<=?)
            #\space c))
      (not (char<? c #\x7f))))

(define (excluded-char? c)
  (or (not (char<? #\space c))
      (not (char<? c #\x7f))
      (char=? c #\<)
      (char=? c #\>)
      (char=? c #\#)
      (char=? c #\%)
      (char=? c #\")
      (char=? c #\{)
      (char=? c #\})
      (char=? c #\|)
      (char=? c #\\)
      (char=? c #\^)
      (char=? c #\[)
      (char=? c #\])
      (char=? c #\`)))

(define (excluded-char?/liberal-for-http-get-values c)
  (or (not (char<? c #\x7f))))

(define (extract-escaped/ISO-8859-1 str start n)
  (let ((result (make-string n)))
    (let loop ((i start) (j 0))
      (if (< j n)
          (let ((c (string-ref str i)))
            (if (char=? c #\%)
                (let ((n (hex-octet str (+ i 1))))
                  (and n
                       (begin
                         (string-set! result j (integer->char n))
                         (loop (+ i 3)
                               (+ j 1)))))
                (begin
                  (string-set! result j (if (char=? c #\+) #\space c))
                  (loop (+ i 1)
                        (+ j 1)))))
          result))))

;; Instead of just doing this work by a |urledecode| call, we produce an u8vector with the bytes
;; to decode here, and then run |utf8-u8vector->string| on it.
(define (extract-escaped/UTF-8 str start n)
  (let ((result (make-u8vector n)))
    (let loop ((i start) (j 0))
      (if (< j n)
          (let ((c (string-ref str i)))
            (if (char=? c #\%)
                (let ((n (hex-octet str (+ i 1))))
                  (and n
                       (begin
                         (u8vector-set! result j n)
                         (loop (+ i 3)
                               (+ j 1)))))
                (begin
                  (u8vector-set! result j
                                 (char->integer (if (char=? c #\+) #\space c)))
                  (loop (+ i 1)
                        (+ j 1)))))
          ;; result now contains the u8vector with the extracted bytes, so let's UTF8-decode them.
          (utf8-u8vector->string result)))))

;; Extract a substring out of a URI / URI component string, depending on current decoding settings.
;;
;; str = the string containing the character sequence that we want to urldecode ("unescape").
;; start = the character position in str that we start processing at.
;; n = how many bytes (as described in non-escaped or escaped form) in str it is that we want to process.
;;     for ISO-8859-1 encoding indeed this is valuable info as this is how many result bytes we should return here,
;;     but, for UTF-8 it carries little sense as it might be fewer than that as well.
(define (extract-escaped str start n)
  (case (uri-query-string:use-char-encoding-in-urldecoding)
    ('UTF-8      (extract-escaped/UTF-8      str start n))
    ('ISO-8859-1 (extract-escaped/ISO-8859-1 str start n))
    (else (error "Unknown charset for uri-decoding" (uri-query-string:use-char-encoding-in-urldecoding)))))

(define (parse-uri str start end decode? cont #!optional (strict #t))
  (let ((uri (make-uri #f #f #f #f "" '() #f)))
    (define (extract-string i j n)
      (if decode?
          (extract-escaped str i n)
          (substring str i j)))
    (define (extract-query i j n)
      (if decode?
          (parse-uri-query str
                           i
                           j
                           decode?
                           (lambda (bindings end)
                             bindings))
          (substring str i j)))
    ;; Extracts the "scheme" part and is a hub for the rest of processing.
    (define (state0 i j n)
      ;; (dbg "State0 executed with i \"" i "\" j \"" j "\" n \"" n "\".")
      (if (< j end)
          (let ((c (string-ref str j)))
            ;; (dbg "state0 processes char " c)
            (cond ((char=? c #\:)
                   (if (= n 0)
                       (state2 j (+ j 1) 1) ; the ":" is in the "path" part
                       (let ((scheme (extract-string i j n)))
                         (and scheme
                              ;; As to help internal convention, we keep URI schemes to always be downcase.
                              ;; This is SRFI 13's down-case that supports ASCII chars only. This is fine as
                              ;; all standardized URI schemes use those characters only.
                              (let ((scheme (string-downcase scheme)))
                                (uri-scheme-set! uri scheme)
                                        ; (dbg "state0 concluded uri-scheme to " scheme)
                                (if (and (< (+ j 2) end)
                                         (char=? (string-ref str (+ j 1))
                                                 #\/)
                                         (char=? (string-ref str (+ j 2))
                                                 #\/))
                                    (state1 (+ j 3) (+ j 3) 0 #f #f)
                                    (state2 (+ j 1) (+ j 1) 0)))))))
                  ((char=? c #\/)
                   (if (and (= n 0)
                            (< (+ j 1) end)
                            (char=? (string-ref str (+ j 1)) #\/))
                       (state1 (+ j 2) (+ j 2) 0 #f #f)
                       (state2 i (+ j 1) (+ n 1))))
                  ((char=? c #\?)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            (uri-path-set! uri path)
                            (state3 (+ j 1) (+ j 1) 0)))))
                  ((char=? c #\#)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            (uri-path-set! uri path)
                            (state4 (+ j 1) (+ j 1) 0)))))
                  ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state0 i (+ j 3) (+ n 1))))
                  ((control-or-space-char? c strict)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            (uri-path-set! uri path)
                            j))))
                  (else
                   (state0 i (+ j 1) (+ n 1)))))
          (let ((path (extract-string i j n)))
                                        ; (dbg "state0 concluded uri-path to " path)
            (and path
                 (begin
                   (uri-path-set! uri path)
                   j)))))
    ;; inside the "authority" part.  last-colon-marker is a cons of (j
    ;; . n) where the last colon was found, userinfo-idx is a cons of
    ;; (j . n) where the last @ was found. This is used in the
    ;; conclude function, which creates the authority part, a list of
    ;; (USERINFO HOST PORT).
    (define (state1 i j n last-colon-marker userinfo-idx)
      (define (conclude fun new-i new-j new-n)
        (let* ( ;; If userinfo-idx (the index of the @ mark) is
               ;; greater than the last-colon-marker, then the
               ;; last-colon-marker doesn't refer to the port
               ;; and should be discarded.
               (last-colon-marker
                (if (and userinfo-idx
                         (> (car userinfo-idx)
                            (car last-colon-marker)))
                    #f
                    last-colon-marker))
               (userinfo
                (and userinfo-idx
                     (extract-string i
                                     (car userinfo-idx)
                                     (cdr userinfo-idx))))
               (after-userinfo (if userinfo-idx
                                   (+ (car userinfo-idx) 1)
                                   i))
               (host-absolute-end
                (if last-colon-marker
                    (car last-colon-marker)
                    j))
               (host (extract-string after-userinfo
                                     host-absolute-end
                                     (- host-absolute-end after-userinfo)))
               (port (and last-colon-marker
                          (let ((port-absolute-begin
                                 (+ 1 (car last-colon-marker))))
                            (string->number
                             (extract-string port-absolute-begin
                                             j
                                             (- j port-absolute-begin))))))
               (authority (list userinfo host port)))
                                        ; (dbg "state1 concluded authority to " authority)
                                        ; (and authority (begin - No need, authority is always set here. The original code contained this though.
          (uri-authority-set! uri authority)
          (fun new-i new-j new-n)))

                                        ; (dbg "State1 executed with i \"" i "\" j \"" j "\" n \"" n "\".")
      (if (< j end)
          (let ((c (string-ref str j)))
            (cond ((char=? c #\/)
                   (conclude state2 j (+ j 1) 1))
                  ((char=? c #\?)
                   (conclude state3 (+ j 1) (+ j 1) 0))
                  ((char=? c #\#)
                   (conclude state4 (+ j 1) (+ j 1) 0))
                  ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state1 i (+ j 3) (+ n 1) last-colon-marker userinfo-idx)))
                  ((control-or-space-char? c strict)
                   (conclude (lambda (i j n) j) i j n)) ; The lambda makes this procedure |state1| return j.
                  ((char=? c #\@)
                   ;; All up until this was userinfo
                   (state1 i (+ j 1) (+ n 1) last-colon-marker (cons j n)))
                  ((char=? c #\:)
                   ;; Set last colon marker
                   (state1 i (+ j 1) (+ n 1) (cons j n) userinfo-idx))
                  (else
                   (state1 i (+ j 1) (+ n 1) last-colon-marker userinfo-idx))))
          (conclude (lambda (new-i new-j new-n) j)
                    0 0 0)))
    ;; inside the "path" part
    (define (state2 i j n)
      ;; (dbg "State2 executed with i \"" i "\" j \"" j "\" n \"" n "\".")
      (if (< j end)
          (let ((c (string-ref str j)))
            ;; (dbg "state2 processes char " c)
            (cond ((char=? c #\?)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            ;; (dbg "state2 concluded path to " path)
                            (uri-path-set! uri path)
                            (state3 (+ j 1) (+ j 1) 0)))))
                  ((char=? c #\#)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            ;; (dbg "state2 concluded path to " path)
                            (uri-path-set! uri path)
                            (state4 (+ j 1) (+ j 1) 0)))))
                  ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state2 i (+ j 3) (+ n 1))))
                  ((control-or-space-char? c strict)
                   (let ((path (extract-string i j n)))
                     (and path
                          (begin
                            ;; (dbg "state2 concluded path to " path)
                            (uri-path-set! uri path)
                            j))))
                  (else
                   (state2 i (+ j 1) (+ n 1)))))
          (let ((path (extract-string i j n)))
                                        ; (dbg "state2 concluded path to " path)
            (and path
                 (begin
                   (uri-path-set! uri path)
                   j)))))
    ;; inside the "query" part
    (define (state3 i j n)
      ;; (dbg "State3 executed with i \"" i "\" j \"" j "\" n \"" n "\".")
      (if (< j end)
          (let ((c (string-ref str j)))
            ;; (dbg "state3 processes char " c)
            (cond ((char=? c #\#)
                   (let ((query (extract-query i j n)))
                     (and query
                          (begin
                            (uri-query-set! uri query)
                            (state4 (+ j 1) (+ j 1) 0)))))
                  ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state3 i (+ j 3) (+ n 1))))
                  ((control-or-space-char? c strict)
                   (let ((query (extract-query i j n)))
                     (and query
                          (begin
                            (uri-query-set! uri query)
                            j))))
                  (else
                   (state3 i (+ j 1) (+ n 1)))))
          (let ((query (extract-query i j n)))
                                        ; (dbg "state3 concluded query to " query)
            (and query
                 (begin
                   (uri-query-set! uri query)
                   j)))))
    ;; inside the "fragment" part
    (define (state4 i j n)
      ;; (dbg "State4 executed with i \"" i "\" j \"" j "\" n \"" n "\".")
      (if (< j end)
          (let ((c (string-ref str j)))
            ;; (dbg "state4 processes char " c)
            (cond ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state4 i (+ j 3) (+ n 1))))
                  ((control-or-space-char? c strict)
                   (let ((fragment (extract-string i j n)))
                     (and fragment
                          (begin
                            (uri-fragment-set! uri fragment)
                            j))))
                  (else
                   (state4 i (+ j 1) (+ n 1)))))
          (let ((fragment (extract-string i j n)))
            (and fragment
                 (begin
                   ;; (dbg "state4 concluded fragment to " fragment)
                   (uri-fragment-set! uri fragment)
                   j)))))
    (let ((i (state0 start start 0)))
      ;;(dbg "state0 returned " i)
      (cont (and i uri)
            (or i start)))))

(define (parse-uri-query str start end decode? cont #!optional (strict #t))
  (let ((rev-bindings '()))
    (define (extract-string i j n)
      (let ((returnvalue
             (if decode?
                 (extract-escaped str i n)
                 (substring str i j))))
        ;; (dbg "extract-string returns \"" returnvalue "\".")
        returnvalue))
    (define (state0 i j n)
      ;; (dbg "state0 run with i " i " j " j " n " n)
      (if (< j end)
          (let ((c (string-ref str j)))
            (cond ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state0 i
                                (+ j 3)
                                (+ n 1))))
                  ((char=? c #\=)
                   (let ((name (extract-string i j n)))
                     (and name
                          (let ((j (+ j 1)))
                            (state1 j
                                    j
                                    0
                                    name)))))
                  ((char=? c #\&)
                   (set! rev-bindings
                         (cons (cons (extract-string i j n) #f) rev-bindings))
                   (let ((nj (+ j 1)))
                     (state0 nj nj 0)))
                  ((excluded-char? c)
                   (if (= n 0)
                       j
                       #f))
                  (else
                   (state0 i
                           (+ j 1)
                           (+ n 1)))))
          (begin
            (set! rev-bindings
                  (cons (cons (extract-string i j n) #f) rev-bindings))
            j)))
    (define (state1 i j n name)
      ;; (dbg "state1 run with i " i " j " j " n " n " name " name)
      (if (< j end)
          (let ((c (string-ref str j)))
            (cond ((char=? c #\%)
                   (and (plausible-hex-escape? str end j strict)
                        (state1 i
                                (+ j 3)
                                (+ n 1)
                                name)))
                  ((char=? c #\&)
                   (let ((val (extract-string i j n)))
                     (and val
                          (let ((j (+ j 1)))
                            (set! rev-bindings
                                  (cons (cons name val) rev-bindings))
                            (and (< j end)
                                 (state0 j
                                         j
                                         0))))))
                  ((and strict (char=? c #\=) #f))
                  ((excluded-char?/liberal-for-http-get-values c)
                   (let ((val (extract-string i j n)))
                     (and val
                          (begin
                            (set! rev-bindings
                                  (cons (cons name val) rev-bindings))
                            j))))
                  (else
                   (state1 i
                           (+ j 1)
                           (+ n 1)
                           name))))
          ;; This is where we extract a query value i.e. /path?key=[VALUE]&key2=[VALUE 2] .
          (let ((val (extract-string i j n)))
            (and val
                 (begin
                   (set! rev-bindings
                         (cons (cons name val) rev-bindings))
                   j)))))
    ;; (dbg "parse-query run with str " str " start " start " end " end " decode? " decode? " cont " cont " strict " strict)
    (let* ((i (state0 start start 0))
           (returnvalue (cont (and i (reverse rev-bindings))
                              (or i start))))
      ;; (dbg "parse-uri-query returns " returnvalue)
      returnvalue)))

(define (string->uri str #!optional (decode? #t) (strict #t))
  (parse-uri str
             0
             (string-length str)
             decode?
             (lambda (uri end)
               (and (= end (string-length str))
                    uri))
             strict))

(define (uri->string uri)
  (with-output-to-string
    ""
    (lambda ()
      (let ((scheme (uri-scheme uri))
            (authority (uri-authority uri))
            (path (uri-path uri))
            (query (uri-query uri))
            (fragment (uri-fragment uri)))
        (if scheme
            (begin
              (display scheme)
              (display ":")))
        (if authority
            (let ((ui (uri-userinfo uri))
                  (p (uri-port uri)))
              (display "//")
              (if ui
                  (begin
                    (display ui)
                    (display "@")))
              (display (uri-host uri))
              (if (and p
                       (not (or (and (equal? scheme "http" ) (eq? p 80 ))
                                (and (equal? scheme "https") (eq? p 443)))))
                  (begin
                    (display ":")
                    (display p)))))
        (if path
            (display (urlencode-uripath path)))

        (if (or (string? query)
                (pair? query))
            (begin
              (display "?")
              (display (uri-query-string uri))))

        (if fragment
            (begin
              (display "#")
              (display fragment)))))))

(define (string->uri-query str #!optional (decode? #t))
  (parse-uri-query str
                   0
                   (string-length str)
                   decode?
                   (lambda (query end)
                     (and (= end (string-length str))
                          query))))

(define (encode-for-uri str)
  (let ((end (string-length str)))
    (define (copy result i j n)
      (if (< i j)
          (let ((new-j (- j 1))
                (new-n (- n 1)))
            (string-set! result new-n (string-ref str new-j))
            (copy result i new-j new-n))
          result))
    (define (hex x)
      (string-ref "0123456789ABCDEF" (bitwise-and x 15)))
    (define (encode i j n)
      (if (< j end)
          (let ((c (string-ref str j)))
            (cond ((char=? c #\space)
                   (let ((result (encode (+ j 1) (+ j 1) (+ n 1))))
                     (string-set! result n #\+)
                     (copy result i j n)))
                  ((or (char=? c #\+)
                       (excluded-char? c))
                   (let ((result (encode (+ j 1) (+ j 1) (+ n 3))))
                     (let* ((x (char->integer c))
                            (hi (hex (arithmetic-shift x -4)))
                            (lo (hex x)))
                       (string-set! result n #\%)
                       (string-set! result (+ n 1) hi)
                       (string-set! result (+ n 2) lo))
                     (copy result i j n)))
                  (else
                   (encode i (+ j 1) (+ n 1)))))
          (let ((result (make-string n)))
            (copy result i j n))))
    (encode 0 0 0)))

;; Removes extraneous "./" and "../" in a URI path. See section 5.2.4
;; of RFC 3986.
(define (remove-dot-segments str)
  (let* ((in-len (string-length str))
         (res (make-string in-len)))
    ;; i is where we are in the source string,
    ;; j is where we are in the result string,
    ;; segs is a list, used as a stack, of the indices of the
    ;; previously encountered path segments in the result string.
    (letrec
        ((new-segment
          (lambda (i j segs)
            (let* ((segment-start (car segs))
                   (segment-length (- j segment-start 1)))
              (cond
               ;; Check for .
               ((and (= 1 segment-length)
                     (char=? #\. (string-ref res segment-start)))
                (loop (+ 1 i) segment-start segs))
               ;; Check for ..
               ((and (= 2 segment-length)
                     (char=? #\. (string-ref res segment-start))
                     (char=? #\. (string-ref res (+ 1 segment-start))))
                (cond
                 ;; Take care of the "/../something" special case; it
                 ;; should return "/something" and not "something".
                 ((and (= 1 segment-start)
                       (char=? #\/ (string-ref res 0)))
                  (loop (+ 1 i) 1 '(1)))
                 ;; This is needed because the code in the else clause
                 ;; assumes that segs is a list of length >= 2
                 ((zero? segment-start)
                  (loop (+ 1 i) 0 segs))
                 (else
                  (loop (+ 1 i) (cadr segs) (cdr segs)))))
               ;; Check for the end of the string
               ((>= (+ 1 i) in-len)
                j)
               (else
                (loop (+ 1 i) j (cons j segs)))))))
         (loop
          (lambda (i j segs)
            (if (>= i in-len)
                (new-segment i j segs)
                (let ((chr (string-ref str i)))
                  (string-set! res j chr)
                  (if (char=? chr #\/)
                      (new-segment i (+ 1 j) segs)
                      (loop (+ 1 i) (+ 1 j) segs)))))))
      (let ((idx (loop 0 0 '(0))))
        (substring res 0 idx)))))

;; Makes an absolute uri from ref. See section 5.2 of RFC 3986.
(define (uri-join base ref)
  (cond
   ((uri-scheme ref)
    (make-uri (uri-scheme ref)
              (uri-userinfo ref)
              (uri-host ref)
              (uri-port ref)
              (remove-dot-segments (uri-path ref))
              (uri-query ref)
              (uri-fragment ref)))
   ((uri-authority ref)
    (make-uri (uri-scheme base)
              (uri-userinfo ref)
              (uri-host ref)
              (uri-port ref)
              (remove-dot-segments (uri-path ref))
              (uri-query ref)
              (uri-fragment ref)))
   ((let ((path (uri-path ref)))
      (and path (positive? (string-length path))))
    (make-uri (uri-scheme base)
              (uri-userinfo ref)
              (uri-host ref)
              (uri-port ref)
              (let ((path (uri-path ref))
                    (base-path (uri-path base)))
                (cond
                 ;; If path begins with /
                 ((char=? #\/ (string-ref path 0))
                  (remove-dot-segments path))
                 ;; The following two cond clauses are the
                 ;; implementation of section 5.2.3 in RFC 3986.
                 ((and (uri-authority base)
                       (or (not base-path)
                           (= 0 (string-length base-path))))
                  (string-append "/" path))
                 (else
                  (string-append
                   (substring base-path
                              0
                              (+ 1
                                 (or (string-index-right
                                      base-path
                                      #\/)
                                     -1)))
                   path))))
              (uri-query ref)
              (uri-fragment ref)))
   (else
    (make-uri (uri-scheme base)
              (uri-userinfo ref)
              (uri-host ref)
              (uri-port ref)
              (uri-path base)
              (or (uri-query ref)
                  (uri-query base))
              (uri-fragment ref)))))

(define (uri-join-strings base ref)
  (uri-join (string->uri base)
            (string->uri ref)))

;; See the "Special urlencoding of URI path" comments section for more info.
(define (urlencode-uripath str)
  (utf8-u8vector->string (with-output-to-u8vector
                          '()
                          (lambda () ( ;; Urlencoder for any ASCII printable character except #\space #\% #\? .
                                 (write-urlencoded-u8vector-lambda
                                  (and (fx<= 33 b 126) ; Anywhere between #\! (as to exclude #\space which is 32) and 126, the last printable character
                                       (not (eq? b 37)) ; = (char->integer #\%)
                                       (not (eq? b 63)))) ; = (char->integer #\?)
                                 (string->utf8-u8vector str))))))
