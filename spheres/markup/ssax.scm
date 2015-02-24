;;!!! XML to SXML deserializer library
;; Optimized for speed
;; Essentially Oleg's SSAX' code, rearranged.
;; Mikael Möre, 2009-2011

;; ## Exports
;; (xml-string->sxml xml-string
;;                   #!optional (namespace-prefix-assig '()) preserve-whitespace-only-node-values on-need-more-data (operation 'parse)
;;                   (namespaces-already-defined '()))
;; operation = 'parse = Do XML to SXML conversion of all xml-string now (i.e. ssax:xml->sxml).
;;             'lazy  = Initiate XML to SXML conversion lazily (i.e. lazy:xml->sxml).
;;             'ssax  = Initiate processing of XML to an SSAX event stream. xml-string->sxml returns a procedure
;;                      that takes no arguments. Each invocation to this procedure returns the next event. Returns #f
;;                      when out of events.
;;
;; xml-string = string with the XML in it. May be empty.
;;
;; on-need-more-data = #f = Make xml-string->sxml get to an EOF while data is needed condition, and produce
;;                          an exception.
;;                     procedure = procedure that takes no arguments, that is invoked when xml-string->sxml ran
;;                                 out of data. Returns either of:
;;                                      #f or empty string = same behavior as if on-need-more-data is #f
;;                                      string with content = xml-string->sxml will use this as the continuation of
;;                                                            xml-string passed earlier/the previous string with content
;;                                                            returned by this procedure.
;;                                      values (string-w-content start end) = same as above but access is limited to start - end
;;                                      When on-need-more-data is run, xml-string and strings returned from any previous
;;                                 on-need-more-data call will not be accessed by xml-string->sxml anymore. Thus it's
;;                                 ok for on-need-more-data to read to and return one and the same buffer string, on each
;;                                 of its invocations.
;;
;; namespaces-already-defined = List. This is xml-string->sxml 's internal namespace structure. For if XML parsing
;;                              is started in the middle of an XML document/stream, this parameter offers the caller
;;                              to pass the namespaces value from the parent elements, as gotten from an 'ssax
;;                              session.
;;                                   For any other namespace matters, use the namespace-prefix-assig argument.
;;
;; Obsolete: eof-handler = #f = EOF produces error
;; Obsolete:               procedure = procedure that takes no arguments, that is invoked on EOF error.
;; Obsolete:                                On return, an error will be thrown, so it needs to skip out of this procedure
;; Obsolete:                           using a call/cc made by itself.
;; Obsolete:                                An alternative behavior that would require some rewriting of this module's code,
;; Obsolete:                           would be for on a non-#f return by eof-handler, xml-string->sxml would return the
;; Obsolete:                           sxml read up to the error happened.
;;
;; xml-string->sxml-start-at-idx
;; Parameter value that tells xml-string->sxml what string index within xml-string to start reading at.
;; By default set to 0.
;;
;; xml-string->sxml-end-at-idx
;; Parameter value that tells xml-string->sxml what string index within xml-string to end reading at.
;; #f = up to the end of the string, default value.
;;
;; xml-string->sxml-read-to-idx
;; Parameter value set at end of xml-string->sxml invocation with operation = 'parse , telling
;; what index of xml-string that parsing ended, or #f if all of xml-string was parsed.
;;      If you use it, obviously as caller remember to parameterize it first.
;;
;; xml-string->sxml-read-to-idx/all-data
;; Same as xml-string->sxml-read-to-idx, but instead of regarding the offset in the currently
;; traversed string, it is the offset within all strings traversed up to now (i.e. xml-string
;; + any strings returned by on-need-more-data).
;;
;; html-entity-unicode-numbers
;; A-list of html entities - (entity-name-symbol . unicode-number)
;;
;; html-entity-unicode-chars
;; Same as html-entity-unicode-numbers but contains character representation
;; of unicode-number instead.
;;
;; ## Design
;; The steps that were taken to optimize the deserializer were:
;;  - Base input on a string that does not change length during the
;;    serialization process, instead of on an input port.
;;  - Replace all used input functions (that is, read-char and
;;    peek-char) with macros that access the string using string-ref.
;;  - Put all code in one object file, thus omitting the possibility
;;    of inter-object file call costs (that are 500-3000 clock cycles
;;    somewhere, don't remember), and give Gambit ability to
;;    automatically inline procedures within the code.
;;  - Put the entire deserializer inside a closure, and remove all
;;    passing of the input port as an argument
;;
;; We now deserialize XML at 0.043 seconds per megabyte, rather than
;; 2.8s per megabyte, in a VirtualBox on my laptop.  That's a
;; seventy-fold speed increase.
;;
;; This means approx 25MB per second rather than 0.3, so we have
;; expat speed now.
;;
;; ** Effectively there's a problem in the parser, which is that it
;; produces symbols out of strings too much.  Symbols are never
;; garbage collected, so this could hypothethically produce fatal
;; errors such as excessive memory consumption.
;;
;; ## on-need-more-data use example
;; (xml-string->sxml "<" '() #f (let ((c 0)) (lambda ()
;;                                             (set! c (+ c 1))
;;                                             (case c ((1) "a"    ) ((2) " "   ) ((3) "b"     )
;;                                                     ((4) "="    ) ((5) "\""  ) ((6) "cde\">")
;;                                                     ((7) "<f />") ((8) "</a>") ((9) #f      )))))
;;
;; ## History
;; 2008-09-30 - Replaced tiny ssax:predefined-parsed-entities with
;;              more robust html-entity-unicode-chars.
;; 2009-12-11 - CDATA reading was broken, fixed.
;; 2011-11-26 - Fixed error at row 1640 (was (read-attrib-value '*eof*entities fragments) )
;;              Introduced eof-handler argument, and xml-string->sxml-read-to-idx parameter value.
;; 2011-11-29 - Introduced 'lazy and 'ssax operation modes.
;;              Introduced on-need-more-data and discarded use of eof-handler .
;;              Introduced the xml-string->sxml-read-to-idx/all-data .

(cond-expand
 (gambit (declare (fixnum)))
 (else (void)))

;; Keep commented out in sharp code.
;; (define (dbg . a)
;;   (print "dbg: ") (for-each (lambda (v) ((if (string? v) print write) v)) a) (print "\n"))

;; ## Custom addition:
;; based on name2codepoint of python's htmlentitydefs library. to get it, type python, import htmlentitydefs, htmlentitydefs.name2codepoint
(define html-entity-unicode-numbers
  '((zwnj . 8204)
    (aring . 229)
    (gt . 62)
    (yen . 165)
    (ograve . 242)
    (Chi . 935)
    (delta . 948)
    (rang . 9002)
    (sup . 8835)
    (trade . 8482)
    (Ntilde . 209)
    (xi . 958)
    (upsih . 978)
    (Yacute . 221)
    (Atilde . 195)
    (radic . 8730)
    (otimes . 8855)
    (aelig . 230)
    (oelig . 339)
    (equiv . 8801)
    (ni . 8715)
    (Psi . 936)
    (auml . 228)
    (Uuml . 220)
    (Epsilon . 917)
    (Yuml . 376)
    (lt . 60)
    (Icirc . 206)
    (shy . 173)
    (Upsilon . 933)
    (Lambda . 923)
    (yacute . 253)
    (Prime . 8243)
    (prime . 8242)
    (psi . 968)
    (Kappa . 922)
    (rsaquo . 8250)
    (Tau . 932)
    (darr . 8595)
    (ocirc . 244)
    (lrm . 8206)
    (zwj . 8205)
    (cedil . 184)
    (rlm . 8207)
    (Alpha . 913)
    (not . 172)
    (amp . 38)
    (AElig . 198)
    (oslash . 248)
    (acute . 180)
    (lceil . 8968)
    (iquest . 191)
    (uacute . 250)
    (laquo . 171)
    (dArr . 8659)
    (rdquo . 8221)
    (ge . 8805)
    (Igrave . 204)
    (nu . 957)
    (ccedil . 231)
    (lsaquo . 8249)
    (sube . 8838)
    (rarr . 8594)
    (sdot . 8901)
    (supe . 8839)
    (nbsp . 160)
    (lfloor . 8970)
    (lArr . 8656)
    (Auml . 196)
    (asymp . 8776)
    (Otilde . 213)
    (szlig . 223)
    (clubs . 9827)
    (agrave . 224)
    (Ocirc . 212)
    (ndash . 8211)
    (Theta . 920)
    (Pi . 928)
    (OElig . 338)
    (Scaron . 352)
    (frac14 . 188)
    (egrave . 232)
    (sub . 8834)
    (iexcl . 161)
    (frac12 . 189)
    (ordf . 170)
    (sum . 8721)
    (prop . 8733)
    (circ . 710)
    (ntilde . 241)
    (atilde . 227)
    (theta . 952)
    (prod . 8719)
    (nsub . 8836)
    (hArr . 8660)
    (rArr . 8658)
    (Oslash . 216)
    (emsp . 8195)
    (THORN . 222)
    (infin . 8734)
    (yuml . 255)
    (Mu . 924)
    (le . 8804)
    (Eacute . 201)
    (thinsp . 8201)
    (ecirc . 234)
    (bdquo . 8222)
    (Sigma . 931)
    (fnof . 402)
    (kappa . 954)
    (Aring . 197)
    (tilde . 732)
    (cup . 8746)
    (mdash . 8212)
    (uarr . 8593)
    (permil . 8240)
    (tau . 964)
    (Ugrave . 217)
    (eta . 951)
    (Agrave . 192)
    (sup1 . 185)
    (forall . 8704)
    (eth . 240)
    (rceil . 8969)
    (iuml . 239)
    (gamma . 947)
    (lambda . 955)
    (harr . 8596)
    (reg . 174)
    (Egrave . 200)
    (sup3 . 179)
    (dagger . 8224)
    (divide . 247)
    (Ouml . 214)
    (image . 8465)
    (alefsym . 8501)
    (igrave . 236)
    (otilde . 245)
    (pound . 163)
    (eacute . 233)
    (frasl . 8260)
    (ETH . 208)
    (lowast . 8727)
    (Nu . 925)
    (plusmn . 177)
    (chi . 967)
    (sup2 . 178)
    (frac34 . 190)
    (Aacute . 193)
    (cent . 162)
    (oline . 8254)
    (Beta . 914)
    (perp . 8869)
    (Delta . 916)
    (loz . 9674)
    (pi . 960)
    (iota . 953)
    (empty . 8709)
    (euml . 235)
    (brvbar . 166)
    (iacute . 237)
    (para . 182)
    (ordm . 186)
    (ensp . 8194)
    (uuml . 252)
    (there4 . 8756)
    (part . 8706)
    (icirc . 238)
    (bull . 8226)
    (omicron . 959)
    (upsilon . 965)
    (copy . 169)
    (Iuml . 207)
    (Oacute . 211)
    (Xi . 926)
    (Dagger . 8225)
    (Ograve . 210)
    (Ucirc . 219)
    (cap . 8745)
    (mu . 956)
    (sigmaf . 962)
    (scaron . 353)
    (lsquo . 8216)
    (isin . 8712)
    (Zeta . 918)
    (minus . 8722)
    (deg . 176)
    (and . 8743)
    (real . 8476)
    (ang . 8736)
    (hellip . 8230)
    (curren . 164)
    (int . 8747)
    (ucirc . 251)
    (rfloor . 8971)
    (crarr . 8629)
    (ugrave . 249)
    (notin . 8713)
    (exist . 8707)
    (cong . 8773)
    (oplus . 8853)
    (times . 215)
    (Acirc . 194)
    (piv . 982)
    (Euml . 203)
    (Phi . 934)
    (Iacute . 205)
    (quot . 34)
    (Uacute . 218)
    (Omicron . 927)
    (ne . 8800)
    (Iota . 921)
    (nabla . 8711)
    (sbquo . 8218)
    (Rho . 929)
    (epsilon . 949)
    (Ecirc . 202)
    (zeta . 950)
    (Omega . 937)
    (acirc . 226)
    (sim . 8764)
    (phi . 966)
    (diams . 9830)
    (macr . 175)
    (larr . 8592)
    (Ccedil . 199)
    (aacute . 225)
    (uArr . 8657)
    (beta . 946)
    (Eta . 919)
    (weierp . 8472)
    (rho . 961)
    (micro . 181)
    (alpha . 945)
    (omega . 969)
    (middot . 183)
    (Gamma . 915)
    (euro . 8364)
    (lang . 9001)
    (spades . 9824)
    (rsquo . 8217)
    (uml . 168)
    (thorn . 254)
    (ouml . 246)
    (thetasym . 977)
    (or . 8744)
    (raquo . 187)
    (sect . 167)
    (ldquo . 8220)
    (hearts . 9829)
    (sigma . 963)
    (oacute . 243)
    (apos . 39)))

(define html-entity-unicode-chars
  (map (lambda (e)
         (cons (symbol->string (car e))
               (make-string 1 (integer->char (cdr e)))))
       html-entity-unicode-numbers))

(define xml-string->sxml-start-at-idx         (make-parameter 0 ))
(define xml-string->sxml-end-at-idx           (make-parameter #f))

(define xml-string->sxml-read-to-idx          (make-parameter #f))
(define xml-string->sxml-read-to-idx/all-data (make-parameter #f))

;;!  The main routine
(define* (xml-string->sxml xml-string
                           (namespace-prefix-assig '())
                           (preserve-whitespace-only-node-values #f)
                           (on-need-more-data #f)
                           (operation 'parse)
                           (namespaces-already-defined '()))
  ;; SRFI 13 cut
  (define (string-index s criterion #!optional (start 0) end)
    (let ((end (or end (string-length s))))
      (cond ((char? criterion)
             (let lp ((i start))
               (and (< i end)
                    (if (char=? criterion (string-ref s i)) i
                        (lp (+ i 1))))))
            ((procedure? criterion)
             (let lp ((i start))
               (and (< i end)
                    (if (criterion (string-ref s i)) i
                        (lp (+ i 1))))))
            (else (error "Second param is neither char, or predicate procedure."
                         string-index criterion)))))
  (define-macro (every? proc lst)
    `(let ((proc ,proc))
       (let loop ((lst ,lst))
         (or (null? lst)
             (and (proc (car lst))
                  (loop (cdr lst)))))))
  ;; Library-internal routine
  (define (string-copy! to tstart from fstart fend)
    (if (> fstart tstart)
        (do ((i fstart (+ i 1))
             (j tstart (+ j 1)))
            ((>= i fend))
          (string-set! to j (string-ref from i)))
        (do ((i (- fend 1)                    (- i 1))
             (j (+ -1 tstart (- fend fstart)) (- j 1)))
            ((< i fstart))
          (string-set! to j (string-ref from i)))))
  (define (string-concatenate/shared strings)
    (if (list? strings)
        (if (every? string? strings)
            (let lp ((strings strings) (nchars 0) (first #f))
              (cond ((pair? strings)    ; Scan the args, add up total
                     (let* ((string  (car strings)) ; length, remember 1st
                            (tail (cdr strings)) ; non-empty string.
                            (slen (string-length string)))
                       (if (zero? slen)
                           (lp tail nchars first)
                           (lp tail (+ nchars slen) (or first strings)))))
                    ((zero? nchars) "")
                    ;; Just one non-empty string! Return it.
                    ((= nchars (string-length (car first))) (car first))
                    (else (let ((ans (make-string nchars)))
                            (let lp ((strings first) (i 0))
                              (if (pair? strings)
                                  (let* ((s (car strings))
                                         (slen (string-length s)))
                                    (string-copy! ans i s 0 slen)
                                    (lp (cdr strings) (+ i slen)))))
                            ans))))
            (error "Not every element of strings is a string: (string-concatenate/shared " strings ")"))
        (error "strings is not a list: (string-concatenate/shared " strings ")")))
  (define (string-xcopy! target tstart s sfrom sto)
    (do ((i sfrom (++ i)) (j tstart (++ j)))
        ((>= i sto))
      (string-set! target j (string-ref s i))))
  ;; procedure string-concatenate-reverse STRINGS FINAL END
  (define (string-concatenate-reverse strs final end)
    (if (null? strs) (substring final 0 end)
        (let*
            ((total-len
              (let loop ((len end) (lst strs))
                (if (null? lst) len
                    (loop (+ len (string-length (car lst))) (cdr lst)))))
             (result (make-string total-len)))
          (let loop ((len end) (j total-len) (str final) (lst strs))
            (string-xcopy! result (- j len) str 0 len)
            (if (null? lst) result
                (loop (string-length (car lst)) (- j len)
                      (car lst) (cdr lst)))))))
  ;; string-concatenate-reverse/shared STRING-LIST [FINAL-STRING END] -> STRING
  ;; We do not use the optional arguments of this procedure. Therefore,
  ;; we do not implement them. See SRFI-13 for the complete
  ;; implementation.
  (define (string-concatenate-reverse/shared strs)
    (cond
     ((null? strs) "")			; Test for the fast path first
     ((null? (cdr strs)) (car strs))
     (else
      (string-concatenate-reverse (cdr strs)
                                  (car strs) (string-length (car strs))))))
  ;; End of SRFI-13 procedures
  (define-macro (io-dbg . a)
                                        ; `(print ,@a "\n")
    '#!void)
  (define-macro (parse-dbg . a)
                                        ; `(print ,@a "\n")
    '#!void)
  (if (not (string? xml-string)) (error "String expected" xml-string))
  (if (not (and (list? namespace-prefix-assig) (zero? (apply + (map (lambda (v) (if (pair? v) 0 -1)) namespace-prefix-assig)))))
      (error "List expected" namespace-prefix-assig))
  (let ((v (xml-string->sxml-start-at-idx)))
    (if (or (not (fixnum? v)) (fx< v 0)) (error "xml-string->sxml-start-at-idx must be a non-negative fixnum." v)))
  ;; preserve-whitespace-only-node-values is only checked for if it's set or not, so may technically have any content.
  (let* ((not-preserve-whitespace-only-node-values (not preserve-whitespace-only-node-values))
         ;; ## Data input mechanism
         ;; The default ssax-sxml distribution relies on the IO operations (read-char) and (peek-char) for all of its
         ;; XML parsing work.
         ;;      Anoter operation (peek-next-char) is frequently used. It is defined out of the previous to procedures
         ;; though; its definition is (lambda () (read-char) (peek-char)) .
         ;;      In this packaging, we use a string as input, and if we run out of content of this string before
         ;; we're done parsing, we invoke (on-need-more-data) to get a new string. This must be done carefully though;
         ;; we cannot, for instance, have any read-ahead buffer (of one or two chars, say), because the caller may
         ;; need a particular output result exactly when sufficient XML data to produce that result have been inputted;
         ;; on 'ssax processing of partial XML streams this need is noticed the most.
         ;;
         ;; This is the character position whose contents will be read on the next read-char.
         ;; The first read is made from position current-xml-string-pos + 1, so that this one is -1 is fine.
         (current-xml-string-pos (fx- (xml-string->sxml-start-at-idx) 1))
         (xml-string-length (or (xml-string->sxml-end-at-idx) (string-length xml-string)))
         (last-valid-xml-string-idx (fx- xml-string-length 1))
         (max-current-xml-string-pos-where-peek-can-be-made (fx- last-valid-xml-string-idx 1))
         (new-xml-string #f) ; xml-string to be taken into use. Returned by on-need-more-data , if set.
         ;; Either #f = none, or nonempty string = the string to be taken into use.
         (new-xml-string:start #f)
         (new-xml-string:end   #f)
         (peek-char-result #f) ; The result that any peek-char operation currently returns.
         ;; #f = not set, needs to be generated. char = that return value.
         (reached-eof #f) ; #f = did not reach eof, #!eof = reached eof.
         (eof-handler #f) ; Not used currently. Could be set to a procedure that takes no args, that is invoked
         ;; when an unexpected EOF error occurs. If that procedure returns, an exception is produced.
         (current-xml-string-pos-offset 0))
    (define-macro (io-dbg-checkpoint)
      ;; '(io-dbg "xml-string-length " xml-string-length " last-valid-xml-string-idx " last-valid-xml-string-idx
      ;;          " max-current-xml-string-pos-where-peek-can-be-made " max-current-xml-string-pos-where-peek-can-be-made)
      '#!void)
    (define-macro (xml-string->sxml-read-char)
      `(or reached-eof
           (begin
             (io-dbg-checkpoint)
             (set! peek-char-result #f)
             (set! current-xml-string-pos (fx+ current-xml-string-pos 1))
             (if (fx> current-xml-string-pos last-valid-xml-string-idx)
                 (xml-string->sxml-read-char:out-of-data)
                 (begin
                   (io-dbg "xml-string->sxml-read-char: Returns picked up char " (##string-ref xml-string current-xml-string-pos)
                           " of xml-string idx " current-xml-string-pos " (of " xml-string-length ").")
                   (##string-ref xml-string current-xml-string-pos))))))
    (define-macro (xml-string->sxml-peek-char)
      `(or peek-char-result
           (begin
             (io-dbg-checkpoint)
             (set! peek-char-result (or reached-eof
                                        (if (fx> current-xml-string-pos max-current-xml-string-pos-where-peek-can-be-made)
                                            (if new-xml-string
                                                (begin
                                                  (io-dbg "xml-string->sxml-peek-char: Using as peek-char-result, new-xml-string 's first character.")
                                                  (##string-ref new-xml-string 0))
                                                (xml-string->sxml-peek-char:out-of-data))
                                            (begin
                                              (io-dbg "xml-string->sxml-peek-char: Using as peek-char-result, idx " (fx+ current-xml-string-pos 1) " of xml-string.")
                                              (##string-ref xml-string (fx+ current-xml-string-pos 1))))))
             (io-dbg-checkpoint)
             (io-dbg "xml-string->sxml-peek-char: Generated new peek-char-result, returning (" peek-char-result ").")
             peek-char-result)))
    ;; Essentially `(begin (read-char) (peek-char)) .
    (define-macro (xml-string->sxml-peek-next-char)
      ;; = read-char:
      `(or reached-eof
           (begin
             (set! peek-char-result #f)
             (set! current-xml-string-pos (fx+ current-xml-string-pos 1))
             (io-dbg-checkpoint)
             (if (fx> current-xml-string-pos last-valid-xml-string-idx)
                 (xml-string->sxml-read-char:out-of-data))
             (io-dbg-checkpoint)
             ;; = peek-char:
             (set! peek-char-result (or reached-eof
                                        (if (fx> current-xml-string-pos max-current-xml-string-pos-where-peek-can-be-made)
                                            (if new-xml-string
                                                (begin
                                                  (io-dbg "xml-string->sxml-peek-next-char: Using as peek-char-result, new-xml-string 's first character.")
                                                  (##string-ref new-xml-string 0))
                                                (xml-string->sxml-peek-char:out-of-data))
                                            (begin
                                              (io-dbg "xml-string->sxml-peek-next-char: Using as peek-char-result, idx " (fx+ current-xml-string-pos 1) " of xml-string.")
                                              (##string-ref xml-string (fx+ current-xml-string-pos 1))))))
             (io-dbg-checkpoint)
             (io-dbg "xml-string->sxml-peek-next-char: Generated new peek-char-result, returning (" peek-char-result ").")
             peek-char-result)))
    (define (xml-string->sxml-read-char:out-of-data)
      ;; read-char reports it's out of data, so let's get new data, and return the first character of that new data,
      ;; or switch to EOF state.
      ;;
      ;; If new-xml-string is waiting for us (introduced by a previous peek-char operation), take it in use now.
      (if new-xml-string
          (begin
            (set! current-xml-string-pos-offset (fx+ current-xml-string-pos-offset xml-string-length))
            (set! xml-string new-xml-string)
            (set! new-xml-string #f)
            (set! xml-string-length new-xml-string:end)
            (set! last-valid-xml-string-idx (fx- xml-string-length 1))
            (set! max-current-xml-string-pos-where-peek-can-be-made (fx- last-valid-xml-string-idx 1))
            (set! current-xml-string-pos new-xml-string:start)
            (io-dbg "xml-string->sxml-read-char:out-of-data: invoked. Had a new-xml-string waiting from a previous peek-char,"
                    " so taking it in use and returning its starting char (" (string-ref xml-string new-xml-string:start) ")."
                    " (start " new-xml-string:start " end " new-xml-string:end ")")
            (string-ref xml-string new-xml-string:start)) ; string-ref serves as fixnum? check for new-xml-string:start.
          ;; No new-xml-string was waiting to be taken in use.
          ;;
          ;; If we have a on-need-more-data procedure, and it gives us a string with contents now, take it in use.
          (let ((new-xml-string* (and on-need-more-data
                                      (let ((v (on-need-more-data)))
                                        (if (string? v)
                                            (values v 0 (##string-length v))
                                            v)))))
            (if (and new-xml-string* (not (zero? (string-length (receive (s unused1 unused2) new-xml-string* s))))) ; string-length serves as string? check.
                (receive
                 (content start end) new-xml-string*
                 (set! current-xml-string-pos-offset (fx+ current-xml-string-pos-offset xml-string-length))
                 (set! xml-string content)
                 (set! xml-string-length end)
                 (set! last-valid-xml-string-idx (fx- xml-string-length 1))
                 (set! max-current-xml-string-pos-where-peek-can-be-made (fx- last-valid-xml-string-idx 1))
                 (set! current-xml-string-pos start)
                 (io-dbg "xml-string->sxml-read-char:out-of-data: invoked. Got new data from on-need-more-data ("
                         content " start " start " end " end "). Returning its starting char (" (string-ref xml-string start) ").")
                 (string-ref xml-string start)) ; string-ref serves as fixnum? check for start.
                (begin
                  (set! reached-eof #!eof)
                  (io-dbg "xml-string->sxml-read-char:out-of-data: invoked. Did not get new data, so returning #!eof.")
                  #!eof)))))
    (define (xml-string->sxml-peek-char:out-of-data)
                                        ; peek-char reports it's out of data, so let's get new data, and return the first character of that new data,
                                        ; or return #!eof.
      (let ((new-xml-string* (and on-need-more-data (let ((v (on-need-more-data)))
                                                      (if (string? v)
                                                          (values v 0 (##string-length v))
                                                          v)))))
        (if (and new-xml-string* (not (zero? (string-length (receive (s unused1 unused2) new-xml-string* s))))) ; string-length serves as string? check.
            (receive
             (content start end) new-xml-string*
             (set! new-xml-string       content)
             (set! new-xml-string:start start)
             (set! new-xml-string:end   end)
             (io-dbg "xml-string->sxml-peek-char:out-of-data: invoked. Got new data from on-need-more-data ("
                     content " start " start " end " end "). Returning its starting char (" (string-ref new-xml-string start) ").")
             (string-ref new-xml-string start)) ; string-ref serves as fixnum? check for start.
            (begin
              (io-dbg "xml-string->sxml-peek-char:out-of-data: invoked. Did not get new data, so returning #!eof.")
              #!eof))))
    (define-macro (skip-while skip-chars)
      (let ((r `(do ((c (xml-string->sxml-peek-char) (xml-string->sxml-peek-char)))
                    ((not (memv c ,skip-chars)) c)
                  (xml-string->sxml-read-char))))
                                        ; (print "skip-while gave:") (pp r)
        r))
    ;; According to the SSAX convention this function
    ;; accepts the port as its first argument which is used for
    ;; location of the error in input file.
    ;; Other parameters are considered as error messages,
    ;;  they are printed to stderr as is.
    ;; only device ports do carry positions (string ports don't),
    ;; return #f for those
    (define (parser-error . args)
      (define-macro (port-name port)
        `(##vector-ref ,port 4))
      (error (with-output-to-string
               "" (lambda ()
                    (if (port? (car args))
                        (begin
                          (display (string-append "Error " (port-name (car args)) " at position "
                                                  (input-port-byte-position (car args)) "\n"
                                                  " at character no. " current-xml-string-pos)
                                   (current-error-port))
                          (apply (lambda (x) (display x (current-output-port))) (cdr args))))
                    (for-each (lambda (x) (display x (current-output-port))) `(": " ,args  "\n"))))))
    (define ssax:warn
      (lambda args
        (define-macro (port-name port)
          `(##vector-ref ,port 4))
        (if
         (port? (car args))
         (string-append "Warning: " (port-name (car args))
                        " at position " (input-port-byte-position (car args)) "\n"
                        (cdr args) "\n")
         #f)))
    (define (cons* a1 a2 . rest)
      (if (null? rest)
          (cons a1 a2)
          (cons a1 (apply cons* (cons a2 rest)))))
    ;;    Looks for a string STR within the first MAX-NO-CHARS chars of the
    ;;    input port IN-PORT
    ;;    MAX-NO-CHARS may be omitted: in that case, the search span would be
    ;;    limited only by the end of the input stream.
    ;;    When the STR is found, the function returns the number of
    ;;    characters it has read from the port, and the port is set
    ;;    to read the first char after that (that is, after the STR)
    ;;    The function returns #f when the string wasn't found
    ;; Note the function reads the port *STRICTLY* sequentially, and does not
    ;; perform any buffering. So the function can be used even if the port is open
    ;; on a pipe or other communication channel.
    ;;
    ;; Probably can be classified as misc-io.
    ;;
    ;; Notes on the algorithm.
    ;; A special care should be taken in a situation when one had achieved a partial
    ;; match with (a head of) STR, and then some unexpected character appeared in
    ;; the stream. It'll be rash to discard all already read characters. Consider
    ;; an example of string "acab" and the stream "bacacab...", specifically when
    ;;    a  c  a _b_
    ;; b  a  c  a  c  a  b ...
    ;; that is, when 'aca' had matched, but then 'c' showed up in the stream
    ;; while we were looking for 'b'. In that case, discarding all already read
    ;; characters and starting the matching process from scratch, that is,
    ;; from 'c a b ...', would miss a certain match.
    ;; Note, we don't actually need to keep already read characters, or at least
    ;; strlen(str) characters in some kind of buffer. If there has been no match,
    ;; we can safely discard read characters. If there was some partial match,
    ;; we already know the characters before, they are in the STR itself, so
    ;; we don't need a special buffer for that.
    ;;
    ;; "MISCIO" Search for string from port.
    ;; Written 1995 by Oleg Kiselyov (oleg@ponder.csci.unt.edu)
    ;; Modified 1996 by A. Jaffer (jaffer@ai.mit.edu)
    (define (find-string-from-port? str . max-no-char)
      (set! max-no-char (if (null? max-no-char) #f (car max-no-char)))
      (letrec
          ((no-chars-read 0)
           (my-xml-string->sxml-peek-char ; Return a peeked char or #f
            (lambda () (and (or (not max-no-char) (< no-chars-read max-no-char))
                       (let ((c (xml-string->sxml-peek-char)))
                         (if (eof-object? c) #f c)))))
           (next-char (lambda () (xml-string->sxml-read-char)
                         (set! no-chars-read  (++ no-chars-read))))
           (match-1st-char              ; of the string str
            (lambda ()
              (let ((c (my-xml-string->sxml-peek-char)))
                (if (not c) #f
                    (begin (next-char)
                           (if (char=? c (string-ref str 0))
                               (match-other-chars 1)
                               (match-1st-char)))))))
           ;; There has been a partial match, up to the point pos-to-match
           ;; (for example, str[0] has been found in the stream)
           ;; Now look to see if str[pos-to-match] for would be found, too
           (match-other-chars
            (lambda (pos-to-match)
              (if (>= pos-to-match (string-length str))
                  no-chars-read        ; the entire string has matched
                  (let ((c (my-xml-string->sxml-peek-char)))
                    (and c
                         (if (not (char=? c (string-ref str pos-to-match)))
                             (backtrack 1 pos-to-match)
                             (begin (next-char)
                                    (match-other-chars (++ pos-to-match)))))))))
           ;; There had been a partial match, but then a wrong char showed up.
           ;; Before discarding previously read (and matched) characters, we check
           ;; to see if there was some smaller partial match. Note, characters read
           ;; so far (which matter) are those of str[0..matched-substr-len - 1]
           ;; In other words, we will check to see if there is such i>0 that
           ;; substr(str,0,j) = substr(str,i,matched-substr-len)
           ;; where j=matched-substr-len - i
           (backtrack
            (lambda (i matched-substr-len)
              (let ((j (- matched-substr-len i)))
                (if (<= j 0)
                    (match-1st-char) ; backed off completely to the begining of str
                    (let loop ((k 0))
                      (if (>= k j)
                          (match-other-chars j) ; there was indeed a shorter match
                          (if (char=? (string-ref str k)
                                      (string-ref str (+ i k)))
                              (loop (++ k))
                              (backtrack (++ i) matched-substr-len)))))))))
        (match-1st-char)))
    (let ((ssax:S-chars (map integer->char '(32 10 9 13))))
      ;;	ucscode->char INT -> CHAR
      ;; Return a character whose UCS (ISO/IEC 10646) code is INT
      ;; Note
      ;; This function is required for processing of XML character entities:
      ;; According to Section "4.1 Character and Entity References"
      ;; of the XML Recommendation:
      ;;  "[Definition: A character reference refers to a specific character
      ;;   in the ISO/IEC 10646 character set, for example one not directly
      ;;   accessible from available input devices.]"
      (define (ucscode->char code)
        (integer->char code))
      ;; -- procedure+: assert-curr-char CHAR-LIST STRING [PORT]
      ;;	Reads a character from theand looks it up
      ;;	in the CHAR-LIST of expected characters
      ;;	If the read character was found among expected, it is returned
      ;;	Otherwise, the procedure writes a nasty message using STRING
      ;;	as a comment, and quits.
      ;;	The optional argumentdefaults to the current input port.
      ;;
      (define (assert-curr-char expected-chars comment)
        (let ((c (xml-string->sxml-read-char)))
          (if (memq c expected-chars) c
              (begin
                (if (and eof-handler (eof-object? c))
                    (eof-handler))
                (parser-error "Wrong character " c
                              " (0x" (if (eof-object? c) "*eof*"
                                         (number->string (char->integer c) 16)) ") "
                              comment ". " expected-chars " expected")))))
      ;; -- procedure+: skip-until CHAR-LIST [PORT]
      ;;	Reads and skips characters from theuntil one of the break
      ;;	characters is encountered. This break character is returned.
      ;;	The break characters are specified as the CHAR-LIST. This list
      ;;	may include EOF, which is to be coded as a symbol *eof*
      ;;
      ;; -- procedure+: skip-until NUMBER [PORT]
      ;;	Skips the specified NUMBER of characters from theand returns #f
      ;;
      ;;	The optional argumentdefaults to the current input port.
      (define (skip-until arg)
        (cond
         ((number? arg)                 ; skip 'arg' characters
          (do ((i arg (-- i)))
              ((<= i 0) #f)
            (if (eof-object? (xml-string->sxml-read-char))
                (begin
                  (if eof-handler (eof-handler))
                  (parser-error "Unexpected EOF while skipping "
                                arg " characters")))))
         (else                         ; skip until break-chars (=arg)
          (let loop ((c (xml-string->sxml-read-char)))
            (cond
             ((memv c arg) c)
             ((eof-object? c)
              (if (memv '*eof* arg)
                  c
                  (begin
                    (if eof-handler (eof-handler))
                    (parser-error "Unexpected EOF while skipping until " arg))))
             (else (loop (xml-string->sxml-read-char))))))))
      ;; -- procedure+: skip-while CHAR-LIST [PORT]
      ;;	Reads characters from theand disregards them,
      ;;	as long as they are mentioned in the CHAR-LIST.
      ;;	The first character (which may be EOF) peeked from the stream
      ;;	that is NOT a member of the CHAR-LIST is returned. This character
      ;;	is left on the stream.
      ;;	The optional argumentdefaults to the current input port.
      ;;
      ;; Streak tokenizers
      ;; -- procedure+:
      ;;    next-token PREFIX-CHAR-LIST BREAK-CHAR-LIST [COMMENT-STRING] [PORT]
      ;;	skips any number of the prefix characters (members of the
      ;;	PREFIX-CHAR-LIST), if any, and reads the sequence of characters
      ;;	up to (but not including) a break character, one of the
      ;;	BREAK-CHAR-LIST.
      ;;	The string of characters thus read is returned.
      ;;	The break character is left on the input stream
      ;;	The list of break characters may include EOF, which is to be coded as
      ;;	a symbol *eof*. Otherwise, EOF is fatal, generating an error message
      ;;	including a specified COMMENT-STRING (if any)
      ;;
      ;;	The optional argumentdefaults to the current input port.
      ;;
      ;; Note: since we can't tell offhand how large the token being read is
      ;; going to be, we make a guess, pre-allocate a string, and grow it by
      ;; quanta if necessary. The quantum is always the length of the string
      ;; before it was extended the last time. Thus the algorithm does
      ;; a Fibonacci-type extension, which has been proven optimal.
      ;; Note, explicitspecification in xml-string->sxml-read-char, xml-string->sxml-peek-char helps.
      ;;
      ;; Procedure input-parse:init-buffer
      ;; returns an initial buffer for next-token* procedures.
      ;; The input-parse:init-buffer may allocate a new buffer per each invocation:
      ;;	(define (input-parse:init-buffer) (make-string 32))
      ;; Size 32 turns out to be fairly good, on average.
      ;; That policy is good only when a Scheme system is multi-threaded with
      ;; preemptive scheduling, or when a Scheme system supports shared substrings.
      ;; In all the other cases, it's better for input-parse:init-buffer to
      ;; return the same static buffer. next-token* functions return a copy
      ;; (a substring) of accumulated data, so the same buffer can be reused.
      ;; We shouldn't worry about new token being too large: next-token will use
      ;; a larger buffer automatically. Still, the best size for the static buffer
      ;; is to allow most of the tokens to fit in.
      ;; Using a static buffer _dramatically_ reduces the amount of produced garbage
      ;; (e.g., during XML parsing).
      (define input-parse:init-buffer
        (let ((buffer (make-string 512)))
          (lambda () buffer)))
      (define* (next-token prefix-skipped-chars break-chars (comment ""))
        (let* ((buffer (input-parse:init-buffer))
               (curr-buf-len (string-length buffer)) (quantum 16))
          (let loop ((i 0) (c (skip-while prefix-skipped-chars)))
                                        ; (print "next-token iterates with prefix-skipped-chars=") (write prefix-skipped-chars)
                                        ; (print ", i=" i ", c=") (write c) (print "\n")
            (cond
             ((memq c break-chars) (substring buffer 0 i))
             ((eof-object? c)
              (if (memq '*eof* break-chars)
                  (substring buffer 0 i) ; was EOF expected?
                  (begin
                    (if eof-handler (eof-handler))
                    (parser-error "EOF while reading a token " comment))))
             (else
              (if (>= i curr-buf-len) ; make space for i-th char in buffer
                  (begin           ; -> grow the buffer by the quantum
                    (set! buffer (string-append buffer (make-string quantum)))
                    (set! quantum curr-buf-len)
                    (set! curr-buf-len (string-length buffer))))
              (string-set! buffer i c)
              (xml-string->sxml-read-char) ; move to the next char
              (loop (++ i) (xml-string->sxml-peek-char)))))))
      ;; Another version of next-token, accumulating characters in a list rather
      ;; than in a string buffer. I heard that it tends to work faster.
      ;; In reality, it works just as fast as the string buffer version above,
      ;; but it allocates 50% more memory and thus has to run garbage collection
      ;; 50% as many times. See next-token-comp.scm
      (define* (next-token-list-based prefix-skipped-chars break-chars (comment ""))
        (let* ((first-char (skip-while prefix-skipped-chars))
               (accum-chars (cons first-char '())))
          (cond
           ((eof-object? first-char)
            (if (memq '*eof* break-chars) ""
                (begin
                  (if eof-handler (eof-handler))
                  (parser-error "EOF while skipping before reading token "
                                comment))))
           ((memq first-char break-chars) "")
           (else
            (xml-string->sxml-read-char) ; consume the first-char
            (let loop ((tail accum-chars) (c (xml-string->sxml-peek-char)))
              (cond
               ((memq c break-chars) (list->string accum-chars))
               ((eof-object? c)
                (if (memq '*eof* break-chars)
                    (list->string accum-chars) ; was EOF expected?
                    (begin
                      (if eof-handler (eof-handler))
                      (parser-error "EOF while reading a token " comment))))
               (else
                (xml-string->sxml-read-char) ; move to the next char
                (set-cdr! tail (cons c '()))
                (loop (cdr tail) (xml-string->sxml-peek-char)))))))))
      ;; -- procedure+: next-token-of INC-CHARSET [PORT]
      ;;	Reads characters from thethat belong to the list of characters
      ;;	INC-CHARSET. The reading stops at the first character which is not
      ;;	a member of the set. This character is left on the stream.
      ;;	All the read characters are returned in a string.
      ;;
      ;; -- procedure+: next-token-of PRED [PORT]
      ;;	Reads characters from thefor which PRED (a procedure of one
      ;;	argument) returns non-#f. The reading stops at the first character
      ;;	for which PRED returns #f. That character is left on the stream.
      ;;	All the results of evaluating of PRED up to #f are returned in a
      ;;	string.
      ;;
      ;;	PRED is a procedure that takes one argument (a character
      ;;	or the EOF object) and returns a character or #f. The returned
      ;;	character does not have to be the same as the input argument
      ;;	to the PRED. For example,
      ;;	(next-token-of (lambda (c)
      ;;			  (cond ((eof-object? c) #f)
      ;;				((char-alphabetic? c) (char-downcase c))
      ;;				(else #f))))
      ;;	will try to read an alphabetic token from the current
      ;;	input port, and return it in lower case.
      ;;
      ;;	The optional argumentdefaults to the current input port.
      ;;
      ;; Note: since we can't tell offhand how large the token being read is
      ;; going to be, we make a guess, pre-allocate a string, and grow it by
      ;; quanta if necessary. The quantum is always the length of the string
      ;; before it was extended the last time. Thus the algorithm does
      ;; a Fibonacci-type extension, which has been proven optimal.
      ;;
      ;; This procedure is similar to next-token but only it implements
      ;; an inclusion rather than delimiting semantics.
      (define (next-token-of incl-list/pred)
        (let* ((buffer (input-parse:init-buffer))
               (curr-buf-len (string-length buffer)) (quantum 16))
          (if (procedure? incl-list/pred)
              (let loop ((i 0) (c (xml-string->sxml-peek-char)))
                (cond
                 ((incl-list/pred c) =>
                  (lambda (c)
                    (if (>= i curr-buf-len) ; make space for i-th char in buffer
                        (begin     ; -> grow the buffer by the quantum
                          (buffer (string-append buffer (make-string quantum)))
                          (set! quantum curr-buf-len)
                          (set! curr-buf-len (string-length buffer))))
                    (string-set! buffer i c)
                    (xml-string->sxml-read-char) ; move to the next char
                    (loop (++ i) (xml-string->sxml-peek-char))))
                 (else (substring buffer 0 i))))
                                        ; incl-list/pred is a list of allowed characters
              (let loop ((i 0) (c (xml-string->sxml-peek-char)))
                (cond
                 ((not (memq c incl-list/pred)) (substring buffer 0 i))
                 (else
                  (if (>= i curr-buf-len) ; make space for i-th char in buffer
                      (begin       ; -> grow the buffer by the quantum
                        (set! buffer (string-append buffer (make-string quantum)))
                        (set! quantum curr-buf-len)
                        (set! curr-buf-len (string-length buffer))))
                  (string-set! buffer i c)
                  (xml-string->sxml-read-char) ; move to the next char
                  (loop (++ i) (xml-string->sxml-peek-char))))))))
      (define (read-string n)
        (if (not (positive? n))
            ""
            (let ((buffer (make-string n)))
              (let loop ((i 0) (c (xml-string->sxml-read-char)))
                (if (eof-object? c)
                    (substring buffer 0 i)
                    (let ((i1 (++ i)))
                      (string-set! buffer i c)
                      (if (= i1 n) buffer (loop i1 (xml-string->sxml-read-char)))))))))
      ;; ## SSAX-code.sch
      ;; $Id: SSAX.scm,v 1.8 2003/08/02 02:14:18 oleg Exp $
      ;; DL: let*-values rewritten to call-with-values
      (define-macro xml-token-kind (lambda (token) `(car ,token)))
      (define-macro xml-token-head (lambda (token) `(cdr ,token)))
      (define-macro ssax:make-pi-parser
        (lambda (my-pi-handlers)
          `(lambda (target seed)
             (case target
               (unquote-splicing
                (let loop ((pi-handlers my-pi-handlers) (default #f))
                  (cond
                   ((null? pi-handlers)
                    (if default
                        `((else (,default target seed)))
                        '((else
                           (ssax:warn "Skipping PI: " target "\n")
                           (ssax:skip-pi)
                           seed))))
                   ((eq? '*DEFAULT* (caar pi-handlers))
                    (loop (cdr pi-handlers) (cdar pi-handlers)))
                   (else
                    (cons
                     `((,(caar pi-handlers)) (,(cdar pi-handlers) target seed))
                     (loop (cdr pi-handlers) default))))))))))
      (define ssax:complete-start-tag 'not-yet-set)
      (define-macro ssax:make-elem-parser
        (lambda (my-new-level-seed
            my-finish-element
            my-char-data-handler
            my-pi-handlers)
          `(lambda (start-tag-head elems entities namespaces preserve-ws? seed)
             (define xml-space-gi (cons ssax:Prefix-XML (string->symbol "space")))
             (let handle-start-tag ((start-tag-head start-tag-head)
                                    (entities entities)
                                    (namespaces namespaces)
                                    (preserve-ws? preserve-ws?)
                                    (parent-seed seed))
               (call-with-values
                   (lambda () (ssax:complete-start-tag
                          start-tag-head
                          elems
                          entities
                          namespaces))
                 (lambda (elem-gi attributes namespaces expected-content)
                   (let ((seed (,my-new-level-seed
                                elem-gi
                                attributes
                                namespaces
                                expected-content
                                parent-seed)))
                     (case expected-content
                       ((EMPTY-TAG)
                        (,my-finish-element
                         elem-gi
                         attributes
                         namespaces
                         parent-seed
                         seed))
                       ((EMPTY)
                        (ssax:assert-token
                         (and (eqv? #\< (ssax:skip-S))
                              (ssax:read-markup-token))
                         'END
                         start-tag-head
                         (lambda (token exp-kind exp-head)
                           (parser-error
                            "[elementvalid] broken for "
                            token
                            " while expecting "
                            exp-kind
                            exp-head)))
                        (,my-finish-element
                         elem-gi
                         attributes
                         namespaces
                         parent-seed
                         seed))
                       (else
                        (let ((preserve-ws?
                               (cond
                                ((assoc xml-space-gi attributes)
                                 =>
                                 (lambda (name-value)
                                   (equal? "preserve" (cdr name-value))))
                                (else preserve-ws?))))
                          (let loop ((entities entities)
                                     (expect-eof? #f)
                                     (seed seed))
                            (call-with-values
                                (lambda () (ssax:xml-string->sxml-read-char-data
                                       expect-eof?
                                       ,my-char-data-handler
                                       seed))
                              (lambda (seed term-token)
                                (if (eof-object? term-token)
                                    seed
                                    (case (xml-token-kind term-token)
                                      ((END)
                                       (ssax:assert-token
                                        term-token
                                        'END
                                        start-tag-head
                                        (lambda (token exp-kind exp-head)
                                          (parser-error
                                           "[GIMatch] broken for "
                                           term-token
                                           " while expecting "
                                           exp-kind
                                           exp-head)))
                                       (,my-finish-element
                                        elem-gi
                                        attributes
                                        namespaces
                                        parent-seed
                                        seed))
                                      ((PI)
                                       (let ((seed
                                              ((ssax:make-pi-parser ,my-pi-handlers)
                                               (xml-token-head term-token)
                                               seed)))
                                         (loop entities expect-eof? seed)))
                                      ((ENTITY-REF)
                                       (let ((seed
                                              (ssax:handle-parsed-entity
                                               (xml-token-head term-token)
                                               entities
                                               (lambda (entities seed)
                                                 (loop entities #t seed))
                                               ,my-char-data-handler
                                               seed)))
                                         (loop entities expect-eof? seed)))
                                      ((START)
                                       (if (eq? expected-content 'PCDATA)
                                           (parser-error
                                            "[elementvalid] broken for "
                                            elem-gi
                                            " with char content only; unexpected token "
                                            term-token))
                                       (let ((seed
                                              (handle-start-tag
                                               (xml-token-head term-token)
                                               entities
                                               namespaces
                                               preserve-ws?
                                               seed)))
                                         (loop entities expect-eof? seed)))
                                      (else
                                       (parser-error
                                        "XML [43] broken for "
                                        term-token)))))))))))))))))
      (define-macro ssax:make-parser
        (lambda user-handlers
          (define all-handlers
            '((DOCTYPE
               lambda
               (docname systemid internal-subset? seed)
               (when internal-subset?
                     (ssax:warn "Internal DTD subset is not currently handled ")
                     (ssax:skip-internal-dtd))
               (ssax:warn
                "DOCTYPE DECL "
                docname
                " "
                systemid
                " found and skipped")
               (values #f '() '() seed))
              (UNDECL-ROOT lambda (elem-gi seed) (values #f '() '() seed))
              (DECL-ROOT lambda (elem-gi seed) seed)
              (NEW-LEVEL-SEED . REQD)
              (FINISH-ELEMENT . REQD)
              (CHAR-DATA-HANDLER . REQD)
              (PI)))
          (define (delete-assoc alist tag cont)
            (let loop ((alist alist) (scanned '()))
              (cond
               ((null? alist) (error "Unknown user-handler-tag: " tag))
               ((eq? tag (caar alist))
                (cont tag (cdar alist) (append scanned (cdr alist))))
               (else (loop (cdr alist) (cons (car alist) scanned))))))
          (define (merge-handlers declared-handlers given-handlers)
            (cond
             ((null? given-handlers)
              (cond
               ((null? declared-handlers) '())
               ((not (eq? 'REQD (cdar declared-handlers)))
                (cons
                 (car declared-handlers)
                 (merge-handlers (cdr declared-handlers) given-handlers)))
               (else
                (error
                 "The handler for the tag "
                 (caar declared-handlers)
                 " must be specified"))))
             ((null? (cdr given-handlers))
              (error "Odd number of arguments to ssax:make-parser"))
             (else
              (delete-assoc
               declared-handlers
               (car given-handlers)
               (lambda (tag value alist)
                 (cons
                  (cons tag (cadr given-handlers))
                  (merge-handlers alist (cddr given-handlers))))))))
          (let ((user-handlers (merge-handlers all-handlers user-handlers)))
            (define (get-handler tag)
              (cond
               ((assq tag user-handlers) => cdr)
               (else (error "unknown tag: " tag))))
            `(lambda (seed)
               (define (handle-decl token-head seed)
                 (or (eq? (string->symbol "DOCTYPE") token-head)
                     (parser-error
                      "XML [22], expected DOCTYPE declaration, found "
                      token-head))
                 (assert-curr-char ssax:S-chars "XML [28], space after DOCTYPE")
                 (ssax:skip-S)
                 (let* ((docname (ssax:read-QName))
                        (systemid
                         (and (ssax:ncname-starting-char? (ssax:skip-S))
                              (ssax:read-external-id)))
                        (internal-subset?
                         (begin
                           (ssax:skip-S)
                           (eqv?
                            #\[
                            (assert-curr-char
                             '(#\> #\[)
                             "XML [28], end-of-DOCTYPE"
                             )))))
                   (call-with-values
                       (lambda () (,(get-handler 'DOCTYPE)
                              docname
                              systemid
                              internal-subset?
                              seed))
                     (lambda (elems entities namespaces seed)
                       (scan-for-significant-prolog-token-2
                        elems
                        entities
                        namespaces
                        seed)))))
               (define (scan-for-significant-prolog-token-1 seed)
                 (let ((token (ssax:scan-Misc)))
                   (if (eof-object? token)
                       (begin
                         (if eof-handler (eof-handler))
                         (parser-error "XML [22], unexpected EOF"))
                       (case (xml-token-kind token)
                         ((PI)
                          (let ((seed
                                 ((ssax:make-pi-parser ,(get-handler 'PI))
                                  (xml-token-head token)
                                  seed)))
                            (scan-for-significant-prolog-token-1 seed)))
                         ((DECL) (handle-decl (xml-token-head token) seed))
                         ((START)
                          (call-with-values
                              (lambda () (,(get-handler 'UNDECL-ROOT)
                                     (xml-token-head token)
                                     seed))
                            (lambda (elems entities namespaces seed)
                              (element-parser
                               (xml-token-head token)
                               elems
                               entities
                               namespaces
                               #f
                               seed))))
                         (else
                          (parser-error "XML [22], unexpected markup " token))))))
               (define (scan-for-significant-prolog-token-2
                        elems
                        entities
                        namespaces
                        seed)
                 (let ((token (ssax:scan-Misc)))
                   (if (eof-object? token)
                       (begin
                         (if eof-handler (eof-handler))
                         (parser-error "XML [22], unexpected EOF"))
                       (case (xml-token-kind token)
                         ((PI)
                          (let ((seed
                                 ((ssax:make-pi-parser ,(get-handler 'PI))
                                  (xml-token-head token)
                                  seed)))
                            (scan-for-significant-prolog-token-2
                             elems
                             entities
                             namespaces
                             seed)))
                         ((START)
                          (element-parser
                           (xml-token-head token)
                           elems
                           entities
                           namespaces
                           #f
                           (,(get-handler 'DECL-ROOT) (xml-token-head token) seed)))
                         (else
                          (parser-error "XML [22], unexpected markup " token))))))
               (define element-parser
                 (ssax:make-elem-parser
                  ,(get-handler 'NEW-LEVEL-SEED)
                  ,(get-handler 'FINISH-ELEMENT)
                  ,(get-handler 'CHAR-DATA-HANDLER)
                  ,(get-handler 'PI)))
               ;; (display "(scan-for-significant-prolog-token-1 seed) out: ") (pp(scan-for-significant-prolog-token-1 seed)) (display "\n(end)")
               (scan-for-significant-prolog-token-1 seed)))))
      ;; ## SSAX-code.scm
      (define (make-xml-token kind head) (cons kind head))
      (define xml-token? pair?)
      (define (string-whitespace? str)
        (let ((len (string-length str)))
          (cond
           ((zero? len) #t)
           ((= 1 len) (char-whitespace? (string-ref str 0)))
           ((= 2 len)
            (and (char-whitespace? (string-ref str 0))
                 (char-whitespace? (string-ref str 1))))
           (else
            (let loop ((i 0))
              (or (>= i len)
                  (and (char-whitespace? (string-ref str i)) (loop (++ i)))))))))
      (define (assq-values val alist)
        (let loop ((alist alist) (scanned '()))
          (cond
           ((null? alist) (values #f scanned))
           ((equal? val (caar alist))
            (values (car alist) (append scanned (cdr alist))))
           (else (loop (cdr alist) (cons (car alist) scanned))))))
      (define (fold-right kons knil lis1)
        (let recur ((lis lis1))
          (if (null? lis)
              knil
              (let ((head (car lis))) (kons head (recur (cdr lis)))))))
      (define (fold kons knil lis1)
        (let lp ((lis lis1) (ans knil))
          (if (null? lis) ans (lp (cdr lis) (kons (car lis) ans)))))
      (define (ssax:skip-S) (skip-while ssax:S-chars))
      (define (ssax:ncname-starting-char? a-char)
        (and (char? a-char) (or (char-alphabetic? a-char) (char=? #\_ a-char))))
      (define (ssax:read-NCName)
        (let ((first-char (xml-string->sxml-peek-char)))
          (or (ssax:ncname-starting-char? first-char)
              (parser-error"XMLNS [4] for '" first-char "'")))
        (string->symbol
         (next-token-of
          (lambda (c)
            (cond
             ((eof-object? c) #f)
             ((char-alphabetic? c) c)
             ((string-index "0123456789.-_" c) c)
             (else #f))))))
      (define (ssax:read-QName)
        (let ((prefix-or-localpart (ssax:read-NCName)))
          (case (xml-string->sxml-peek-char)
            ((#\:)
             (xml-string->sxml-read-char)
             (cons prefix-or-localpart (ssax:read-NCName)))
            (else prefix-or-localpart))))
      (define ssax:Prefix-XML (string->symbol "xml"))
      (define name-compare
        (letrec ((symbol-compare
                  (lambda (symb1 symb2)
                    (cond
                     ((eq? symb1 symb2) '=)
                     ((string<? (symbol->string symb1) (symbol->string symb2)) '<)
                     (else '>)))))
          (lambda (name1 name2)
            (cond
             ((symbol? name1) (if (symbol? name2) (symbol-compare name1 name2) '<))
             ((symbol? name2) '>)
             ((eq? name2 ssax:largest-unres-name) '<)
             ((eq? name1 ssax:largest-unres-name) '>)
             ((eq? (car name1) (car name2)) (symbol-compare (cdr name1) (cdr name2)))
             (else (symbol-compare (car name1) (car name2)))))))
      (define ssax:largest-unres-name
        (cons (string->symbol "#LARGEST-SYMBOL") (string->symbol "#LARGEST-SYMBOL")))
      (define ssax:read-markup-token
        (let ()
          (define (skip-comment)
            (assert-curr-char '(#\-) "XML [15], second dash")
            (if (not (find-string-from-port? "-->"))
                (parser-error "XML [15], no -->"))
            (make-xml-token 'COMMENT #f))
          (define (read-cdata)
            (assert (string=? "CDATA[" (read-string 6)))
            (make-xml-token 'CDSECT #f))
          (lambda ()
            (assert-curr-char '(#\<) "start of the token")
            (let ((v (case (xml-string->sxml-peek-char)
                       ((#\/)
                        (xml-string->sxml-read-char)
                        (begin0
                         (make-xml-token 'END (ssax:read-QName))
                         (ssax:skip-S)
                         (assert-curr-char '(#\>) "XML [42]")))
                       ((#\?) (xml-string->sxml-read-char) (make-xml-token 'PI (ssax:read-NCName)))
                       ((#\!)
                        (case (xml-string->sxml-peek-next-char)
                          ((#\-) (xml-string->sxml-read-char) (skip-comment))
                          ((#\[) (xml-string->sxml-read-char) (read-cdata))
                          (else (make-xml-token 'DECL (ssax:read-NCName)))))
                       (else (make-xml-token 'START (ssax:read-QName))))))
              ;; (print "read-markup-token returns: " v "\n")
              v))))
      (define (ssax:skip-pi)
        (if (not (find-string-from-port? "?>"))
            (parser-error"Failed to find ?> terminating the PI")))
      (define (ssax:read-pi-body-as-string)
        (ssax:skip-S)
        (string-concatenate/shared
         (let loop ()
           (let ((pi-fragment (next-token '() '(#\?) "reading PI content")))
             (if (eqv? #\> (xml-string->sxml-peek-next-char))
                 (begin (xml-string->sxml-read-char) (cons pi-fragment '()))
                 (cons* pi-fragment "?" (loop)))))))
      (define (ssax:skip-internal-dtd)
        (if (not (find-string-from-port? "]>"))
            (parser-error
             "Failed to find ]> terminating the internal DTD subset")))
      (define ssax:read-cdata-body
        (let ((cdata-delimiters (list #\return #\newline #\] #\&)))
          (lambda (str-handler seed)
            (let ((r
                   (let loop ((seed seed))
                     ;; (print "ssax:read-cdata-body iterates with seed=") (write seed) (print "\n")
                     (let ((fragment
                            (next-token '() cdata-delimiters "reading CDATA"))
                           (c (xml-string->sxml-read-char)))
                       ;; (print "ssax:read-cdata-body handles fragment=") (write fragment) (print ", char=") (write c) (print "\n")
                       ;; (step)
                       (case c
                         ((#\newline) (loop (str-handler fragment "\n" seed)))
                         ((#\])
                          (if (not (eqv? (xml-string->sxml-peek-char) #\]))
                              (loop (str-handler fragment "]" seed))
                              (let check-after-second-braket ((seed
                                                               (if (string-null? fragment)
                                                                   seed
                                                                   (str-handler
                                                                    fragment
                                                                    ""
                                                                    seed))))
                                (let ((c (xml-string->sxml-peek-next-char)))
                                  ;; (print "check-after-second-braket: seed=") (write seed) (print ", c=")
                                  ;; (write c) (print "\n")
                                  (case c
                                    ((#\>) (xml-string->sxml-read-char) seed)
                                    ((#\])
                                     (check-after-second-braket (str-handler "]" "" seed)))
                                    (else (loop (str-handler "]]" "" seed))))))))
                         ((#\&)
                          (let ((ent-ref
                                 (next-token-of
                                  (lambda (c)
                                    (and (not (eof-object? c)) (char-alphabetic? c) c)))))
                            (cond
                             ((and (string=? "gt" ent-ref) (eqv? (xml-string->sxml-peek-char) #\;))
                              (xml-string->sxml-read-char)
                              (loop (str-handler fragment ">" seed)))
                             (else
                              (loop
                               (str-handler ent-ref "" (str-handler fragment "&" seed)))))))
                         (else
                          (if (eqv? (xml-string->sxml-peek-char) #\newline) (xml-string->sxml-read-char))
                          (loop (str-handler fragment "\n" seed))))))))
              ;; (print "ssax:read-cdata-body returns ") (write r) (print "\n")
              r))))
      (define (ssax:xml-string->sxml-read-char-ref)
        (let* ((base
                (cond ((eqv? (xml-string->sxml-peek-char) #\x) (xml-string->sxml-read-char) 16) (else 10)))
               (name (next-token '() '(#\;) "XML [66]"))
               (char-code (string->number name base)))
          (xml-string->sxml-read-char)
          (if (integer? char-code)
              (ucscode->char char-code)
              (parser-error"[wf-Legalchar] broken for '" name "'"))))
      (define (ssax:handle-parsed-entity
               name
               entities
               content-handler
               str-handler
               seed)
        (cond
         ((assq name entities)
          =>
          (lambda (decl-entity)
            (let ((ent-body (cdr decl-entity))
                  (new-entities (cons (cons name #f) entities)))
              (cond
               ((string? ent-body)
                (call-with-input-string
                 ent-body
                 (lambda () (content-handler new-entities seed))))
               ((procedure? ent-body)
                ;;(let ((port (ent-body)))
                (begin0
                 (content-handler new-entities seed)
                 (close-input-port)))   ;)
               (else (parser-error "[norecursion] broken for " name))))))
         ((assoc (symbol->string name) html-entity-unicode-chars)
          =>
          (lambda (decl-entity) (str-handler (cdr decl-entity) "" seed)))
         (else (parser-error"[wf-entdeclared] broken for " name))))
      (define (make-empty-attlist) '())
      (define (attlist-add attlist name-value)
        (if (null? attlist)
            (cons name-value attlist)
            (case (name-compare (car name-value) (caar attlist))
              ((=) #f)
              ((<) (cons name-value attlist))
              (else (cons (car attlist) (attlist-add (cdr attlist) name-value))))))
      (define attlist-null? null?)
      (define (attlist-remove-top attlist) (values (car attlist) (cdr attlist)))
      (define (attlist->alist attlist) attlist)
      (define-macro (attlist-fold . params) `(fold ,@params))
      (define ssax:read-attributes
        (let ((value-delimeters (append ssax:S-chars '(#\< #\&))))
          (define (read-attrib-value delimiter entities prev-fragments)
            (let* ((new-fragments
                    (cons
                     (next-token
                      '()
                      (cons delimiter value-delimeters)
                      "XML [10]")
                     prev-fragments))
                   (cterm (xml-string->sxml-read-char)))
              (cond
               ((or (eof-object? cterm) (eqv? cterm delimiter)) new-fragments)
               ((eqv? cterm #\return)
                (if (eqv? (xml-string->sxml-peek-char) #\newline) (xml-string->sxml-read-char))
                (read-attrib-value delimiter entities (cons " " new-fragments)))
               ((memv cterm ssax:S-chars)
                (read-attrib-value delimiter entities (cons " " new-fragments)))
               ((eqv? cterm #\&)
                (cond
                 ((eqv? (xml-string->sxml-peek-char) #\#)
                  (xml-string->sxml-read-char)
                  (read-attrib-value
                   delimiter
                   entities
                   (cons (string (ssax:xml-string->sxml-read-char-ref)) new-fragments)))
                 (else
                  (read-attrib-value
                   delimiter
                   entities
                   (read-named-entity entities new-fragments)))))
               (else (parser-error"[CleanAttrVals] broken")))))
          (define (read-named-entity entities fragments)
            (let ((name (ssax:read-NCName)))
              (assert-curr-char '(#\;) "XML [68]")
              (ssax:handle-parsed-entity
               name
               entities
               (lambda (entities fragments)
                 (read-attrib-value '*eof* entities fragments))
               (lambda (str1 str2 fragments)
                 (if (equal? "" str2)
                     (cons str1 fragments)
                     (cons* str2 str1 fragments)))
               fragments)))
          (lambda (entities)
            (let loop ((attr-list (make-empty-attlist)))
              (if (not (ssax:ncname-starting-char? (ssax:skip-S)))
                  attr-list
                  (let ((name (ssax:read-QName)))
                    (ssax:skip-S)
                    (assert-curr-char '(#\=) "XML [25]")
                    (ssax:skip-S)
                    (let ((delimiter (assert-curr-char '(#\' #\") "XML [10]")))
                      (loop
                       (or (attlist-add
                            attr-list
                            (cons
                             name
                             (string-concatenate-reverse/shared
                              (read-attrib-value delimiter entities '()))))
                           (parser-error
                            "[uniqattspec] broken for "
                            name))))))))))
      (define (ssax:resolve-name unres-name namespaces apply-default-ns?)
        (cond
         ((pair? unres-name)
          (cons
           (cond
            ((assq (car unres-name) namespaces) => cadr)
            ((eq? (car unres-name) ssax:Prefix-XML) ssax:Prefix-XML)
            (else
             (parser-error "[nsc-NSDeclared] broken; prefix " (car unres-name))))
           (cdr unres-name)))
         (apply-default-ns?
          (let ((default-ns (assq '*DEFAULT* namespaces)))
            (if (and default-ns (cadr default-ns))
                (cons (cadr default-ns) unres-name)
                unres-name)))
         (else unres-name)))
      (define (ssax:uri-string->symbol uri-str) (string->symbol uri-str))
      ;; cut out ssax:complete-start-tag
      (define (ssax:read-external-id)
        (let ((discriminator (ssax:read-NCName)))
          (assert-curr-char ssax:S-chars "space after SYSTEM or PUBLIC")
          (ssax:skip-S)
          (let ((delimiter (assert-curr-char '(#\' #\") "XML [11], XML [12]")))
            (cond
             ((eq? discriminator (string->symbol "SYSTEM"))
              (begin0
               (next-token '() (list delimiter) "XML [11]")
               (xml-string->sxml-read-char)))
             ((eq? discriminator (string->symbol "PUBLIC"))
              (skip-until (list delimiter))
              (assert-curr-char ssax:S-chars "space after PubidLiteral")
              (ssax:skip-S)
              (let* ((delimiter (assert-curr-char '(#\' #\") "XML [11]"))
                     (systemid (next-token '() (list delimiter) "XML [11]")))
                (xml-string->sxml-read-char)
                systemid))
             (else
              (parser-error
               "XML [75], "
               discriminator
               " rather than SYSTEM or PUBLIC"))))))
      (define (ssax:scan-Misc)
        (let loop ((c (ssax:skip-S)))
          (cond
           ((eof-object? c) c)
           ((not (char=? c #\<))
            (parser-error"XML [22], char '" c "' unexpected"))
           (else
            (let ((token (ssax:read-markup-token)))
              (case (xml-token-kind token)
                ((COMMENT) (loop (ssax:skip-S)))
                ((PI DECL START) token)
                (else
                 (parser-error
                  "XML [22], unexpected token of kind "
                  (xml-token-kind token)))))))))
      (define ssax:xml-string->sxml-read-char-data
        (let ((terminators-usual (list #\< #\& #\return))
              (terminators-usual-eof (list #\< '*eof* #\& #\return))
              (handle-fragment
               (lambda (fragment str-handler seed)
                 (if (string-null? fragment) seed (str-handler fragment "" seed)))))
          (lambda (expect-eof? str-handler seed)
            (let ((r (begin
                       ;; (let ((c (xml-string->sxml-peek-char)))
                       ;; (print "Important peek-char returns ") (write c) (print "\n"))
                       (if (eqv? #\< (xml-string->sxml-peek-char))
                           (let ((token (ssax:read-markup-token)))
                             ;; (print "token=" token ", (xml-token-kind token)=" (xml-token-kind token) "\n")
                             (case (xml-token-kind token)
                               ((START END)
                                ;; (print "iloop: (START END)\n")
                                (values seed token))
                               ((CDSECT)
                                ;; (print "iloop: Entered CDSECT.\n")
                                (let ((seed (ssax:read-cdata-body str-handler seed)))
                                  ;; (print "iloop: Recursing.\n")
                                  (ssax:xml-string->sxml-read-char-data expect-eof? str-handler seed)))
                               ((COMMENT) (ssax:xml-string->sxml-read-char-data expect-eof? str-handler seed))
                               (else
                                ;; (print "iloop: /else/\n")
                                (values seed token))))
                           (let ((char-data-terminators
                                  (if expect-eof? terminators-usual-eof terminators-usual)))
                             (let loop ((seed seed))
                               (let* ((fragment
                                       (next-token
                                        '()
                                        char-data-terminators
                                        "reading char data"))
                                      (term-char (xml-string->sxml-peek-char)))
                                 (if (eof-object? term-char)
                                     (begin
                                       ;; (print "iloop: eof term-char\n")
                                       (values (handle-fragment fragment str-handler seed) term-char))
                                     (case term-char
                                       ((#\<)
                                        (let ((token (ssax:read-markup-token)))
                                          (case (xml-token-kind token)
                                            ((CDSECT)
                                             (loop
                                              (ssax:read-cdata-body
                                               str-handler
                                               (handle-fragment fragment str-handler seed))))
                                            ((COMMENT)
                                             (loop (handle-fragment fragment str-handler seed)))
                                            (else
                                             ;; (print "iloop: else 2.\n")
                                             (values
                                              (handle-fragment fragment str-handler seed)
                                              token)))))
                                       ((#\&)
                                        (case (xml-string->sxml-peek-next-char)
                                          ((#\#)
                                           (xml-string->sxml-read-char)
                                           (loop
                                            (str-handler
                                             fragment
                                             (string (ssax:xml-string->sxml-read-char-ref))
                                             seed)))
                                          (else
                                           (let ((name (ssax:read-NCName)))
                                             (assert-curr-char '(#\;) "XML [68]")
                                             ;; (print "iloop: XML 68 case.\n")
                                             (values
                                              (handle-fragment fragment str-handler seed)
                                              (make-xml-token 'ENTITY-REF name))))))
                                       (else
                                        (if (eqv? (xml-string->sxml-peek-next-char) #\newline) (xml-string->sxml-read-char))
                                        (loop
                                         (str-handler fragment (string #\newline) seed))))))))))))
              ;; (print "ssax:xml-string->sxml-read-char-data: returns ") (write (##vector->list r)) (print "\n")
              r))))
      (define (ssax:assert-token token kind gi error-cont)
        ;; (print "ssax:assert-token invoked for: token=") (write token) (print " kind=") (write kind)
        ;; (print " gi=") (write gi) (print " error-cont=") (write error-cont) (print "\n")
        (or (and (xml-token? token)
                 (eq? kind (xml-token-kind token))
                 (equal? gi (xml-token-head token)))
            (error-cont token kind gi)))
      (define (ssax:reverse-collect-str fragments)
        ;; (print "ssax:reverse-collect-str: invoked with fragments=") (write fragments) (print "\n")
        (cond
         ((null? fragments) '())
         ((null? (cdr fragments)) fragments)
         (else
          (let loop ((fragments fragments) (result '()) (strs '()))
            ;; (print "ssax:reverse-collect-str: iterates with fragments = ") (write fragments)
            ;; (print " result=") (write result)
            ;; (print " strs=") (write strs) (print "\n\n")
            (cond
             ((null? fragments)
              (if (null? strs)
                  result
                  (cons (string-concatenate/shared strs) result)))
             ((string? (car fragments))
              (loop (cdr fragments) result (cons (car fragments) strs)))
             (else
              (loop
               (cdr fragments)
               (cons
                (car fragments)
                (if (null? strs)
                    result
                    (cons (string-concatenate/shared strs) result)))
               '())))))))
      ;; Possible optimization: Split out the below to two different lambdas depending
      ;; on if preserve-whitespace-only-node-values is set
      (define (ssax:reverse-collect-str-drop-ws fragments)
        (cond
         ((null? fragments) '())
         ((null? (cdr fragments))
          (if (and (string? (car fragments)) (string-whitespace? (car fragments)) not-preserve-whitespace-only-node-values)
              '()
              fragments))
         (else
          (let loop ((fragments fragments)
                     (result '())
                     (strs '())
                     (all-whitespace? #t))
            ;; (print "ssax:reverse-collect-str: iterates with fragments = ") (write fragments)
            ;; (print " result=") (write result)
            ;; (print " strs=") (write strs) (print " all-whitespace?=") (write all-whitespace?) (print "\n\n")
            (cond
             ((null? fragments)
              (if all-whitespace?
                  result
                  (cons (string-concatenate/shared strs) result)))
             ((string? (car fragments))
              (loop
               (cdr fragments)
               result
               (cons (car fragments) strs)
               (and all-whitespace? not-preserve-whitespace-only-node-values (string-whitespace? (car fragments)))))
             (else
              (loop
               (cdr fragments)
               (cons
                (car fragments)
                (if all-whitespace?
                    result
                    (cons (string-concatenate/shared strs) result)))
               '()
               #t)))))))
      (letrec ((namespaces
                `(,@(map
                     (lambda (el)
                       (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
                     namespace-prefix-assig)
                  . ,namespaces-already-defined))
               (RES-NAME->SXML
                (lambda (res-name)
                  (string->symbol
                   (string-append
                    (symbol->string (car res-name))
                    ":"
                    (symbol->string (cdr res-name)))))))
        ;; procedure: ssax:xml->sxml NAMESPACE-PREFIX-ASSIG
        ;;
        ;; This is an instance of a SSAX parser that returns an SXML
        ;; representation of the XML document to be read from PORT.
        ;; NAMESPACE-PREFIX-ASSIG is a list of (USER-PREFIX . URI-STRING)
        ;; that assigns USER-PREFIXes to certain namespaces identified by
        ;; particular URI-STRINGs. It may be an empty list.
        ;; The procedure returns an SXML tree. Thepoints out to the
        ;; first character after the root element.
        (define (ssax:xml->sxml namespace-prefix-assig)
          ;; The following code is used by lazy:xml->sxml and parse-into-ssax-event-stream also,
          ;; therefore we split it out to a block above.
          ;;
          ;; (letrec ((namespaces
          ;;           (map
          ;;            (lambda (el)
          ;;              (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
          ;;            namespace-prefix-assig))
          ;;          (RES-NAME->SXML
          ;;           (lambda (res-name)
          ;;             (string->symbol
          ;;              (string-append
          ;;               (symbol->string (car res-name))
          ;;               ":"
          ;;               (symbol->string (cdr res-name)))))))
          (let ((result
                 (reverse
                  ((ssax:make-parser
                    NEW-LEVEL-SEED
                    (lambda (elem-gi attributes namespaces expected-content seed)
                      '())
                    FINISH-ELEMENT
                    (lambda (elem-gi attributes namespaces parent-seed seed)
                      ;; (print "ssax:xml->sxml FINISH-ELEMENT invoked with: elem-gi=") (write elem-gi)
                      ;; (print " attributes=") (write attributes) (print " namespaces=")
                      ;; (write namespaces) (print " parent-seed=") (write parent-seed)
                      ;; (print " seed=") (write seed) (print "\n")
                      (let ((seed (ssax:reverse-collect-str-drop-ws seed))
                            (attrs
                             (attlist-fold
                              (lambda (attr accum)
                                (cons
                                 (list
                                  (if (symbol? (car attr))
                                      (car attr)
                                      (RES-NAME->SXML (car attr)))
                                  (cdr attr))
                                 accum))
                              '()
                              attributes)))
                        (cons
                         (cons
                          (if (symbol? elem-gi) elem-gi (RES-NAME->SXML elem-gi))
                          (if (null? attrs) seed (cons (cons '@ attrs) seed)))
                         parent-seed)))
                    CHAR-DATA-HANDLER
                    (lambda (string1 string2 seed)
                      (if (string-null? string2)
                          (cons string1 seed)
                          (cons* string2 string1 seed)))
                    DOCTYPE
                    (lambda (docname systemid internal-subset? seed)
                      (when internal-subset?
                            (ssax:warn
                             "Internal DTD subset is not currently handled ")
                            (ssax:skip-internal-dtd))
                      (ssax:warn
                       "DOCTYPE DECL "
                       docname
                       " "
                       systemid
                       " found and skipped")
                      (values #f '() namespaces seed))
                    UNDECL-ROOT
                    (lambda (elem-gi seed) (values #f '() namespaces seed))
                    PI
                    ((*DEFAULT*
                      lambda
                      (pi-tag seed)
                      (cons
                       (list '*PI* pi-tag (ssax:read-pi-body-as-string))
                       seed))))
                   '()))))
            (cons
             '*TOP*
             (if (null? namespace-prefix-assig)
                 result
                 (cons
                  (list
                   '@@
                   (cons
                    '*NAMESPACES*
                    (map
                     (lambda (ns) (list (car ns) (cdr ns)))
                     namespace-prefix-assig)))
                  result)))))
        ;; ## lazy:xml->sxml session begin
        ;; A specialized lazy XML->SXML parser
        ;; Is heavily based on continuations
        ;; Preliminary helper functions
        (define lazy:promise? ##promise?) ; In Gambit.
        ;; A helper that forces all descendants of a given node or a nodeset
        (define (lazy:force-descendants node)
          (cond
           ((lazy:promise? node)        ; force it
            (lazy:force-descendants (force node)))
           ((pair? node)                ; not null
            (for-each lazy:force-descendants node))
           (else              ; null or not pair -> nothing to be done
            #t)))
        ;; Returns the list containing of all members of the argument list except
        ;; for the last member
        (define (lazy:except-last lst)
          (if
           (or (null? lst)              ; this shouldn't happen
               (null? (cdr lst)))
           '()
           (cons (car lst) (lazy:except-last (cdr lst)))))
        ;; Returns the common part of the seed
        (define (lazy:seed-common seed)
          ((if (null? (cdr seed))       ; a short seed
               car caddr)
           seed))
        ;; A monad-like handler
        ;; Replaces the common part of the seed
        (define (lazy:replace-common seed new-common)
          (if (null? (cdr seed))        ; a short seed
              (list new-common)
              (list (car seed)
                    (cadr seed)
                    new-common
                    (cadddr seed))))
        ;; Produces a lazy SXML document, which corresponds to reading a source
        ;; document in a stream-wise fashion
        (define (lazy:xml->sxml namespace-prefix-assig)
          ;; The following code is used by ssax:xml->sxml and parse-into-ssax-event-stream also,
          ;; therefore we split it out to a block above.
          ;; (let ((namespaces
          ;;        (map (lambda (el)
          ;;               (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
          ;;             namespace-prefix-assig))
          ;;       (RES-NAME->SXML
          ;;        (lambda (res-name)
          ;;          (string->symbol
          ;;           (string-append
          ;;            (symbol->string (car res-name))
          ;;            ":"
          ;;            (symbol->string (cdr res-name)))))))
          ((lambda (result)
             ;; We assume that nobody follows the document element
             (if (null? namespace-prefix-assig)
                 (cons '*TOP* (lazy:except-last result))
                 (cons
                  '*TOP*
                  (cons
                   `(@@ (*NAMESPACES*
                         ,@(map
                            (lambda (ns) (list (car ns) (cdr ns)))
                            namespace-prefix-assig)))
                   (lazy:except-last result)))))
           (call-with-current-continuation ; we grab the continuation to escape from parsing
            (lambda (result-k)
              ;; seed ::= (list result-k state-k common-seed level)
              ;; result-k - continuation on what to do with the current result portion
              ;; state-k - continuation to return to SSAX state on this level of XML
              ;;  tree hierarchy
              ;; common-seed - general seed information
              ;; level - level of a current node in a tree hierarchy
              ((ssax:make-parser
                NEW-LEVEL-SEED
                (lambda (elem-gi attributes namespaces expected-content seed)
                                        ;(pp (cons elem-gi (cadddr seed)))
                  (if
                   (or (null? (cdr seed))   ; short seed
                       (> (cadddr seed) 3)) ; deep level
                   (list '())   ; work like a conventional SSAX parser
                   (let ((attrs
                          (attlist-fold
                           (lambda (attr accum)
                             (cons (list
                                    (if (symbol? (car attr)) (car attr)
                                        (RES-NAME->SXML (car attr)))
                                    (cdr attr)) accum))
                           '() attributes)))
                     (call-with-current-continuation
                      (lambda (new-level-k) ; how to parse next
                        ((car seed)         ; return the result
                         (let ((elem-content
                                        ; A promise to continue parsing
                                (call-with-current-continuation ; where to put the result
                                 (lambda (elem-k)
                                   (new-level-k
                                    (list   ; now form a seed
                                     elem-k ; what to do with result
                                     new-level-k ; SSAX state on this level
                                     '() ; common-seed is empty
                                     (+ (cadddr seed) 1) ; increase level
                                     ))))))
                           (append
                            ;; Previous string content
                            (ssax:reverse-collect-str-drop-ws (caddr seed))
                            (list
                             (cons
                              (if (symbol? elem-gi) elem-gi
                                  (RES-NAME->SXML elem-gi))
                              (if (null? attrs) elem-content
                                  (cons (cons '@ attrs) elem-content)))
                             ;; The following siblings of this element
                             (delay
                               (call-with-current-continuation ; where to put the result
                                (lambda (foll-k)
                                        ; First we force the parsing of the current element
                                  (lazy:force-descendants elem-content)
                                        ; Than continue parsing
                                  ((cadr seed) ; recover the parent level of nesting
                                   (list
                                    foll-k ; what to do with result
                                    (cadr seed)
                                    '() ; common-seed is empty
                                    (cadddr seed) ; the same level for siblings
                                    ))))))))))))))
                FINISH-ELEMENT
                (lambda (elem-gi attributes namespaces parent-seed seed)
                  (if
                   (null? (cdr seed))   ; a short seed
                   (let ((common (ssax:reverse-collect-str-drop-ws
                                  (lazy:seed-common seed)))
                         (attrs
                          (attlist-fold
                           (lambda (attr accum)
                             (cons (list
                                    (if (symbol? (car attr)) (car attr)
                                        (RES-NAME->SXML (car attr)))
                                    (cdr attr)) accum))
                           '() attributes)))
                     (lazy:replace-common
                      parent-seed
                      (cons
                       (cons
                        (if (symbol? elem-gi) elem-gi
                            (RES-NAME->SXML elem-gi))
                        (if (null? attrs) common
                            (cons (cons '@ attrs) common)))
                       (lazy:seed-common parent-seed))))
                                        ; Otherwise - just return the remaining character content
                   ((car seed)          ; continuation
                    (ssax:reverse-collect-str-drop-ws
                     (lazy:seed-common seed)))))
                CHAR-DATA-HANDLER
                (lambda (string1 string2 seed)
                                        ;(pp (list string1 string2 seed))
                  (lazy:replace-common
                   seed
                   (if (string-null? string2)
                       (cons string1 (lazy:seed-common seed))
                       (cons* string2 string1 (lazy:seed-common seed)))))
                DOCTYPE
                (lambda (docname systemid internal-subset? seed)
                  (when internal-subset?
                        (ssax:warn
                         "Internal DTD subset is not currently handled ")
                        (ssax:skip-internal-dtd))
                  (ssax:warn "DOCTYPE DECL " docname " "
                             systemid " found and skipped")
                  (values #f '() namespaces seed))
                UNDECL-ROOT
                (lambda (elem-gi seed)
                  (values #f '() namespaces seed))
                PI
                ((*DEFAULT* .
                            (lambda (pi-tag seed)
                              (lazy:replace-common
                               seed
                               (cons
                                (list '*PI* pi-tag (ssax:read-pi-body-as-string))
                                (lazy:seed-common seed)))))))
               (list                    ; form initial seed
                result-k                ; put the result
                (lambda (seed) ; dummy top-level parser state that produces '()
                  ((car seed)  ; where to put the result nodeset
                   '()))
                '()
                ;; level for the document element4
                1))))))
        ;; ## / lazy:xml->sxml session begin
        (define (parse-into-ssax-event-stream namespace-prefix-assig)
          (letrec ((parse-next-thunk
                    (lambda ()
                      (call/cc
                       (lambda (return-to-caller)
                         (define (produce-event . a)
                           (call/cc
                            (lambda (continue-processing)
                              (set! parse-next-thunk continue-processing)
                              (apply return-to-caller `(,current-xml-string-pos
                                                        ,(fx+ current-xml-string-pos-offset current-xml-string-pos)
                                                        .,a)))))
                         ((ssax:make-parser
                           ;; ** NOTE: In these handlers, what we return back to ssax:make-parser is a bit arbitrary currently.
                           ;; **       Basically it's like this only to make it not throw any exceptions.
                           NEW-LEVEL-SEED
                           (lambda (elem-gi attributes namespaces expected-content seed)
                             ;; (dbg current-xml-string-pos " NEW-LEVEL-SEED: " elem-gi " " attributes " " namespaces " " expected-content " " seed)
                             ;; '()
                             (produce-event 'NEW-LEVEL-SEED elem-gi attributes namespaces expected-content seed)
                             '())
                           FINISH-ELEMENT
                           (lambda (elem-gi attributes namespaces parent-seed seed)
                             ;; (dbg current-xml-string-pos " FINISH-ELEMENT " elem-gi " " attributes " " namespaces " " parent-seed " " seed)
                             ;; (print "ssax:xml->sxml FINISH-ELEMENT invoked with: elem-gi=") (write elem-gi)
                             ;; (print " attributes=") (write attributes) (print " namespaces=")
                             ;; (write namespaces) (print " parent-seed=") (write parent-seed)
                             ;; (print " seed=") (write seed) (print "\n")
                             (let* ((seed (ssax:reverse-collect-str-drop-ws seed))
                                    (attrs
                                     (attlist-fold
                                      (lambda (attr accum)
                                        (cons
                                         (list
                                          (if (symbol? (car attr))
                                              (car attr)
                                              (RES-NAME->SXML (car attr)))
                                          (cdr attr))
                                         accum))
                                      '()
                                      attributes))
                                    (constructed-data
                                     (cons
                                      (if (symbol? elem-gi) elem-gi (RES-NAME->SXML elem-gi))
                                      (if (null? attrs) seed (cons (cons '@ attrs) seed)))))
                               (produce-event 'FINISH-ELEMENT elem-gi attributes namespaces parent-seed seed constructed-data)
                               '()))
                           CHAR-DATA-HANDLER
                           (lambda (string1 string2 seed)
                                        ; (dbg current-xml-string-pos " CHAR-DATA-HANDLER " string1 " " string2 " " seed)
                             (let ((constructed-data (if (string-null? string2)
                                                         (cons string1 seed)
                                                         (cons* string2 string1 seed))))
                               (produce-event 'CHAR-DATA-HANDLER string1 string2 seed constructed-data)
                               '()))
                           DOCTYPE
                           (lambda (docname systemid internal-subset? seed)
                             ;; (dbg current-xml-string-pos " DOCTYPE " docname " " systemid " " internal-subset? " " seed)
                             ;; (when internal-subset?
                             ;;   (ssax:warn
                             ;;    "Internal DTD subset is not currently handled ")
                             ;;   (ssax:skip-internal-dtd))
                             ;; (ssax:warn
                             ;;  "DOCTYPE DECL "
                             ;;  docname
                             ;;  " "
                             ;;  systemid
                             ;;  " found and skipped")
                             ;; (values #f '() namespaces seed)
                             (produce-event 'DOCTYPE docname systemid internal-subset? seed)
                             (values #f '() namespaces seed))
                           UNDECL-ROOT
                           (lambda (elem-gi seed)
                             ;; (dbg current-xml-string-pos " UNDECL-ROOT " elem-gi " " seed)
                             ;; (values #f '() namespaces seed)
                             (produce-event 'UNDECL-ROOT elem-gi seed)
                             (values #f '() namespaces seed))
                           PI
                           ((*DEFAULT*
                             lambda
                             (pi-tag seed)
                                        ; (dbg current-xml-string-pos " *DEFAULT* " pi-tag " " seed)
                             (let ((constructed-data
                                        ; (cons
                                    (list '*PI* pi-tag (ssax:read-pi-body-as-string))
                                        ; seed)
                                    ))
                               (produce-event 'PI constructed-data)
                               (cons constructed-data seed)))))
                          '())
                         (set! parse-next-thunk (lambda () #f)) ; Return #f on any subsequent call.
                         #f)))))
            ;; We return a procedure that in turn invokes parse-next-thunk , as parse-next-thunk changes
            ;; between every invocation, while we want the caller to just re-invoke one and the same
            ;; procedure reference all over.
            ;; 2012-10-25: Replaced (or parse-next-thunk parse-process-start) with parse-next-thunk
            ;;             as parse-process-start is not used nor declared anymore.
            (lambda () (parse-next-thunk))))
        (set! ssax:complete-start-tag
              (let ((xmlns (string->symbol "xmlns"))
                    (largest-dummy-decl-attr (list ssax:largest-unres-name #f #f #f)))
                (define (validate-attrs attlist decl-attrs)
                  (define (add-default-decl decl-attr result)
                    (call-with-values
                        (lambda () (apply values decl-attr))
                      (lambda (attr-name content-type use-type default-value)
                        (and (eq? use-type 'REQUIRED)
                             (parser-error"[RequiredAttr] broken for" attr-name))
                        (if default-value
                            (cons (cons attr-name default-value) result)
                            result))))
                  (let loop ((attlist attlist) (decl-attrs decl-attrs) (result '()))
                    (if (attlist-null? attlist)
                        (attlist-fold add-default-decl result decl-attrs)
                        (call-with-values
                            (lambda () (attlist-remove-top attlist))
                          (lambda (attr attr-others)
                            (call-with-values
                                (lambda ()
                                  (if (attlist-null? decl-attrs)
                                      (values largest-dummy-decl-attr decl-attrs)
                                      (attlist-remove-top decl-attrs)))
                              (lambda (decl-attr other-decls)
                                (case (name-compare (car attr) (car decl-attr))
                                  ((<)
                                   (if (or (eq? xmlns (car attr))
                                           (and (pair? (car attr)) (eq? xmlns (caar attr))))
                                       (loop attr-others decl-attrs (cons attr result))
                                       (parser-error"[ValueType] broken for " attr)))
                                  ((>)
                                   (loop attlist other-decls (add-default-decl decl-attr result)))
                                  (else
                                   (call-with-values
                                       (lambda () (apply values decl-attr))
                                     (lambda (attr-name content-type use-type default-value)
                                       (cond
                                        ((eq? use-type 'FIXED)
                                         (or (equal? (cdr attr) default-value)
                                             (parser-error
                                              "[FixedAttr] broken for "
                                              attr-name)))
                                        ((eq? content-type 'CDATA) #t)
                                        ((pair? content-type)
                                         (or (member (cdr attr) content-type)
                                             (parser-error
                                              "[enum] broken for "
                                              attr-name
                                              "="
                                              (cdr attr))))
                                        (else
                                         (ssax:warn
                                          "declared content type "
                                          content-type
                                          " not verified yet")))
                                       (loop attr-others other-decls
                                             (cons attr result)))))))))))))
                (define (add-ns prefix uri-str namespaces)
                  (and (equal? "" uri-str)
                       (parser-error "[dt-NSName] broken for " prefix))
                  (let ((uri-symbol (ssax:uri-string->symbol uri-str)))
                    (let loop ((nss namespaces))
                      (cond
                       ((null? nss) (cons (cons* prefix uri-symbol uri-symbol) namespaces))
                       ((eq? uri-symbol (cddar nss))
                        (cons (cons* prefix (cadar nss) uri-symbol) namespaces))
                       (else (loop (cdr nss)))))))
                (define (adjust-namespace-decl attrs namespaces)
                  (let loop ((attrs attrs) (proper-attrs '()) (namespaces namespaces))
                    (cond
                     ((null? attrs) (values proper-attrs namespaces))
                     ((eq? xmlns (caar attrs))
                      (loop
                       (cdr attrs)
                       proper-attrs
                       (if (equal? "" (cdar attrs))
                           (cons (cons* '*DEFAULT* #f #f) namespaces)
                           (add-ns '*DEFAULT* (cdar attrs) namespaces))))
                     ((and (pair? (caar attrs)) (eq? xmlns (caaar attrs)))
                      (loop
                       (cdr attrs)
                       proper-attrs
                       (add-ns(cdaar attrs) (cdar attrs) namespaces)))
                     (else
                      (loop (cdr attrs) (cons (car attrs) proper-attrs) namespaces)))))
                (lambda (tag-head elems entities namespaces)
                  (let* ((attlist (ssax:read-attributes entities))
                         (empty-el-tag?
                          (begin
                            (ssax:skip-S)
                            (and (eqv?
                                  #\/
                                  (assert-curr-char
                                   '(#\> #\/)
                                   "XML [40], XML [44], no '>'"
                                   ))
                                 (assert-curr-char
                                  '(#\>)
                                  "XML [44], no '>'"
                                  )))))
                    (call-with-values
                        (lambda () (if elems
                                       (cond
                                        ((assoc tag-head elems)
                                         =>
                                         (lambda (decl-elem)
                                           (values
                                            (if empty-el-tag? 'EMPTY-TAG (cadr decl-elem))
                                            (caddr decl-elem))))
                                        (else
                                         (parser-error
                                          "[elementvalid] broken, no decl for "
                                          tag-head)))
                                       (values (if empty-el-tag? 'EMPTY-TAG 'ANY) #f)))
                      (lambda (elem-content decl-attrs)
                        (let ((merged-attrs
                               (if decl-attrs
                                   (validate-attrs attlist decl-attrs)
                                   (attlist->alist attlist))))
                          (call-with-values
                              (lambda ()
                                (adjust-namespace-decl merged-attrs namespaces))
                            (lambda (proper-attrs namespaces)
                              (values
                               (ssax:resolve-name tag-head namespaces #t)
                               (fold-right
                                (lambda (name-value attlist)
                                  (or (attlist-add
                                       attlist
                                       (cons
                                        (ssax:resolve-name (car name-value)
                                                           namespaces #f)
                                        (cdr name-value)))
                                      (parser-error
                                       "[uniqattspec] after NS expansion broken for "
                                       name-value)))
                                (make-empty-attlist)
                                proper-attrs)
                               namespaces
                               elem-content))))))))))
        (io-dbg-checkpoint)
        (case operation
          ((parse)
           (let ((r (ssax:xml->sxml namespace-prefix-assig)))
             (xml-string->sxml-read-to-idx (if reached-eof
                                               #f ; xml-string-length
                                               current-xml-string-pos))
             (xml-string->sxml-read-to-idx/all-data (if reached-eof
                                                        #f ; xml-string-length
                                                        (fx+ current-xml-string-pos-offset current-xml-string-pos)))
             r))
          ((lazy)
           (lazy:xml->sxml namespace-prefix-assig))
          ((ssax)
           (parse-into-ssax-event-stream namespace-prefix-assig))
          (else (error "Unknown operation" operation)))))))
