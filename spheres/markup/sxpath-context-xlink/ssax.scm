;;-------------------------------------------------------------------------------
;; Macros

(define-macro xlink:xml-token-kind (lambda (token) `(car ,token)))

(define-macro xlink:xml-token-head (lambda (token) `(cdr ,token)))

(define-macro xlink:make-pi-parser
  (lambda (my-pi-handlers)
    `(lambda (port target seed)
       (case target
         (unquote-splicing
          (let loop ((pi-handlers my-pi-handlers) (default #f))
            (cond
             ((null? pi-handlers)
              (if default
                  `((else (,default port target seed)))
                  '((else
                     (xlink:warn port "Skipping PI: " target nl)
                     (xlink:skip-pi port)
                     seed))))
             ((eq? '*DEFAULT* (caar pi-handlers))
              (loop (cdr pi-handlers) (cdar pi-handlers)))
             (else
              (cons
               `((,(caar pi-handlers)) (,(cdar pi-handlers) port target seed))
               (loop (cdr pi-handlers) default))))))))))

(define-macro xlink:make-elem-parser
  (lambda (my-new-level-seed
      my-finish-element
      my-char-data-handler
      my-pi-handlers)
    `(lambda (start-tag-head port elems entities namespaces preserve-ws? seed)
       (define xml-space-gi (cons xlink:Prefix-XML (string->symbol "space")))
       (let handle-start-tag ((start-tag-head start-tag-head)
                              (port port)
                              (entities entities)
                              (namespaces namespaces)
                              (preserve-ws? preserve-ws?)
                              (parent-seed seed))
         (call-with-values
             (lambda () (xlink:complete-start-tag
                    start-tag-head
                    port
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
                  (xlink:assert-token
                   (and (eqv? #\< (xlink:skip-S port))
                        (xlink:read-markup-token port))
                   'END
                   start-tag-head
                   (lambda (token exp-kind exp-head)
                     (xlink:parser-error
                      port
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
                    (let loop ((port port)
                               (entities entities)
                               (expect-eof? #f)
                               (seed seed))
                      (call-with-values
                          (lambda () (xlink:read-char-data
                                 port
                                 expect-eof?
                                 ,my-char-data-handler
                                 seed))
                        (lambda (seed term-token)
                          (if (eof-object? term-token)
                              seed
                              (case (xlink:xml-token-kind term-token)
                                ((END)
                                 (xlink:assert-token
                                  term-token
                                  'END
                                  start-tag-head
                                  (lambda (token exp-kind exp-head)
                                    (xlink:parser-error
                                     port
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
                                        ((xlink:make-pi-parser ,my-pi-handlers)
                                         port
                                         (xlink:xml-token-head term-token)
                                         seed)))
                                   (loop port entities expect-eof? seed)))
                                ((ENTITY-REF)
                                 (let ((seed
                                        (xlink:handle-parsed-entity
                                         port
                                         (xlink:xml-token-head term-token)
                                         entities
                                         (lambda (port entities seed)
                                           (loop port entities #t seed))
                                         ,my-char-data-handler
                                         seed)))
                                   (loop port entities expect-eof? seed)))
                                ((START)
                                 (if (eq? expected-content 'PCDATA)
                                     (xlink:parser-error
                                      port
                                      "[elementvalid] broken for "
                                      elem-gi
                                      " with char content only; unexpected token "
                                      term-token))
                                 (let ((seed
                                        (handle-start-tag
                                         (xlink:xml-token-head term-token)
                                         port
                                         entities
                                         namespaces
                                         preserve-ws?
                                         seed)))
                                   (loop port entities expect-eof? seed)))
                                (else
                                 (xlink:parser-error
                                  port
                                  "XML [43] broken for "
                                  term-token)))))))))))))))))

(define-macro xlink:make-parser
  (lambda user-handlers
    (define all-handlers
      '((DOCTYPE
         lambda
         (port docname systemid internal-subset? seed)
         (when internal-subset?
               (xlink:warn port "Internal DTD subset is not currently handled ")
               (xlink:skip-internal-dtd port))
         (xlink:warn
          port
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
        (error "Odd number of arguments to xlink:make-parser"))
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
      `(lambda (port seed)
         (define (handle-decl port token-head seed)
           (or (eq? (string->symbol "DOCTYPE") token-head)
               (xlink:parser-error
                port
                "XML [22], expected DOCTYPE declaration, found "
                token-head))
           (xlink:assert-curr-char xlink:S-chars "XML [28], space after DOCTYPE" port)
           (xlink:skip-S port)
           (let* ((docname (xlink:read-QName port))
                  (systemid
                   (and (xlink:ncname-starting-char? (xlink:skip-S port))
                        (xlink:read-external-id port)))
                  (internal-subset?
                   (begin
                     (xlink:skip-S port)
                     (eqv?
                      #\[
                      (xlink:assert-curr-char
                       '(#\> #\[)
                       "XML [28], end-of-DOCTYPE"
                       port)))))
             (call-with-values
                 (lambda () (,(get-handler 'DOCTYPE)
                             port
                             docname
                             systemid
                             internal-subset?
                             seed))
               (lambda (elems entities namespaces seed)
                 (scan-for-significant-prolog-token-2
                  port
                  elems
                  entities
                  namespaces
                  seed)))))
         (define (scan-for-significant-prolog-token-1 port seed)
           (let ((token (xlink:scan-Misc port)))
             (if (eof-object? token)
                 (xlink:parser-error port "XML [22], unexpected EOF")
                 (case (xlink:xml-token-kind token)
                   ((PI)
                    (let ((seed
                           ((xlink:make-pi-parser ,(get-handler 'PI))
                            port
                            (xlink:xml-token-head token)
                            seed)))
                      (scan-for-significant-prolog-token-1 port seed)))
                   ((DECL) (handle-decl port (xlink:xml-token-head token) seed))
                   ((START)
                    (call-with-values
                        (lambda () (,(get-handler 'UNDECL-ROOT)
                                    (xlink:xml-token-head token)
                                    seed))
                      (lambda (elems entities namespaces seed)
                        (element-parser
                         (xlink:xml-token-head token)
                         port
                         elems
                         entities
                         namespaces
                         #f
                         seed))))
                   (else
                    (xlink:parser-error port "XML [22], unexpected markup " token))))))
         (define (scan-for-significant-prolog-token-2
                  port
                  elems
                  entities
                  namespaces
                  seed)
           (let ((token (xlink:scan-Misc port)))
             (if (eof-object? token)
                 (xlink:parser-error port "XML [22], unexpected EOF")
                 (case (xlink:xml-token-kind token)
                   ((PI)
                    (let ((seed
                           ((xlink:make-pi-parser ,(get-handler 'PI))
                            port
                            (xlink:xml-token-head token)
                            seed)))
                      (scan-for-significant-prolog-token-2
                       port
                       elems
                       entities
                       namespaces
                       seed)))
                   ((START)
                    (element-parser
                     (xlink:xml-token-head token)
                     port
                     elems
                     entities
                     namespaces
                     #f
                     (,(get-handler 'DECL-ROOT) (xlink:xml-token-head token) seed)))
                   (else
                    (xlink:parser-error port "XML [22], unexpected markup " token))))))
         (define element-parser
           (xlink:make-elem-parser
            ,(get-handler 'NEW-LEVEL-SEED)
            ,(get-handler 'FINISH-ELEMENT)
            ,(get-handler 'CHAR-DATA-HANDLER)
            ,(get-handler 'PI)))
         (scan-for-significant-prolog-token-1 port seed)))))


;;-------------------------------------------------------------------------------
;; Procedures

(cond-expand
 (gambit (declare (fixnum)))
 (else (void)))


(define (xlink:filter proc lst)
  (let recur ((lst lst))
    (if (null? lst)
        '()
        (let ((head (car lst)))
          (if (proc head)
              (cons head (recur (cdr lst)))
              (recur (cdr lst)))))))

(define xlink:warn
  (lambda args
    (define-macro (port-name port)
      `(##vector-ref ,port 4))
    (if
     (port? (car args))
     (string-append "Warning: " (port-name (car args))
                    " at position " (input-port-byte-position (car args)) "\n"
                    (cdr args) "\n")
     #f)))

(define (xlink:string-split str . rest)
  ;; maxsplit is a positive number
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
       ((>= i (string-length str)) '())
       ((char-whitespace? (string-ref str i))
        (skip-ws (++ i) yet-to-split-count))
       (else (scan-beg-word (++ i) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
       ((zero? yet-to-split-count)
        (cons (substring str from (string-length str)) '()))
       (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
       ((>= i (string-length str))
        (cons (substring str from i) '()))
       ((char-whitespace? (string-ref str i))
        (cons (substring str from i)
              (skip-ws (++ i) (- yet-to-split-count 1))))
       (else (scan-word (++ i) from yet-to-split-count))))
    (skip-ws 0 (- maxsplit 1)))
  ;; maxsplit is a positive number
  ;; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
       ((>= from (string-length str)) '(""))
       ((zero? yet-to-split-count)
        (cons (substring str from (string-length str)) '()))
       (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
       ((>= i (string-length str))
        (cons (substring str from i) '()))
       ((memq (string-ref str i) delimeters)
        (cons (substring str from i)
              (scan-beg-word (++ i) (- yet-to-split-count 1))))
       (else (scan-word (++ i) from yet-to-split-count))))
    (scan-beg-word 0 (- maxsplit 1)))
  ;; resolver of overloading...
  ;; if omitted, maxsplit defaults to
  ;; (++ (string-length str))
  (if (string-null? str) '()
      (if (null? rest)
          (split-by-whitespace str (++ (string-length str)))
          (let ((charset (car rest))
                (maxsplit
                 (if (pair? (cdr rest)) (cadr rest) (++ (string-length str)))))
            (cond
             ((not (positive? maxsplit)) '())
             ((null? charset) (split-by-whitespace str maxsplit))
             (else (split-by-charset str charset maxsplit)))))))

(define* (xlink:next-token prefix-skipped-chars break-chars
                           (comment "")
                           (port (current-input-port)))
  (define input-parse:init-buffer
    (let ((buffer (make-string 512)))
      (lambda () buffer)))
  (define (skip-while skip-chars port)
    (do ((c (peek-char port) (peek-char port)))
        ((not (memv c skip-chars)) c)
      (read-char port)))
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer)) (quantum 16))
    (let loop ((i 0) (c (skip-while prefix-skipped-chars port)))
      (cond
       ((memq c break-chars) (substring buffer 0 i))
       ((eof-object? c)
        (if (memq '*eof* break-chars)
    	    (substring buffer 0 i)      ; was EOF expected?
    	    (xlink:parser-error port "EOF while reading a token " comment)))
       (else
        (if (>= i curr-buf-len)   ; make space for i-th char in buffer
    	    (begin                ; -> grow the buffer by the quantum
    	      (set! buffer (string-append buffer (make-string quantum)))
    	      (set! quantum curr-buf-len)
    	      (set! curr-buf-len (string-length buffer))))
        (string-set! buffer i c)
        (read-char port)                ; move to the next char
        (loop (++ i) (peek-char port)))))))

(define* (xlink:peek-next-char (port (current-input-port)))
  (read-char port)
  (peek-char port))

(define* (xlink:next-token-of incl-list/pred
                              (port (current-input-port)) )
  (define input-parse:init-buffer
    (let ((buffer (make-string 512)))
      (lambda () buffer)))
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer)) (quantum 16))
    (if (procedure? incl-list/pred)
        (let loop ((i 0) (c (peek-char port)))
          (cond
           ((incl-list/pred c) =>
            (lambda (c)
              (if (>= i curr-buf-len) ; make space for i-th char in buffer
                  (begin           ; -> grow the buffer by the quantum
                    (set! buffer (string-append buffer (make-string quantum)))
                    (set! quantum curr-buf-len)
                    (set! curr-buf-len (string-length buffer))))
              (string-set! buffer i c)
              (read-char port)          ; move to the next char
              (loop (++ i) (peek-char port))))
           (else (substring buffer 0 i))))
                                        ; incl-list/pred is a list of allowed characters
        (let loop ((i 0) (c (peek-char port)))
          (cond
           ((not (memq c incl-list/pred)) (substring buffer 0 i))
           (else
            (if (>= i curr-buf-len) ; make space for i-th char in buffer
                (begin             ; -> grow the buffer by the quantum
                  (set! buffer (string-append buffer (make-string quantum)))
                  (set! quantum curr-buf-len)
                  (set! curr-buf-len (string-length buffer))))
            (string-set! buffer i c)
            (read-char port)            ; move to the next char
            (loop (++ i) (peek-char port))))))))

(define (xlink:find-string-from-port? str <input-port> . max-no-char)
  (set! max-no-char (if (null? max-no-char) #f (car max-no-char)))
  (letrec
      ((no-chars-read 0)
       (my-peek-char			; Return a peeked char or #f
	(lambda () (and (or (not max-no-char) (< no-chars-read max-no-char))
                   (let ((c (peek-char <input-port>)))
                     (if (eof-object? c) #f c)))))
       (next-char (lambda () (read-char <input-port>)
                     (set! no-chars-read  (++ no-chars-read))))
       (match-1st-char			; of the string str
	(lambda ()
	  (let ((c (my-peek-char)))
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
	      no-chars-read            ; the entire string has matched
	      (let ((c (my-peek-char)))
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

(define* (xlink:assert-curr-char expected-chars comment (port (current-input-port)))
  (let ((c (read-char port)))
    (if (memq c expected-chars) c
        (xlink:parser-error port "Wrong character " c
                            " (0x" (if (eof-object? c) "*eof*"
                                       (number->string (char->integer c) 16)) ") "
                            comment ". " expected-chars " expected"))))

(define (xlink:make-xml-token kind head) (cons kind head))

(define xlink:xml-token? pair?)

(define (xlink:string-whitespace? str)
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

(define xlink:S-chars (map integer->char '(32 10 9 13)))

(define (xlink:skip-S port)
  (define (skip-while skip-chars port)
    (do ((c (peek-char port) (peek-char port)))
        ((not (memv c skip-chars)) c)
      (read-char port)))
  (skip-while xlink:S-chars port))

(define (xlink:ncname-starting-char? a-char)
  (and (char? a-char) (or (char-alphabetic? a-char) (char=? #\_ a-char))))

(define (xlink:read-NCName port)
  (let ((first-char (peek-char port)))
    (or (xlink:ncname-starting-char? first-char)
        (xlink:parser-error port "XMLNS [4] for '" first-char "'")))
  (string->symbol
   (xlink:next-token-of
    (lambda (c)
      (cond
       ((eof-object? c) #f)
       ((char-alphabetic? c) c)
       ((string-index "0123456789.-_" c) c)
       (else #f)))
    port)))

(define (xlink:read-QName port)
  (let ((prefix-or-localpart (xlink:read-NCName port)))
    (case (peek-char port)
      ((#\:)
       (read-char port)
       (cons prefix-or-localpart (xlink:read-NCName port)))
      (else prefix-or-localpart))))

(define xlink:Prefix-XML (string->symbol "xml"))

(define xlink:name-compare
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
       ((eq? name2 xlink:largest-unres-name) '<)
       ((eq? name1 xlink:largest-unres-name) '>)
       ((eq? (car name1) (car name2)) (symbol-compare (cdr name1) (cdr name2)))
       (else (symbol-compare (car name1) (car name2)))))))

(define xlink:largest-unres-name
  (cons (string->symbol "#LARGEST-SYMBOL") (string->symbol "#LARGEST-SYMBOL")))

(define xlink:read-markup-token
  (let ()
    (define (skip-comment port)
      (xlink:assert-curr-char '(#\-) "XML [15], second dash" port)
      (if (not (xlink:find-string-from-port? "-->" port))
          (xlink:parser-error port "XML [15], no -->"))
      (xlink:make-xml-token 'COMMENT #f))
    (define (read-cdata port)
      (define (read-string n port)
        (if (not (positive? n))
            ""
            (let ((buffer (make-string n)))
              (let loop ((i 0) (c (read-char port)))
                (if (eof-object? c)
                    (substring buffer 0 i)
                    (let ((i1 (++ i)))
                      (string-set! buffer i c)
                      (if (= i1 n) buffer (loop i1 (read-char port)))))))))
      (assert (string=? "CDATA[" (read-string 6 port)))
      (xlink:make-xml-token 'CDSECT #f))
    (lambda (port)
      (xlink:assert-curr-char '(#\<) "start of the token" port)
      (case (peek-char port)
        ((#\/)
         (read-char port)
         (begin0
          (xlink:make-xml-token 'END (xlink:read-QName port))
          (xlink:skip-S port)
          (xlink:assert-curr-char '(#\>) "XML [42]" port)))
        ((#\?) (read-char port) (xlink:make-xml-token 'PI (xlink:read-NCName port)))
        ((#\!)
         (case (xlink:peek-next-char port)
           ((#\-) (read-char port) (skip-comment port))
           ((#\[) (read-char port) (read-cdata port))
           (else (xlink:make-xml-token 'DECL (xlink:read-NCName port)))))
        (else (xlink:make-xml-token 'START (xlink:read-QName port)))))))

(define (xlink:skip-pi port)
  (if (not (xlink:find-string-from-port? "?>" port))
      (xlink:parser-error port "Failed to find ?> terminating the PI")))

(define (xlink:read-pi-body-as-string port)
  (define (cons* a1 a2 . rest)
    (if (null? rest)
        (cons a1 a2)
        (cons a1 (apply cons* (cons a2 rest)))))
  (xlink:skip-S port)
  (string-concatenate/shared
   (let loop ()
     (let ((pi-fragment (xlink:next-token '() '(#\?) "reading PI content" port)))
       (if (eqv? #\> (xlink:peek-next-char port))
           (begin (read-char port) (cons pi-fragment '()))
           (cons* pi-fragment "?" (loop)))))))

(define (xlink:skip-internal-dtd port)
  (if (not (xlink:find-string-from-port? "]>" port))
      (xlink:parser-error
       port
       "Failed to find ]> terminating the internal DTD subset")))

(define xlink:read-cdata-body
  (let ((cdata-delimiters (list #\return #\newline #\] #\&)))
    (lambda (port str-handler seed)
      (let loop ((seed seed))
        (let ((fragment
               (xlink:next-token '() cdata-delimiters "reading CDATA" port)))
          (case (read-char port)
            ((#\newline) (loop (str-handler fragment "\n" seed)))
            ((#\])
             (if (not (eqv? (peek-char port) #\]))
                 (loop (str-handler fragment "]" seed))
                 (let check-after-second-braket ((seed
                                                  (if (string-null? fragment)
                                                      seed
                                                      (str-handler
                                                       fragment
                                                       ""
                                                       seed))))
                   (case (xlink:peek-next-char port)
                     ((#\>) (read-char port) seed)
                     ((#\])
                      (check-after-second-braket (str-handler "]" "" seed)))
                     (else (loop (str-handler "]]" "" seed)))))))
            ((#\&)
             (let ((ent-ref
                    (xlink:next-token-of
                     (lambda (c)
                       (and (not (eof-object? c)) (char-alphabetic? c) c))
                     port)))
               (cond
                ((and (string=? "gt" ent-ref) (eqv? (peek-char port) #\;))
                 (read-char port)
                 (loop (str-handler fragment ">" seed)))
                (else
                 (loop
                  (str-handler ent-ref "" (str-handler fragment "&" seed)))))))
            (else
             (if (eqv? (peek-char port) #\newline) (read-char port))
             (loop (str-handler fragment "\n" seed)))))))))

(define (xlink:read-char-ref port)
  ;; ucscode->char INT -> CHAR
  ;; Return a character whose UCS (ISO/IEC 10646) code is INT
  ;; Note
  ;; This function is required for processing of XML character entities:
  ;; According to Section "4.1 Character and Entity References"
  ;; of the XML Recommendation:
  ;;  "[Definition: A character reference refers to a specific character
  ;;   in the ISO/IEC 10646 character set, for example one not directly
  ;;   accessible from available input devices.]"
  (define (ucscode->char code)
    (cond-expand
     (bigloo
      (ucs2->char (integer->ucs2 code)))
     ((or scheme48 scsh)             ; Scheme48 has no support for UCS
      (integer->char code))
     (else
      (integer->char code))))
  (let* ((base
          (cond ((eqv? (peek-char port) #\x) (read-char port) 16) (else 10)))
         (name (xlink:next-token '() '(#\;) "XML [66]" port))
         (char-code (string->number name base)))
    (read-char port)
    (if (integer? char-code)
        (ucscode->char char-code)
        (xlink:parser-error port "[wf-Legalchar] broken for '" name "'"))))

(define xlink:predefined-parsed-entities
  `((,(string->symbol "amp") . "&")
    (,(string->symbol "lt") . "<")
    (,(string->symbol "gt") . ">")
    (,(string->symbol "apos") . "'")
    (,(string->symbol "quot") . "\"")))

(define (xlink:handle-parsed-entity
         port
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
           (lambda (port) (content-handler port new-entities seed))))
         ((procedure? ent-body)
          (let ((port (ent-body)))
            (begin0
             (content-handler port new-entities seed)
             (close-input-port port))))
         (else (xlink:parser-error port "[norecursion] broken for " name))))))
   ((assq name xlink:predefined-parsed-entities)
    =>
    (lambda (decl-entity) (str-handler (cdr decl-entity) "" seed)))
   (else (xlink:parser-error port "[wf-entdeclared] broken for " name))))

(define (xlink:make-empty-attlist) '())

(define (xlink:attlist-add attlist name-value)
  (if (null? attlist)
      (cons name-value attlist)
      (case (xlink:name-compare (car name-value) (caar attlist))
        ((=) #f)
        ((<) (cons name-value attlist))
        (else (cons (car attlist) (xlink:attlist-add (cdr attlist) name-value))))))

(define xlink:attlist-null? null?)

(define (xlink:attlist-remove-top attlist) (values (car attlist) (cdr attlist)))

(define (xlink:attlist->alist attlist) attlist)

(define (xlink:attlist-fold kons knil lis1)
  (let lp ((lis lis1) (ans knil))
    (if (null? lis) ans (lp (cdr lis) (kons (car lis) ans)))))

(define xlink:ssax-read-attributes
  (let ((value-delimeters (append xlink:S-chars '(#\< #\&))))
    (define (read-attrib-value delimiter port entities prev-fragments)
      (let* ((new-fragments
              (cons
               (xlink:next-token
                '()
                (cons delimiter value-delimeters)
                "XML [10]"
                port)
               prev-fragments))
             (cterm (read-char port)))
        (cond
         ((or (eof-object? cterm) (eqv? cterm delimiter)) new-fragments)
         ((eqv? cterm #\return)
          (if (eqv? (peek-char port) #\newline) (read-char port))
          (read-attrib-value delimiter port entities (cons " " new-fragments)))
         ((memv cterm xlink:S-chars)
          (read-attrib-value delimiter port entities (cons " " new-fragments)))
         ((eqv? cterm #\&)
          (cond
           ((eqv? (peek-char port) #\#)
            (read-char port)
            (read-attrib-value
             delimiter
             port
             entities
             (cons (string (xlink:read-char-ref port)) new-fragments)))
           (else
            (read-attrib-value
             delimiter
             port
             entities
             (read-named-entity port entities new-fragments)))))
         (else (xlink:parser-error port "[CleanAttrVals] broken")))))
    (define (read-named-entity port entities fragments)
      (define (cons* a1 a2 . rest)
        (if (null? rest)
            (cons a1 a2)
            (cons a1 (apply cons* (cons a2 rest)))))
      (let ((name (xlink:read-NCName port)))
        (xlink:assert-curr-char '(#\;) "XML [68]" port)
        (xlink:handle-parsed-entity
         port
         name
         entities
         (lambda (port entities fragments)
           (read-attrib-value '*eof* port entities fragments))
         (lambda (str1 str2 fragments)
           (if (equal? "" str2)
               (cons str1 fragments)
               (cons* str2 str1 fragments)))
         fragments)))
    (lambda (port entities)
      (let loop ((attr-list (xlink:make-empty-attlist)))
        (if (not (xlink:ncname-starting-char? (xlink:skip-S port)))
            attr-list
            (let ((name (xlink:read-QName port)))
              (xlink:skip-S port)
              (xlink:assert-curr-char '(#\=) "XML [25]" port)
              (xlink:skip-S port)
              (let ((delimiter (xlink:assert-curr-char '(#\' #\") "XML [10]" port)))
                (loop
                 (or (xlink:attlist-add
                      attr-list
                      (cons
                       name
                       (string-concatenate-reverse/shared
                        (read-attrib-value delimiter port entities '()))))
                     (xlink:parser-error
                      port
                      "[uniqattspec] broken for "
                      name))))))))))

(define (xlink:resolve-name port unres-name namespaces apply-default-ns?)
  (cond
   ((pair? unres-name)
    (cons
     (cond
      ((assq (car unres-name) namespaces) => cadr)
      ((eq? (car unres-name) xlink:Prefix-XML) xlink:Prefix-XML)
      (else
       (xlink:parser-error
        port
        "[nsc-NSDeclared] broken; prefix "
        (car unres-name))))
     (cdr unres-name)))
   (apply-default-ns?
    (let ((default-ns (assq '*DEFAULT* namespaces)))
      (if (and default-ns (cadr default-ns))
          (cons (cadr default-ns) unres-name)
          unres-name)))
   (else unres-name)))

(define (xlink:uri-string->symbol uri-str) (string->symbol uri-str))

(define xlink:complete-start-tag
  (let ((xmlns (string->symbol "xmlns"))
        (largest-dummy-decl-attr (list xlink:largest-unres-name #f #f #f)))
    (define (cons* a1 a2 . rest)
      (if (null? rest)
          (cons a1 a2)
          (cons a1 (apply cons* (cons a2 rest)))))
    (define (validate-attrs port attlist decl-attrs)
      (define (add-default-decl decl-attr result)
        (call-with-values
            (lambda () (apply values decl-attr))
          (lambda (attr-name content-type use-type default-value)
            (and (eq? use-type 'REQUIRED)
                 (xlink:parser-error port "[RequiredAttr] broken for" attr-name))
            (if default-value
                (cons (cons attr-name default-value) result)
                result))))
      (let loop ((attlist attlist) (decl-attrs decl-attrs) (result '()))
        (if (xlink:attlist-null? attlist)
            (xlink:attlist-fold add-default-decl result decl-attrs)
            (call-with-values
                (lambda () (xlink:attlist-remove-top attlist))
              (lambda (attr attr-others)
                (call-with-values
                    (lambda ()
                      (if (xlink:attlist-null? decl-attrs)
                          (values largest-dummy-decl-attr decl-attrs)
                          (xlink:attlist-remove-top decl-attrs)))
                  (lambda (decl-attr other-decls)
                    (case (xlink:name-compare (car attr) (car decl-attr))
                      ((<)
                       (if (or (eq? xmlns (car attr))
                               (and (pair? (car attr)) (eq? xmlns (caar attr))))
                           (loop attr-others decl-attrs (cons attr result))
                           (xlink:parser-error port "[ValueType] broken for " attr)))
                      ((>)
                       (loop attlist other-decls (add-default-decl decl-attr result)))
                      (else
                       (call-with-values
                           (lambda () (apply values decl-attr))
                         (lambda (attr-name content-type use-type default-value)
                           (cond
                            ((eq? use-type 'FIXED)
                             (or (equal? (cdr attr) default-value)
                                 (xlink:parser-error
                                  port
                                  "[FixedAttr] broken for "
                                  attr-name)))
                            ((eq? content-type 'CDATA) #t)
                            ((pair? content-type)
                             (or (member (cdr attr) content-type)
                                 (xlink:parser-error
                                  port
                                  "[enum] broken for "
                                  attr-name
                                  "="
                                  (cdr attr))))
                            (else
                             (xlink:warn
                              port
                              "declared content type "
                              content-type
                              " not verified yet")))
                           (loop attr-others other-decls
                                 (cons attr result)))))))))))))
    (define (add-ns port prefix uri-str namespaces)
      (and (equal? "" uri-str)
           (xlink:parser-error port "[dt-NSName] broken for " prefix))
      (let ((uri-symbol (xlink:uri-string->symbol uri-str)))
        (let loop ((nss namespaces))
          (cond
           ((null? nss) (cons (cons* prefix uri-symbol uri-symbol) namespaces))
           ((eq? uri-symbol (cddar nss))
            (cons (cons* prefix (cadar nss) uri-symbol) namespaces))
           (else (loop (cdr nss)))))))
    (define (adjust-namespace-decl port attrs namespaces)
      (let loop ((attrs attrs) (proper-attrs '()) (namespaces namespaces))
        (cond
         ((null? attrs) (values proper-attrs namespaces))
         ((eq? xmlns (caar attrs))
          (loop
           (cdr attrs)
           proper-attrs
           (if (equal? "" (cdar attrs))
               (cons (cons* '*DEFAULT* #f #f) namespaces)
               (add-ns port '*DEFAULT* (cdar attrs) namespaces))))
         ((and (pair? (caar attrs)) (eq? xmlns (caaar attrs)))
          (loop
           (cdr attrs)
           proper-attrs
           (add-ns port (cdaar attrs) (cdar attrs) namespaces)))
         (else
          (loop (cdr attrs) (cons (car attrs) proper-attrs) namespaces)))))
    (lambda (tag-head port elems entities namespaces)
      (let* ((attlist (xlink:ssax-read-attributes port entities))
             (empty-el-tag?
              (begin
                (xlink:skip-S port)
                (and (eqv?
                      #\/
                      (xlink:assert-curr-char
                       '(#\> #\/)
                       "XML [40], XML [44], no '>'"
                       port))
                     (xlink:assert-curr-char
                      '(#\>)
                      "XML [44], no '>'"
                      port)))))
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
                             (xlink:parser-error
                              port
                              "[elementvalid] broken, no decl for "
                              tag-head)))
                           (values (if empty-el-tag? 'EMPTY-TAG 'ANY) #f)))
          (lambda (elem-content decl-attrs)
            (define (fold-right kons knil lis1)
              (let recur ((lis lis1))
                (if (null? lis)
                    knil
                    (let ((head (car lis))) (kons head (recur (cdr lis)))))))
            (let ((merged-attrs
                   (if decl-attrs
                       (validate-attrs port attlist decl-attrs)
                       (xlink:attlist->alist attlist))))
              (call-with-values
                  (lambda ()
                    (adjust-namespace-decl port merged-attrs namespaces))
                (lambda (proper-attrs namespaces)
                  (values
                   (xlink:resolve-name port tag-head namespaces #t)
                   (fold-right
                    (lambda (name-value attlist)
                      (or (xlink:attlist-add
                           attlist
                           (cons
                            (xlink:resolve-name port (car name-value)
                                                namespaces #f)
                            (cdr name-value)))
                          (xlink:parser-error
                           port
                           "[uniqattspec] after NS expansion broken for "
                           name-value)))
                    (xlink:make-empty-attlist)
                    proper-attrs)
                   namespaces
                   elem-content))))))))))

(define (xlink:read-external-id port)
  (define (skip-until arg port)
    (cond
     ((number? arg)                     ; skip 'arg' characters
      (do ((i arg (-- i)))
      	  ((<= i 0) #f)
        (if (eof-object? (read-char port))
      	    (xlink:parser-error port "Unexpected EOF while skipping "
                                arg " characters"))))
     (else                             ; skip until break-chars (=arg)
      (let loop ((c (read-char port)))
        (cond
         ((memv c arg) c)
         ((eof-object? c)
          (if (memv '*eof* arg) c
              (xlink:parser-error port "Unexpected EOF while skipping until " arg)))
         (else (loop (read-char port))))))))
  (let ((discriminator (xlink:read-NCName port)))
    (xlink:assert-curr-char xlink:S-chars "space after SYSTEM or PUBLIC" port)
    (xlink:skip-S port)
    (let ((delimiter (xlink:assert-curr-char '(#\' #\") "XML [11], XML [12]" port)))
      (cond
       ((eq? discriminator (string->symbol "SYSTEM"))
        (begin0
         (xlink:next-token '() (list delimiter) "XML [11]" port)
         (read-char port)))
       ((eq? discriminator (string->symbol "PUBLIC"))
        (skip-until (list delimiter) port)
        (xlink:assert-curr-char xlink:S-chars "space after PubidLiteral" port)
        (xlink:skip-S port)
        (let* ((delimiter (xlink:assert-curr-char '(#\' #\") "XML [11]" port))
               (systemid (xlink:next-token '() (list delimiter) "XML [11]" port)))
          (read-char port)
          systemid))
       (else
        (xlink:parser-error
         port
         "XML [75], "
         discriminator
         " rather than SYSTEM or PUBLIC"))))))

(define (xlink:scan-Misc port)
  (let loop ((c (xlink:skip-S port)))
    (cond
     ((eof-object? c) c)
     ((not (char=? c #\<))
      (xlink:parser-error port "XML [22], char '" c "' unexpected"))
     (else
      (let ((token (xlink:read-markup-token port)))
        (case (xlink:xml-token-kind token)
          ((COMMENT) (loop (xlink:skip-S port)))
          ((PI DECL START) token)
          (else
           (xlink:parser-error
            port
            "XML [22], unexpected token of kind "
            (xlink:xml-token-kind token)))))))))

(define xlink:read-char-data
  (let ((terminators-usual (list #\< #\& #\return))
        (terminators-usual-eof (list #\< '*eof* #\& #\return))
        (handle-fragment
         (lambda (fragment str-handler seed)
           (if (string-null? fragment) seed (str-handler fragment "" seed)))))
    (lambda (port expect-eof? str-handler seed)
      (if (eqv? #\< (peek-char port))
          (let ((token (xlink:read-markup-token port)))
            (case (xlink:xml-token-kind token)
              ((START END) (values seed token))
              ((CDSECT)
               (let ((seed (xlink:read-cdata-body port str-handler seed)))
                 (xlink:read-char-data port expect-eof? str-handler seed)))
              ((COMMENT) (xlink:read-char-data port expect-eof? str-handler seed))
              (else (values seed token))))
          (let ((char-data-terminators
                 (if expect-eof? terminators-usual-eof terminators-usual)))
            (let loop ((seed seed))
              (let* ((fragment
                      (xlink:next-token
                       '()
                       char-data-terminators
                       "reading char data"
                       port))
                     (term-char (peek-char port)))
                (if (eof-object? term-char)
                    (values (handle-fragment fragment str-handler seed) term-char)
                    (case term-char
                      ((#\<)
                       (let ((token (xlink:read-markup-token port)))
                         (case (xlink:xml-token-kind token)
                           ((CDSECT)
                            (loop
                             (xlink:read-cdata-body
                              port
                              str-handler
                              (handle-fragment fragment str-handler seed))))
                           ((COMMENT)
                            (loop (handle-fragment fragment str-handler seed)))
                           (else
                            (values
                             (handle-fragment fragment str-handler seed)
                             token)))))
                      ((#\&)
                       (case (xlink:peek-next-char port)
                         ((#\#)
                          (read-char port)
                          (loop
                           (str-handler
                            fragment
                            (string (xlink:read-char-ref port))
                            seed)))
                         (else
                          (let ((name (xlink:read-NCName port)))
                            (xlink:assert-curr-char '(#\;) "XML [68]" port)
                            (values
                             (handle-fragment fragment str-handler seed)
                             (xlink:make-xml-token 'ENTITY-REF name))))))
                      (else
                       (if (eqv? (xlink:peek-next-char port) #\newline) (read-char port))
                       (loop
                        (str-handler fragment (string #\newline) seed))))))))))))

(define (xlink:assert-token token kind gi error-cont)
  (or (and (xlink:xml-token? token)
           (eq? kind (xlink:xml-token-kind token))
           (equal? gi (xlink:xml-token-head token)))
      (error-cont token kind gi)))

;; Returns
(define (xlink:RES-NAME->SXML res-name)
  (string->symbol
   (string-append
    (symbol->string (car res-name))
    ":"
    (symbol->string (cdr res-name)))))

;; Given the list of fragments (some of which are text strings)
;; reverse the list and concatenate adjacent text strings
(define (xlink:reverse-collect-str fragments)
  (cond
   ((null? fragments) '())
   ((null? (cdr fragments)) fragments)
   (else
    (let loop ((fragments fragments) (result '()) (strs '()))
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

(define (xlink:reverse-collect-str-drop-ws fragments)
  (cond
   ((null? fragments) '())
   ((null? (cdr fragments))
    (if (and (string? (car fragments)) (xlink:string-whitespace? (car fragments)))
        '()
        fragments))
   (else
    (let loop ((fragments fragments)
               (result '())
               (strs '())
               (all-whitespace? #t))
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
         (and all-whitespace? (xlink:string-whitespace? (car fragments)))))
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

;;! This is an instance of a SSAX parser that returns an SXML
;; representation of the XML document to be read from PORT.
;; NAMESPACE-PREFIX-ASSIG is a list of (USER-PREFIX . URI-STRING)
;; that assigns USER-PREFIXes to certain namespaces identified by
;; particular URI-STRINGs. It may be an empty list.
;; The procedure returns an SXML tree. The port points out to the
;; first character after the root element.
(define (xlink:xml->sxml port namespace-prefix-assig)
  (define (cons* a1 a2 . rest)
    (cons a1 (apply cons* (cons a2 rest))))
  (letrec ((namespaces
            (map
             (lambda (el)
               (cons* #f (car el) (xlink:uri-string->symbol (cdr el))))
             namespace-prefix-assig))
           (xlink:RES-NAME->SXML
            (lambda (res-name)
              (string->symbol
               (string-append
                (symbol->string (car res-name))
                ":"
                (symbol->string (cdr res-name)))))))
    (let ((result
           (reverse
            ((xlink:make-parser
              NEW-LEVEL-SEED
              (lambda (elem-gi attributes namespaces expected-content seed)
                '())
              FINISH-ELEMENT
              (lambda (elem-gi attributes namespaces parent-seed seed)
                (let ((seed (xlink:reverse-collect-str-drop-ws seed))
                      (attrs
                       (xlink:attlist-fold
                        (lambda (attr accum)
                          (cons
                           (list
                            (if (symbol? (car attr))
                                (car attr)
                                (xlink:RES-NAME->SXML (car attr)))
                            (cdr attr))
                           accum))
                        '()
                        attributes)))
                  (cons
                   (cons
                    (if (symbol? elem-gi) elem-gi (xlink:RES-NAME->SXML elem-gi))
                    (if (null? attrs) seed (cons (cons '@ attrs) seed)))
                   parent-seed)))
              CHAR-DATA-HANDLER
              (lambda (string1 string2 seed)
                (if (string-null? string2)
                    (cons string1 seed)
                    (cons* string2 string1 seed)))
              DOCTYPE
              (lambda (port docname systemid internal-subset? seed)
                (when internal-subset?
                      (xlink:warn
                       port
                       "Internal DTD subset is not currently handled ")
                      (xlink:skip-internal-dtd port))
                (xlink:warn
                 port
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
                (port pi-tag seed)
                (cons
                 (list '*PI* pi-tag (xlink:read-pi-body-as-string port))
                 seed))))
             port
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
            result))))))

(define XLINK:XML->SXML xlink:xml->sxml)
