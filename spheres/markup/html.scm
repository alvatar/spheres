;;!!! HtmlPrag provides permissive HTML parsing and emitting capability to Scheme
;; programs.  The parser is useful for software agent extraction of
;; information from Web pages, for programmatically transforming HTML files,
;; and for implementing interactive Web browsers.  HtmlPrag emits ``SHTML,''
;; which is an encoding of HTML in [SXML], so that conventional HTML may be
;; processed with XML tools such as [SXPath] and [SXML-Tools].  Like
;; [SSAX-HTML], HtmlPrag provides a permissive tokenizer, but also attempts to
;; recover structure.  HtmlPrag also includes procedures for encoding SHTML in
;; HTML syntax.
;;
;; The HtmlPrag parsing behavior is permissive in that it accepts erroneous
;; HTML, handling several classes of HTML syntax errors gracefully, without
;; yielding a parse error.  This is crucial for parsing arbitrary real-world
;; Web pages, since many pages actually contain syntax errors that would
;; defeat a strict or validating parser.  HtmlPrag's handling of errors is
;; intended to generally emulate popular Web browsers' interpretation of the
;; structure of erroneous HTML.  We euphemistically term this kind of parse
;; ``pragmatic.''
;;
;; HtmlPrag also has some support for [XHTML], although XML namespace
;; qualifiers [XML-Names] are currently accepted but stripped from the
;; resulting SHTML.  Note that valid XHTML input is of course better handled
;; by a validating XML parser like [SSAX].
;;
;; To receive notification of new versions of HtmlPrag, and to be polled for
;; input on changes to HtmlPrag being considered, ask the author to add you to
;; the @code{scheme-announce} moderated email list,
;;
;; HtmlPrag requires R5RS, [SRFI-6], and [SRFI-23].

;; The following bindings are used internally by HtmlPrag for portability, with
;; the intention that packagings of HtmlPrag use more efficient procedures for
;; the particular Scheme implementation.  This is waiting on universal support
;; of SRFI-0.


;;! Returns the character with ASCII value @var{num}.  In most Scheme
;; implementations, this is the same as @code{integer->char}.  Two exceptions
;; are Scheme 48 0.57 and Scsh 0.6.3, for which the user must manually edit
;; file @code{htmlprag.scm} to bind this variable to @code{ascii->char}.  A
;; future version of HtmlPrag will automatically use @code{ascii->char} where
;; available.
(define %htmlprag:a2c integer->char)

;;! Returns a concatenation of lists @var{a} and @var{b}, modifying the tail of
;; @var{a} to point to the head of @var{b} if both lists are non-null.  A
;; future version should use the more general @code{append!} where available.
(define (%htmlprag:append! a b)
  (cond ((null? a) b)
        ((null? b) a)
        (else      (let loop  ((sub a))
                     (if (null? (cdr sub))
                         (begin (set-cdr! sub b)
                                a)
                         (loop (cdr sub)))))))

;;! Returns a reversed list @var{lst}, possibly destructive.  A future version
;; will use @code{reverse!} where available, and @code{reverse} elsewhere.
(define %htmlprag:reverse!ok reverse)

;; Top-level cond-expand expanded automatically
(define %htmlprag:error error)

(define (%htmlprag:down s)
  (list->string (map char-downcase (string->list s))))

;;! Returns a string that is equivalent to @var{str} with all characters mapped
;; to lowercase, as if by @code{char-downcase}, possibly mutating @var{str}.
;; A future version should use the Scheme implementation's native destructive
;; or nondestructive procedure where available.
(define %htmlprag:down!ok %htmlprag:down)

;;! One-shot version of the conventional @code{get-output-string}.  The result
;; of any subsequent attempt to write to the port or get the output string is
;; undefined.  This may or may not free up resources.
(define (%htmlprag:gosc os)
  (let ((str (get-output-string os)))
    ;; Note: By default, we don't call close-output-port, since at least one
    ;; tested Scheme implementation barfs on that.
    ;;
    ;; (close-output-port os)
    str))


;;-------------------------------------------------------------------------------
;;!! SHTML and SXML

;; SHTML is a variant of [SXML], with two minor but useful extensions:
;;
;; The SXML keyword symbols, such as @code{*TOP*}, are defined to be in all
;; uppercase, regardless of the case-sensitivity of the reader of the hosting
;; Scheme implementation in any context.  This avoids several pitfalls.
;;
;; Since not all character entity references used in HTML can be converted to
;; Scheme characters in all R5RS Scheme implementations, nor represented in
;; conventional text files or other common external text formats to which one
;; might wish to write SHTML, SHTML adds a special @code{&} syntax for
;; non-ASCII (or non-Extended-ASCII) characters.  The syntax is @code{(&
;; @var{val})}, where @var{val} is a symbol or string naming with the symbolic
;; name of the character, or an integer with the numeric value of the
;; character.


;; These variables are bound to the following case-sensitive symbols used in
;; SHTML, respectively: @code{*COMMENT*}, @code{*DECL*}, @code{*EMPTY*},
;; @code{*END*}, @code{*ENTITY*}, @code{*PI*}, @code{*START*}, @code{*TEXT*},
;; and @code{*TOP*}.  These can be used in lieu of the literal symbols in
;; programs read by a case-insensitive Scheme reader.@footnote{Scheme
;; implementators who have not yet made @code{read} case-sensitive by default
;; are encouraged to do so.}
(define shtml-comment-symbol (string->symbol "*COMMENT*"))
(define shtml-decl-symbol    (string->symbol "*DECL*"))
(define shtml-empty-symbol   (string->symbol "*EMPTY*"))
(define shtml-end-symbol     (string->symbol "*END*"))
(define shtml-entity-symbol  (string->symbol "*ENTITY*"))
(define shtml-pi-symbol      (string->symbol "*PI*"))
(define shtml-start-symbol   (string->symbol "*START*"))
(define shtml-text-symbol    (string->symbol "*TEXT*"))
(define shtml-top-symbol     (string->symbol "*TOP*"))

;; These variables are bound to the SHTML entity public identifier strings
;; used in SHTML @code{*ENTITY*} named and numeric character entity
;; references.
(define shtml-named-char-id   "shtml-named-char")
(define shtml-numeric-char-id "shtml-numeric-char")

;;! Yields an SHTML character entity reference for @var{val}.  For example:
;;
;; (make-shtml-entity "rArr")                  @result{} (& rArr)
;; (make-shtml-entity (string->symbol "rArr")) @result{} (& rArr)
;; (make-shtml-entity 151)                     @result{} (& 151)
(define (make-shtml-entity val)
  (list '& (cond ((symbol?  val) val)
                 ((integer? val) val)
                 ((string?  val) (string->symbol val))
                 (else (%htmlprag:error "make-shtml-entity"
                                        "invalid SHTML entity value:"
                                        val)))))

;; TODO:
;;
;; (define (shtml-entity? x)
;;   (and (shtml-entity-value entity) #t))

;;! Yields the value for the SHTML entity @var{obj}, or @code{#f} if @var{obj}
;; is not a recognized entity.  Values of named entities are symbols, and
;; values of numeric entities are numbers.  An error may raised if @var{obj}
;; is an entity with system ID inconsistent with its public ID.  For example:
;;
;; (define (f s) (shtml-entity-value (cadr (html->shtml s))))
;; (f "&nbsp;")  @result{} nbsp
;; (f "&#2000;") @result{} 2000
(define (shtml-entity-value entity)
  (cond ((not (pair? entity)) #f)
        ((null? (cdr entity)) #f)
        ((eqv? (car entity) '&)
         ;; TODO: Error-check for extraneous list members?
         (let ((val (cadr entity)))
           (cond ((symbol?  val) val)
                 ((integer? val) val)
                 ((string?  val) (string->symbol val))
                 (else           #f))))
        ((eqv? (car entity) shtml-entity-symbol)
         (if (null? (cddr entity))
             #f
             (let ((public-id (list-ref entity 1))
                   (system-id (list-ref entity 2)))
               ;; TODO: Error-check for extraneous list members?
               (cond ((equal? public-id shtml-named-char-id)
                      (string->symbol system-id))
                     ((equal? public-id shtml-numeric-char-id)
                      (string->number system-id))
                     (else #f)))))
        (else #f)))


;;-------------------------------------------------------------------------------
;;!! Tokenizing

;; The tokenizer is used by the higher-level structural parser, but can also
;; be called directly for debugging purposes or unusual applications.  Some of
;; the list structure of tokens, such as for start tag tokens, is mutated and
;; incorporated into the SHTML list structure emitted by the parser.

;; TODO: Document the token format.

;;! Constructs an HTML tokenizer procedure on input port @var{in}.  If boolean
;; @var{normalized?} is true, then tokens will be in a format conducive to use
;; with a parser emitting normalized SXML.  Each call to the resulting
;; procedure yields a successive token from the input.  When the tokens have
;; been exhausted, the procedure returns the null list.  For example:
;;
;; (define input (open-input-string "<a href=\"foo\">bar</a>"))
;; (define next  (make-html-tokenizer input #f))
;; (next) @result{} (a (@@ (href "foo")))
;; (next) @result{} "bar"
;; (next) @result{} (*END* a)
;; (next) @result{} ()
;; (next) @result{} ()
(define make-html-tokenizer
  ;; TODO: Have the tokenizer replace contiguous whitespace within individual
  ;; text tokens with single space characters (except for when in `pre' and
  ;; verbatim elements).  The parser will introduce new contiguous whitespace
  ;; (e.g., when text tokens are concatenated, invalid end tags are removed,
  ;; whitespace is irrelevant between certain elements), but then the parser
  ;; only has to worry about the first and last character of each string.
  ;; Perhaps the text tokens should have both leading and trailing whitespace
  ;; stripped, and contain flags for whether or not leading and trailing
  ;; whitespace occurred.
  (letrec ((no-token '())
           ;; TODO: Maybe make this an option.
           (verbatim-to-eof-elems '(plaintext))
           ;; TODO: Implement proper parsing of `verbatim-pair-elems' elements.
           ;; Note that we must support invalid termination like this:
           (verbatim-pair-elems '(script server style xmp))
           (ws-chars (list #\space
                           (%htmlprag:a2c 9)
                           (%htmlprag:a2c 10)
                           (%htmlprag:a2c 11)
                           (%htmlprag:a2c 12)
                           (%htmlprag:a2c 13)))
           (output-string->string-or-false
            (lambda (os)
              (let ((s (%htmlprag:gosc os)))
                (if (string=? s "") #f s))))
           (output-string->symbol-or-false
            (lambda (os)
              (let ((s (output-string->string-or-false os)))
                (if s (string->symbol s) #f)))))
    (lambda (in normalized?)
      ;; TODO: Make a tokenizer option that causes XML namespace qualifiers to
      ;; be ignored.
      (letrec
          (
           ;; Port buffer with inexpensive unread of one character and slightly
           ;; more expensive pushback of second character to unread.  The
           ;; procedures themselves do no consing.  The tokenizer currently
           ;; needs two-symbol lookahead, due to ambiguous "/" while parsing
           ;; element and attribute names, which could be either empty-tag
           ;; syntax or XML qualified names.
           (c           #f)
           (next-c      #f)
           (c-consumed? #t)
           (read-c      (lambda ()
                          (if c-consumed?
                              (if next-c
                                  (begin (set! c      next-c)
                                         (set! next-c #f))
                                  (set! c (read-char in)))
                              (set! c-consumed? #t))))
           (unread-c    (lambda ()
                          (if c-consumed?
                              (set! c-consumed? #f)
                              ;; TODO: Procedure name in error message really
                              ;; isn't "make-html-tokenizer"...
                              (%htmlprag:error "make-html-tokenizer"
                                               "already unread:"
                                               c))))
           (push-c      (lambda (new-c)
                          (if c-consumed?
                              (begin (set! c           new-c)
                                     (set! c-consumed? #f))
                              (if next-c
                                  (%htmlprag:error
                                   "make-html-tokenizer"
                                   "pushback full:"
                                   c)
                                  (begin (set! next-c      c)
                                         (set! c           new-c)
                                         (set! c-consumed? #f))))))
           ;; TODO: These procedures are a temporary convenience for
           ;; enumerating the pertinent character classes, with an eye towards
           ;; removing redundant tests of character class.  These procedures
           ;; should be eliminated in a future version.
           (c-eof?      (lambda () (eof-object? c)))
           (c-amp?      (lambda () (eqv? c #\&)))
           (c-apos?     (lambda () (eqv? c #\')))
           (c-bang?     (lambda () (eqv? c #\!)))
           (c-colon?    (lambda () (eqv? c #\:)))
           (c-quot?     (lambda () (eqv? c #\")))
           (c-equals?   (lambda () (eqv? c #\=)))
           (c-gt?       (lambda () (eqv? c #\>)))
           (c-lt?       (lambda () (eqv? c #\<)))
           (c-minus?    (lambda () (eqv? c #\-)))
           (c-pound?    (lambda () (eqv? c #\#)))
           (c-ques?     (lambda () (eqv? c #\?)))
           (c-semi?     (lambda () (eqv? c #\;)))
           (c-slash?    (lambda () (eqv? c #\/)))
           (c-splat?    (lambda () (eqv? c #\*)))
           (c-lf?       (lambda () (eqv? c #\newline)))
           (c-angle?    (lambda () (memv c '(#\< #\>))))
           (c-ws?       (lambda () (memv c ws-chars)))
           (c-alpha?    (lambda () (char-alphabetic? c)))
           (c-digit?    (lambda () (char-numeric? c)))
           (c-alphanum? (lambda () (or (c-alpha?) (c-digit?))))
           (c-hexlet?   (lambda () (memv c '(#\a #\b #\c #\d #\e #\f
                                        #\A #\B #\C #\D #\E #\F))))
           (skip-ws     (lambda () (read-c) (if (c-ws?) (skip-ws) (unread-c))))
           (make-start-token
            (if normalized?
                (lambda (name ns attrs)
                  (list name (cons '@ attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list name)
                      (list name (cons '@ attrs))))))
           (make-empty-token
            (lambda (name ns attrs)
              (cons shtml-empty-symbol
                    (make-start-token name ns attrs))))
           (make-end-token
            (if normalized?
                (lambda (name ns attrs)
                  (list shtml-end-symbol
                        name
                        (cons '@ attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list shtml-end-symbol name)
                      (list shtml-end-symbol
                            name
                            (cons '@ attrs))))))
           (make-comment-token
            (lambda (str) (list shtml-comment-symbol str)))
           (make-decl-token
            (lambda (parts) (cons shtml-decl-symbol parts)))
           (scan-qname
            ;; TODO: Make sure we don't accept local names that have "*", since
            ;; this can break SXML tools.  Have to validate this afterwards if
            ;; "verbatim-safe?".  Also check for "@" and maybe "@@".  Check
            ;; qname parsing code, especially for verbatim mode.  This is
            ;; important!
            (lambda (verbatim-safe?)
              ;; Note: If we accept some invalid local names, we only need two
              ;; symbols of lookahead to determine the end of a qname.
              (letrec ((os      #f)
                       (ns      '())
                       (vcolons 0)
                       (good-os (lambda ()
                                  (or os
                                      (begin (set! os (open-output-string))
                                             os)))))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((or (c-ws?) (c-splat?))
                         (if verbatim-safe?
                             (unread-c)))
                        ((or (c-angle?) (c-equals?) (c-quot?) (c-apos?))
                         (unread-c))
                        ((c-colon?)
                         (or (null? ns)
                             (set! ns (cons ":" ns)))
                         (if os
                             (begin
                               (set! ns (cons (%htmlprag:gosc os)
                                              ns))
                               (set! os #f)))
                         (loop))
                        ((c-slash?)
                         (read-c)
                         (cond ((or (c-eof?)
                                    (c-ws?)
                                    (c-equals?)
                                    (c-apos?)
                                    (c-quot?)
                                    (c-angle?)
                                    (c-splat?))
                                (unread-c)
                                (push-c #\/))
                               (else (write-char #\/ (good-os))
                                     (write-char c   os)
                                     (loop))))
                        (else (write-char c (good-os))
                              (loop))))
                (let ((ns    (if (null? ns)
                                 #f
                                 (apply string-append
                                        (%htmlprag:reverse!ok ns))))
                      (local (if os (%htmlprag:gosc os) #f)))
                  (if verbatim-safe?
                      ;; TODO: Make sure we don't have ambiguous ":" or drop
                      ;; any characters!
                      (cons ns local)
                      ;; Note: We represent "xml:" and "xmlns:" syntax as
                      ;; normal qnames, for lack of something better to do with
                      ;; them when we don't support XML namespaces.
                      ;;
                      ;; TODO: Local names are currently forced to lowercase,
                      ;; since HTML is usually case-insensitive.  If XML
                      ;; namespaces are used, we might wish to keep local names
                      ;; case-sensitive.
                      (if local
                          (if ns
                              (if (or (string=? ns "xml")
                                      (string=? ns "xmlns"))
                                  (string->symbol (string-append ns ":" local))
                                  (cons ns
                                        (string->symbol
                                         (%htmlprag:down!ok
                                          local))))
                              (string->symbol
                               (%htmlprag:down!ok local)))
                          (if ns
                              (string->symbol
                               (%htmlprag:down!ok ns))
                              ;; TODO: Ensure that it's OK to return #f as a
                              ;; name.
                              #f)))))))
           (scan-tag
            (lambda (start?)
              (skip-ws)
              (let ((tag-name   (scan-qname #f))
                    (tag-ns     #f)
                    (tag-attrs  #f)
                    (tag-empty? #f))
                ;; Scan element name.
                (if (pair? tag-name)
                    (begin (set! tag-ns   (car tag-name))
                           (set! tag-name (cdr tag-name))))
                ;; TODO: Ensure there's no case in which a #f tag-name isn't
                ;; compensated for later.
                ;;
                ;; Scan element attributes.
                (set! tag-attrs
                      (let scan-attr-list ()
                        (read-c)
                        (cond ((c-eof?)   '())
                              ((c-angle?) (unread-c) '())
                              ((c-slash?)
                               (set! tag-empty? #t)
                               (scan-attr-list))
                              ((c-alpha?)
                               (unread-c)
                               (let ((attr (scan-attr)))
                                 (cons attr (scan-attr-list))))
                              (else (scan-attr-list)))))
                ;; Find ">" or unnatural end.
                (let loop ()
                  (read-c)
                  (cond ((c-eof?)   no-token)
                        ((c-slash?) (set! tag-empty? #t) (loop))
                        ((c-gt?)    #f)
                        ((c-ws?)    (loop))
                        (else       (unread-c))))
                ;; Change the tokenizer mode if necessary.
                (cond ((not start?) #f)
                      (tag-empty?   #f)
                      ;; TODO: Maybe make one alist lookup here, instead of
                      ;; two.
                      ((memq tag-name verbatim-to-eof-elems)
                       (set! nexttok verbeof-nexttok))
                      ((memq tag-name verbatim-pair-elems)
                       (set! nexttok (make-verbpair-nexttok tag-name))))
                ;; Return a token object.
                (if start?
                    (if tag-empty?
                        (make-empty-token tag-name tag-ns tag-attrs)
                        (make-start-token tag-name tag-ns tag-attrs))
                    (make-end-token tag-name tag-ns tag-attrs)))))
           (scan-attr
            (lambda ()
              (let ((name (scan-qname #f))
                    (val  #f))
                (if (pair? name)
                    (set! name (cdr name)))
                (let loop-equals-or-end ()
                  (read-c)
                  (cond ((c-eof?) no-token)
                        ((c-ws?)  (loop-equals-or-end))
                        ((c-equals?)
                         (let loop-quote-or-unquoted ()
                           (read-c)
                           (cond ((c-eof?) no-token)
                                 ((c-ws?) (loop-quote-or-unquoted))
                                 ((or (c-apos?) (c-quot?))
                                  (let ((term c))
                                    (set! val (open-output-string))
                                    (let loop-quoted-val ()
                                      (read-c)
                                      (cond ((c-eof?)      #f)
                                            ((eqv? c term) #f)
                                            (else (write-char c val)
                                                  (loop-quoted-val))))))
                                 ((c-angle?) (unread-c))
                                 (else
                                  (set! val (open-output-string))
                                  (write-char c val)
                                  (let loop-unquoted-val ()
                                    (read-c)
                                    (cond ((c-eof?)  no-token)
                                          ((c-apos?) #f)
                                          ((c-quot?) #f)
                                          ((or (c-ws?) (c-angle?)
                                               ;;(c-slash?)
                                               )
                                           (unread-c))
                                          ;; Note: We can treat a slash in an
                                          ;; unquoted attribute value as a
                                          ;; value constituent because the
                                          ;; slash is specially-handled only
                                          ;; for XHTML, and XHTML attribute
                                          ;; values must always be quoted.  We
                                          ;; could do lookahead for "/>", but
                                          ;; that wouldn't let us parse HTML
                                          ;; "<a href=/>" correctly, so this is
                                          ;; an easier and more correct way to
                                          ;; do things.
                                          (else (write-char c val)
                                                (loop-unquoted-val))))))))
                        (else (unread-c))))
                (if normalized?
                    (list name (if val
                                   (%htmlprag:gosc val)
                                   (symbol->string name)))
                    (if val
                        (list name (%htmlprag:gosc val))
                        (list name))))))
           (scan-comment
            ;; TODO: Rewrite this to use tail recursion rather than a state
            ;; variable.
            (lambda ()
              (let ((os    (open-output-string))
                    (state 'start-minus))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((c-minus?)
                         (set! state
                               (case state
                                 ((start-minus) 'start-minus-minus)
                                 ((start-minus-minus body) 'end-minus)
                                 ((end-minus) 'end-minus-minus)
                                 ((end-minus-minus)
                                  (write-char #\- os)
                                  state)
                                 (else (%htmlprag:error
                                        "make-html-tokenizer"
                                        "invalid state:"
                                        state))))
                         (loop))
                        ((and (c-gt?) (eq? state 'end-minus-minus)) #f)
                        (else (case state
                                ((end-minus)       (write-char #\- os))
                                ((end-minus-minus) (display "--" os)))
                              (set! state 'body)
                              (write-char c os)
                              (loop))))
                (make-comment-token (%htmlprag:gosc os)))))
           (scan-pi
            (lambda ()
              (skip-ws)
              (let ((name (open-output-string))
                    (val  (open-output-string)))
                (let scan-name ()
                  (read-c)
                  (cond ((c-eof?)   #f)
                        ((c-ws?)    #f)
                        ((c-alpha?) (write-char c name) (scan-name))
                        (else       (unread-c))))
                ;; TODO: Do we really want to emit #f for PI name?
                (set! name (output-string->symbol-or-false name))
                (let scan-val ()
                  (read-c)
                  (cond ((c-eof?)  #f)
                        ;; ((c-amp?) (display (scan-entity) val)
                        ;;           (scan-val))
                        ((c-ques?)
                         (read-c)
                         (cond ((c-eof?) (write-char #\? val))
                               ((c-gt?)  #f)
                               (else     (write-char #\? val)
                                         (unread-c)
                                         (scan-val))))
                        (else (write-char c val) (scan-val))))
                (list shtml-pi-symbol
                      name
                      (%htmlprag:gosc val)))))
           (scan-decl
            ;; TODO: Find if SXML includes declaration forms, and if so, use
            ;; whatever format SXML wants.
            ;;
            ;; TODO: Rewrite to eliminate state variables.
            (letrec
                ((scan-parts
                  (lambda ()
                    (let ((part       (open-output-string))
                          (nonsymbol? #f)
                          (state      'before)
                          (last?      #f))
                      (let loop ()
                        (read-c)
                        (cond ((c-eof?) #f)
                              ((c-ws?)
                               (case state
                                 ((before) (loop))
                                 ((quoted) (write-char c part) (loop))))
                              ((and (c-gt?) (not (eq? state 'quoted)))
                               (set! last? #t))
                              ((and (c-lt?) (not (eq? state 'quoted)))
                               (unread-c))
                              ((c-quot?)
                               (case state
                                 ((before)   (set! state 'quoted) (loop))
                                 ((unquoted) (unread-c))
                                 ((quoted)   #f)))
                              (else
                               (if (eq? state 'before)
                                   (set! state 'unquoted))
                               (set! nonsymbol? (or nonsymbol?
                                                    (not (c-alphanum?))))
                               (write-char c part)
                               (loop))))
                      (set! part (%htmlprag:gosc part))
                      (if (string=? part "")
                          '()
                          (cons (if (or (eq? state 'quoted) nonsymbol?)
                                    part
                                    ;; TODO: Normalize case of things we make
                                    ;; into symbols here.
                                    (string->symbol part))
                                (if last?
                                    '()
                                    (scan-parts))))))))
              (lambda () (make-decl-token (scan-parts)))))
           (scan-entity
            (lambda ()
              (read-c)
              (cond ((c-eof?) "&")
                    ((c-alpha?)
                     ;; TODO: Do entity names have a maximum length?
                     (let ((name (open-output-string)))
                       (write-char c name)
                       (let loop ()
                         (read-c)
                         (cond ((c-eof?)   #f)
                               ((c-alpha?) (write-char c name) (loop))
                               ((c-semi?)  #f)
                               (else       (unread-c))))
                       (set! name (%htmlprag:gosc name))
                       ;; TODO: Make the entity map an option.
                       (let ((pair (assoc name '(("amp"  . "&")
                                                 ("apos" . "'")
                                                 ("gt"   . ">")
                                                 ("lt"   . "<")
                                                 ("quot" . "\"")))))
                         (if pair
                             (cdr pair)
                             (make-shtml-entity name)))))
                    ((c-pound?)
                     (let ((num  (open-output-string))
                           (hex? #f))
                       (read-c)
                       (cond ((c-eof?)            #f)
                             ((memv c '(#\x #\X)) (set! hex? #t) (read-c)))
                       (let loop ()
                         (cond ((c-eof?)  #f)
                               ((c-semi?) #f)
                               ((or (c-digit?) (and hex? (c-hexlet?)))
                                (write-char c num)
                                (read-c)
                                (loop))
                               (else (unread-c))))
                       (set! num (%htmlprag:gosc num))
                       (if (string=? num "")
                           "&#;"
                           (let ((n (string->number num (if hex? 16 10))))
                             (if (and (<= 32 n 255) (not (= n 127)))
                                 (string (%htmlprag:a2c n))
                                 (make-shtml-entity n))))))
                    (else (unread-c) "&"))))
           (normal-nexttok
            (lambda ()
              (read-c)
              (cond ((c-eof?) no-token)
                    ((c-lt?)
                     (let loop ()
                       (read-c)
                       (cond ((c-eof?)   "<")
                             ((c-ws?)    (loop))
                             ((c-slash?) (scan-tag #f))
                             ((c-ques?)  (scan-pi))
                             ((c-bang?)  (let loop ()
                                           (read-c)
                                           (cond ((c-eof?)   no-token)
                                                 ((c-ws?)    (loop))
                                                 ((c-minus?) (scan-comment))
                                                 (else       (unread-c)
                                                             (scan-decl)))))
                             ((c-alpha?) (unread-c) (scan-tag #t))
                             (else       (unread-c) "<"))))
                    ((c-gt?) ">")
                    (else (let ((os (open-output-string)))
                            (let loop ()
                              (cond ((c-eof?)   #f)
                                    ((c-angle?) (unread-c))
                                    ((c-amp?)
                                     (let ((entity (scan-entity)))
                                       (if (string? entity)
                                           (begin (display entity os)
                                                  (read-c)
                                                  (loop))
                                           (let ((saved-nexttok nexttok))
                                             (set! nexttok
                                                   (lambda ()
                                                     (set! nexttok
                                                           saved-nexttok)
                                                     entity))))))
                                    (else (write-char c os)
                                          (or (c-lf?)
                                              (begin (read-c) (loop))))))
                            (let ((text (%htmlprag:gosc os)))
                              (if (equal? text "")
                                  (nexttok)
                                  text)))))))
           (verbeof-nexttok
            (lambda ()
              (read-c)
              (if (c-eof?)
                  no-token
                  (let ((os (open-output-string)))
                    (let loop ()
                      (or (c-eof?)
                          (begin (write-char c os)
                                 (or (c-lf?)
                                     (begin (read-c) (loop))))))
                    (%htmlprag:gosc os)))))
           (make-verbpair-nexttok
            (lambda (elem-name)
              (lambda ()
                (let ((os (open-output-string)))
                  ;; Accumulate up to a newline-terminated line.
                  (let loop ()
                    (read-c)
                    (cond ((c-eof?)
                           ;; Got EOF in verbatim context, so set the normal
                           ;; nextok procedure, then fall out of loop.
                           (set! nexttok normal-nexttok))
                          ((c-lt?)
                           ;; Got "<" in verbatim context, so get next
                           ;; character.
                           (read-c)
                           (cond ((c-eof?)
                                  ;; Got "<" then EOF, so set to the normal
                                  ;; nexttok procedure, add the "<" to the
                                  ;; verbatim string, and fall out of loop.
                                  (set! nexttok normal-nexttok)
                                  (write-char #\< os))
                                 ((c-slash?)
                                  ;; Got "</", so...
                                  (read-c)
                                  (cond
                                   ((c-eof?)
                                    (display "</" os))
                                   ((c-alpha?)
                                    ;; Got "</" followed by alpha, so unread
                                    ;; the alpha, scan qname, compare...
                                    (unread-c)
                                    (let* ((vqname (scan-qname #t))
                                           (ns     (car vqname))
                                           (local  (cdr vqname)))
                                      ;; Note: We ignore XML namespace
                                      ;; qualifier for purposes of comparison.
                                      ;;
                                      ;; Note: We're interning strings here for
                                      ;; comparison when in theory there could
                                      ;; be many such unique interned strings
                                      ;; in a valid HTML document, although in
                                      ;; practice this should not be a problem.
                                      (if (and local
                                               (eqv? (string->symbol
                                                      (%htmlprag:down
                                                       local))
                                                     elem-name))
                                          ;; This is the terminator tag, so
                                          ;; scan to the end of it, set the
                                          ;; nexttok, and fall out of the loop.
                                          (begin
                                            (let scan-to-end ()
                                              (read-c)
                                              (cond ((c-eof?) #f)
                                                    ((c-gt?)  #f)
                                                    ((c-lt?)  (unread-c))
                                                    ((c-alpha?)
                                                     (unread-c)
                                                     ;; Note: This is an
                                                     ;; expensive way to skip
                                                     ;; over an attribute, but
                                                     ;; in practice more
                                                     ;; verbatim end tags will
                                                     ;; not have attributes.
                                                     (scan-attr)
                                                     (scan-to-end))
                                                    (else (scan-to-end))))
                                            (set! nexttok
                                                  (lambda ()
                                                    (set! nexttok
                                                          normal-nexttok)
                                                    (make-end-token
                                                     elem-name #f '()))))
                                          ;; This isn't the terminator tag, so
                                          ;; add to the verbatim string the
                                          ;; "</" and the characters of what we
                                          ;; were scanning as a qname, and
                                          ;; recurse in the loop.
                                          (begin
                                            (display "</" os)
                                            (if ns
                                                (begin (display ns os)
                                                       (display ":" os)))
                                            (if local
                                                (display local os))
                                            (loop)))))
                                   (else
                                    ;; Got "</" and non-alpha, so unread new
                                    ;; character, add the "</" to verbatim
                                    ;; string, then loop.
                                    (unread-c)
                                    (display "</" os)
                                    (loop))))
                                 (else
                                  ;; Got "<" and non-slash, so unread the new
                                  ;; character, write the "<" to the verbatim
                                  ;; string, then loop.
                                  (unread-c)
                                  (write-char #\< os)
                                  (loop))))
                          (else
                           ;; Got non-"<" in verbatim context, so just add it
                           ;; to the buffer, then, if it's not a linefeed, fall
                           ;; out of the loop so that the token can be
                           ;; returned.
                           (write-char c os)
                           (or (c-lf?) (loop)))))
                  ;; Return the accumulated line string, if non-null, or call
                  ;; nexttok.
                  (or (output-string->string-or-false os) (nexttok))))))
           (nexttok #f))
        (set! nexttok normal-nexttok)
        (lambda () (nexttok))))))

;;! Returns a list of tokens from input port @var{in}, normalizing according to
;; boolean @var{normalized?}.  This is probably most useful as a debugging
;; convenience.  For example:
;;
;; (tokenize-html (open-input-string "<a href=\"foo\">bar</a>") #f)
;; @result{} ((a (@@ (href "foo"))) "bar" (*END* a))
(define (tokenize-html in normalized?)
  (let ((next-tok (make-html-tokenizer in normalized?)))
    (let loop ((tok (next-tok)))
      (if (null? tok)
          '()
          (cons tok (loop (next-tok)))))))

;;! Returns a symbol indicating the kind of tokenizer @var{token}:
;; @code{*COMMENT*}, @code{*DECL*}, @code{*EMPTY*}, @code{*END*},
;; @code{*ENTITY*}, @code{*PI*}, @code{*START*}, @code{*TEXT*}.
;; This is used by higher-level parsing code.  For example:
;;
;; (map shtml-token-kind
;;      (tokenize-html (open-input-string "<a<b>><c</</c") #f))
;; @result{} (*START* *START* *TEXT* *START* *END* *END*)
(define (shtml-token-kind token)
  (cond ((string? token) shtml-text-symbol)
        ((list?   token)
         (let ((s (list-ref token 0)))
           (if (memq s `(,shtml-comment-symbol
                         ,shtml-decl-symbol
                         ,shtml-empty-symbol
                         ,shtml-end-symbol
                         ,shtml-entity-symbol
                         ,shtml-pi-symbol))
               s
               shtml-start-symbol)))
        (else (%htmlprag:error "shtml-token-kind"
                               "unrecognized token kind:"
                               token))))

;;!! Parsing

;; Most applications will call a parser procedure such as
;; @code{html->shtml} rather than calling the tokenizer directly.

;;! List of names of HTML element types that have no content, represented as a
;; list of symbols.  This is used internally by the parser and encoder.  The
;; effect of mutating this list is undefined.
;; TODO: Document exactly which elements these are, after we make the new
;; parameterized parser constructor.
(define %htmlprag:empty-elements
  '(& area base br frame hr img input isindex keygen link meta object param
      spacer wbr))

;;! Emits a parse tree like @code{html->shtml} and related procedures, except
;; using @var{tokenizer} as a source of tokens, rather than tokenizing from an
;; input port.  This procedure is used internally, and generally should not be
;; called directly.
(define parse-html/tokenizer
  ;; TODO: Document the algorithm, then see if rewriting as idiomatic Scheme
  ;; can make it more clear.
  (letrec ((empty-elements
            ;; TODO: Maybe make this an option.  This might also be an
            ;; acceptable way to parse old HTML that uses the `p' element as a
            ;; paragraph terminator.
            %htmlprag:empty-elements)
           (parent-constraints
            ;; TODO: Maybe make this an option.
            '((area     . (map))
              (body     . (html))
              (caption  . (table))
              (colgroup . (table))
              (dd       . (dl))
              (dt       . (dl))
              (frame    . (frameset))
              (head     . (html))
              (isindex  . (head))
              (li       . (dir menu ol ul))
              (meta     . (head))
              (noframes . (frameset))
              (option   . (select))
              (p        . (body td th))
              (param    . (applet))
              (tbody    . (table))
              (td       . (tr))
              (th       . (tr))
              (thead    . (table))
              (title    . (head))
              (tr       . (table tbody thead))))
           (start-tag-name (lambda (tag-token) (car tag-token)))
           (end-tag-name   (lambda (tag-token) (list-ref tag-token 1))))
    (lambda (tokenizer normalized?)
      ;; Example `begs' value:
      ;;
      ;; ( ((head ...) . ( (title ...)                         ))
      ;;   ((html ...) . ( (head  ...) (*COMMENT* ...)         ))
      ;;   (#f         . ( (html  ...) (*DECL*    doctype ...) )) )
      (let ((begs (list (cons #f '()))))
        (letrec ((add-to-current-beg
                  (lambda (tok)
                    (set-cdr! (car begs) (cons tok (cdr (car begs))))))
                 (finish-all-begs
                  (lambda ()
                    (let ((toplist #f))
                      (map (lambda (beg) (set! toplist (finish-beg beg)))
                           begs)
                      toplist)))
                 (finish-beg
                  (lambda (beg)
                    (let ((start-tok (car beg)))
                      (if start-tok
                          (%htmlprag:append!
                           (car beg)
                           (%htmlprag:reverse!ok (cdr beg)))
                          (%htmlprag:reverse!ok (cdr beg))))))
                 (finish-begs-to
                  (lambda (name lst)
                    (let* ((top      (car lst))
                           (starttag (car top)))
                      (cond ((not starttag) #f)
                            ((eqv? name (start-tag-name starttag))
                             (set! begs (cdr lst))
                             (finish-beg top)
                             #t)
                            (else (if (finish-begs-to name (cdr lst))
                                      (begin (finish-beg top) #t)
                                      #f))))))
                 (finish-begs-upto
                  (lambda (parents lst)
                    (let* ((top      (car lst))
                           (starttag (car top)))
                      (cond ((not starttag) #f)
                            ((memq (start-tag-name starttag) parents)
                             (set! begs lst)
                             #t)
                            (else (if (finish-begs-upto parents (cdr lst))
                                      (begin (finish-beg top) #t)
                                      #f)))))))
          (let loop ()
            (let ((tok (tokenizer)))
              (if (null? tok)
                  (finish-all-begs)
                  (let ((kind (shtml-token-kind tok)))
                    (cond ((memv kind `(,shtml-comment-symbol
                                        ,shtml-decl-symbol
                                        ,shtml-entity-symbol
                                        ,shtml-pi-symbol
                                        ,shtml-text-symbol))
                           (add-to-current-beg tok))
                          ((eqv? kind shtml-start-symbol)
                           (let* ((name (start-tag-name tok))
                                  (cell (assq name parent-constraints)))
                             (and cell (finish-begs-upto (cdr cell) begs))
                             (add-to-current-beg tok)
                             (or (memq name empty-elements)
                                 (set! begs (cons (cons tok '()) begs)))))
                          ((eqv? kind shtml-empty-symbol)
                           ;; Empty tag token, so just add it to current
                           ;; beginning while stripping off leading `*EMPTY*'
                           ;; symbol so that the token becomes normal SXML
                           ;; element syntax.
                           (add-to-current-beg (cdr tok)))
                          ((eqv? kind shtml-end-symbol)
                           (let ((name (end-tag-name tok)))
                             (if name
                                 ;; Try to finish to a start tag matching this
                                 ;; end tag.  If none, just drop the token,
                                 ;; though we used to add it to the current
                                 ;; beginning.
                                 (finish-begs-to name begs)
                                 ;; We have an anonymous end tag, so match it
                                 ;; with the most recent beginning.  If no
                                 ;; beginning to match, then just drop the
                                 ;; token, though we used to add it to the
                                 ;; current beginning.
                                 (and (car (car begs))
                                      (begin (finish-beg (car begs))
                                             (set! begs (cdr begs)))))))
                          (else (%htmlprag:error "parse-html/tokenizer"
                                                 "unknown tag kind:"
                                                 kind)))
                    (loop))))))))))

;;! This procedure is now used internally by @code{html->shtml} and its
;; variants, and should not be used directly by programs.  The interface is
;; likely to change in future versions of HtmlPrag.
(define (%htmlprag:parse-html input normalized? top?)
  (let ((parse
         (lambda ()
           (parse-html/tokenizer
            (make-html-tokenizer
             (cond ((input-port? input) input)
                   ((string?     input) (open-input-string input))
                   (else (%htmlprag:error
                          "%htmlprag:parse-html"
                          "invalid input type:"
                          input)))
             normalized?)
            normalized?))))
    (if top?
        (cons shtml-top-symbol (parse))
        (parse))))

;;! Permissively parse HTML from @var{input}, which is either an input port or
;; a string, and emit an SHTML equivalent or approximation.  To borrow and
;; slightly modify an example from [SSAX-HTML]:
;;
;; (html->shtml
;;  "<html><head><title></title><title>whatever</title></head><body>
;; <a href=\"url\">link</a><p align=center><ul compact style=\"aa\">
;; <p>BLah<!-- comment <comment> --> <i> italic <b> bold <tt> ened</i>
;; still &lt; bold </b></body><P> But not done yet...")
;; @result{}
;; (*TOP* (html (head (title) (title "whatever"))
;;              (body "\n"
;;                    (a (@@ (href "url")) "link")
;;                    (p (@@ (align "center"))
;;                       (ul (@@ (compact) (style "aa")) "\n"))
;;                    (p "BLah"
;;                       (*COMMENT* " comment <comment> ")
;;                       " "
;;                       (i " italic " (b " bold " (tt " ened")))
;;                       "\n"
;;                       "still < bold "))
;;              (p " But not done yet...")))
;;
;; Note that in the emitted SHTML the text token @code{"still < bold"} is
;; @emph{not} inside the @code{b} element, which represents an unfortunate
;; failure to emulate all the quirks-handling behavior of some popular Web
;; browsers.
;;
;; The procedures @code{html->sxml-@var{n}nf} for @var{n} 0 through 2
;; correspond to 0th through 2nd normal forms of SXML as specified in [SXML],
;; and indicate the minimal requirements of the emitted SXML.
;;
;; @code{html->sxml} and @code{html->shtml} are currently aliases for
;; @code{html->sxml-0nf}, and can be used in scripts and interactively, when
;; terseness is important and any normal form of SXML would suffice.
(define (html->sxml-0nf input) (%htmlprag:parse-html input #f #t))
(define (html->sxml-1nf input) (%htmlprag:parse-html input #f #t))
(define (html->sxml-2nf input) (%htmlprag:parse-html input #t #t))

(define html->sxml  html->sxml-0nf)
(define html->shtml html->sxml-0nf)


;;-------------------------------------------------------------------------------
;; Emitting HTML

;; Two procedures encoding the SHTML representation as conventional HTML,
;; @code{write-shtml-as-html} and @code{shtml->html}.  These are perhaps most
;; useful for emitting the result of parsed and transformed input HTML.  They
;; can also be used for emitting HTML from generated or handwritten SHTML.

;;! Writes a conventional HTML transliteration of the SHTML @var{shtml} to
;; output port @var{out}.  If @var{out} is not specified, the default is the
;; current output port.  HTML elements of types that are always empty are
;; written using HTML4-compatible XHTML tag syntax.
;;
;; If @var{foreign-filter} is specified, it is a procedure of two argument
;; that is applied to any non-SHTML (``foreign'') object encountered in
;; @var{shtml}, and should yield SHTML.  The first argument is the object, and
;; the second argument is a boolean for whether or not the object is part of
;; an attribute value.
;;
;; No inter-tag whitespace or line breaks not explicit in @var{shtml} is
;; emitted.  The @var{shtml} should normally include a newline at the end of
;; the document.  For example:
;; (write-shtml-as-html
;;  '((html (head (title "My Title"))
;;          (body (@@ (bgcolor "white"))
;;                (h1 "My Heading")
;;                (p "This is a paragraph.")
;;                (p "This is another paragraph.")))))
;; @print{} <html><head><title>My Title</title></head><body bgcolor="whi
;; @print{} te"><h1>My Heading</h1><p>This is a paragraph.</p><p>This is
;; @print{}  another paragraph.</p></body></html>
(define (%htmlprag:write-shtml-as-html/fixed shtml out foreign-filter)
  (letrec
      ((write-shtml-text
        (lambda (str out)
          (let ((len (string-length str)))
            (let loop ((i 0))
              (if (< i len)
                  (begin (display (let ((c (string-ref str i)))
                                    (case c
                                      ;; ((#\") "&quot;")
                                      ((#\&) "&amp;")
                                      ((#\<) "&lt;")
                                      ((#\>) "&gt;")
                                      (else c)))
                                  out)
                         (loop (+ 1 i))))))))
       (write-dquote-ampified
        (lambda (str out)
          ;; TODO: If we emit "&quot;", we really should parse it, and HTML
          ;; 4.01 says we should, but anachronisms in HTML create the potential
          ;; for nasty mutilation of URI in attribute values.
          (let ((len (string-length str)))
            (let loop ((i 0))
              (if (< i len)
                  (begin (display (let ((c (string-ref str i)))
                                    (if (eqv? c #\") "&quot;" c))
                                  out)
                         (loop (+ 1 i))))))))
       (do-thing
        (lambda (thing)
          (cond ((string? thing) (write-shtml-text thing out))
                ((list? thing)   (if (not (null? thing))
                                     (do-list-thing thing)))
                (else (do-thing (foreign-filter thing #f))))))
       (do-list-thing
        (lambda (thing)
          (let ((head (car thing)))
            (cond ((symbol? head)
                   ;; Head is a symbol, so...
                   (cond ((eq? head shtml-comment-symbol)
                          ;; TODO: Make sure the comment text doesn't contain a
                          ;; comment end sequence.
                          (display "<!-- " out)
                          (let ((text (car (cdr thing))))
                            (if (string? text)
                                ;; TODO: Enforce whitespace safety without
                                ;; padding unnecessarily.
                                ;;
                                ;; (let ((len (string-length text)))
                                ;; (if (= len 0)
                                ;; (display #\space out)
                                ;; (begin (if (not (eqv?
                                ;; (string-ref text 0)
                                ;; #\space))
                                (display text out)
                                (%htmlprag:error
                                 "write-shtml-as-html"
                                 "invalid SHTML comment text:"
                                 thing)))
                          (or (null? (cdr (cdr thing)))
                              (%htmlprag:error
                               "write-shtml-as-html"
                               "invalid SHTML comment body:"
                               thing))
                          (display " -->" out))
                         ((eq? head shtml-decl-symbol)
                          (let ((head (car (cdr thing))))
                            (display "<!" out)
                            (display (symbol->string head) out)
                            (for-each
                             (lambda (n)
                               (cond ((symbol? n)
                                      (display #\space out)
                                      (display (symbol->string n) out))
                                     ((string? n)
                                      (display " \"" out)
                                      (write-dquote-ampified n out)
                                      (display #\" out))
                                     (else (%htmlprag:error
                                            "write-shtml-as-html"
                                            "invalid SHTML decl:"
                                            thing))))
                             (cdr (cdr thing)))
                            (display #\> out)))
                         ((eq? head shtml-pi-symbol)
                          (display "<?" out)
                          (display (symbol->string (car (cdr thing))) out)
                          (display #\space out)
                          (display (car (cdr (cdr thing))) out)
                          ;; TODO: Error-check that no more rest of PI.
                          (display "?>" out))
                         ((eq? head shtml-top-symbol)
                          (for-each do-thing (cdr thing)))
                         ((eq? head shtml-empty-symbol)
                          #f)
                         ((eq? head '@)
                          (%htmlprag:error
                           "write-shtml-as-html"
                           "illegal position of SHTML attributes:"
                           thing))
                         ((or (eq? head '&) (eq? head shtml-entity-symbol))
                          (let ((val (shtml-entity-value thing)))
                            (if val
                                (begin (write-char     #\& out)
                                       (if (integer? val)
                                           (write-char #\# out))
                                       (display        val out)
                                       (write-char     #\; out))
                                (%htmlprag:error
                                 "write-shtml-as-html"
                                 "invalid SHTML entity reference:"
                                 thing))))
                         ((memq head `(,shtml-end-symbol
                                       ,shtml-start-symbol
                                       ,shtml-text-symbol))
                          (%htmlprag:error "write-shtml-as-html"
                                           "invalid SHTML symbol:"
                                           head))
                         (else
                          (display #\< out)
                          (display head out)
                          (let* ((rest   (cdr thing)))
                            (if (not (null? rest))
                                (let ((second (car rest)))
                                  (and (list? second)
                                       (not (null? second))
                                       (eq? (car second)
                                            '@)
                                       (begin (for-each do-attr (cdr second))
                                              (set! rest (cdr rest))))))
                            (if (memq head
                                      %htmlprag:empty-elements)
                                ;; TODO: Error-check to make sure the element
                                ;; has no content other than attributes.  We
                                ;; have to test for cases like: (br (@) ()
                                ;; (()))
                                (display " />" out)
                                (begin (display #\> out)
                                       (for-each do-thing rest)
                                       (display "</" out)
                                       (display (symbol->string head) out)
                                       (display #\> out)))))))
                  ;; ((or (list? head) (string? head))
                  ;;
                  ;; Head is a list or string, which might occur as the result
                  ;; of an SXML transform, so we'll cope.
                  (else
                   ;; Head is not a symbol, which might occur as the result of
                   ;; an SXML transform, so we'll cope.
                   (for-each do-thing thing))
                  ;;(else
                  ;; ;; Head is NOT a symbol, list, or string, so error.
                  ;; (%htmlprag:error "write-shtml-as-html"
                  ;;                          "invalid SHTML list:"
                  ;;                          thing))
                  ))))
       (write-attr-val-dquoted
        (lambda (str out)
          (display #\" out)
          (display str out)
          (display #\" out)))
       (write-attr-val-squoted
        (lambda (str out)
          (display #\' out)
          (display str out)
          (display #\' out)))
       (write-attr-val-dquoted-and-amped
        (lambda (str out)
          (display #\" out)
          (write-dquote-ampified str out)
          (display #\" out)))
       (write-attr-val
        (lambda (str out)
          (let ((len (string-length str)))
            (let find-dquote-and-squote ((i 0))
              (if (= i len)
                  (write-attr-val-dquoted str out)
                  (let ((c (string-ref str i)))
                    (cond ((eqv? c #\")
                           (let find-squote ((i (+ 1 i)))
                             (if (= i len)
                                 (write-attr-val-squoted str out)
                                 (if (eqv? (string-ref str i) #\')
                                     (write-attr-val-dquoted-and-amped str
                                                                       out)
                                     (find-squote (+ 1 i))))))
                          ((eqv? c #\')
                           (let find-dquote ((i (+ 1 i)))
                             (if (= i len)
                                 (write-attr-val-dquoted str out)
                                 (if (eqv? (string-ref str i) #\")
                                     (write-attr-val-dquoted-and-amped str
                                                                       out)
                                     (find-dquote (+ 1 i))))))
                          (else (find-dquote-and-squote (+ 1 i))))))))))
       (collect-and-write-attr-val
        ;; TODO: Take another look at this.
        (lambda (lst out)
          (let ((os #f))
            (let do-list ((lst lst))
              (for-each
               (lambda (thing)
                 (let do-thing ((thing thing))
                   (cond ((string? thing)
                          (or os (set! os (open-output-string)))
                          (display thing os))
                         ((list? thing)
                          (do-list thing))
                         ((eq? thing #t)
                          #f)
                         (else
                          (do-thing (foreign-filter thing #t))))))
               lst))
            (if os
                (begin
                  (display #\= out)
                  (write-attr-val (%htmlprag:gosc os) out))))))
       (do-attr
        (lambda (attr)
          (or (list? attr)
              (%htmlprag:error "write-shtml-as-html"
                               "invalid SHTML attribute:"
                               attr))
          (if (not (null? attr))
              (let ((name (car attr)))
                (or (symbol? name)
                    (%htmlprag:error
                     "write-shtml-as-html"
                     "invalid name in SHTML attribute:"
                     attr))
                (if (not (eq? name '@))
                    (begin
                      (display #\space out)
                      (display name    out)
                      (collect-and-write-attr-val (cdr attr) out))))))))
    (do-thing shtml)
    (if #f #f)))

(define write-shtml-as-html
  (letrec ((error-foreign-filter
            (lambda (foreign-object in-attribute-value?)
              (%htmlprag:error
               "write-shtml-as-html"
               (if in-attribute-value?
                   "unhandled foreign object in shtml attribute value:"
                   "unhandled foreign object in shtml:")
               foreign-object))))
    (lambda (shtml . rest)
      (case (length rest)
        ((0) (%htmlprag:write-shtml-as-html/fixed
              shtml
              (current-output-port)
              error-foreign-filter))
        ((1) (%htmlprag:write-shtml-as-html/fixed
              shtml
              (car rest)
              error-foreign-filter))
        ((2) (%htmlprag:write-shtml-as-html/fixed
              shtml
              (car rest)
              (cadr rest)))
        (else
         (%htmlprag:error "write-shtml-as-html"
                          "extraneous arguments:"
                          (cddr rest)))))))

;;! Yields an HTML encoding of SHTML @var{shtml} as a string.  For example:
;;
;; (shtml->html
;;  (html->shtml
;;   "<P>This is<br<b<I>bold </foo>italic</ b > text.</p>"))
;; @result{} "<p>This is<br /><b><i>bold italic</i></b> text.</p>"
;;
;; Note that, since this procedure constructs a string, it should normally
;; only be used when the HTML is relatively small.  When encoding HTML
;; documents of conventional size and larger, @code{write-shtml-as-html} is
;; much more efficient.
(define (shtml->html shtml)
  (let ((os (open-output-string)))
    (write-shtml-as-html shtml os)
    (%htmlprag:gosc os)))

;;!! Deprecated
;; As HtmlPrag evolves towards version 1.0,
;; The equivalences below show the deprecated expressions below, the code on
;; the left is deprecated and should be replaced with the code on the right.

(define sxml->html lshtml->html)
(define write-sxml-html write-shtml-as-html)
