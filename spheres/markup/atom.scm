;;!!! Atom Syndication Format
;; .author Jim Ursetto
;; .link http://wiki.call-cc.org/eggref/4/atom

;; TODO not supported: xml:base, xml:lang due to inheritance
;;   http://www.feedparser.org/docs/resolving-relative-links.html
;; possible solutions, 1) traverse after parse and fixup links,
;;                     2) include fake or real base elt in every descendant (of non-root)
;;                     annotations can be stored in (@ (@ (key value)))

;; TODO: A lot of the constructors check arguments for string? when they should
;;       check for non-empty-string?.  Or change optional-element etc. to consider ""
;;       as #f -- which may break certain text values.

;; TODO: element attribute accessors, such as category-scheme, return #f when missing attribute,
;;       as opposed to text accessors such as rights-text and author-uri, which return "" on
;;       missing element.

(define +atom-egg-version+ "0.1")

(define atom-ns-prefixes
  (make-parameter `((atom03 . "http://purl.org/atom/ns#")
                    (thr . "http://purl.org/syndication/thread/1.0")
                    (xhtml . "http://www.w3.org/1999/xhtml")
                    . ,srl:conventional-ns-prefixes)))

(define (read-atom-doc p)
  (define (drop-preamble doc)
    (drop-while (lambda (x) ;; or (find-tail sxml:element? (cdr doc))
                  (memq (car x) '(*PI* *COMMENT* *ENTITY* @ @@)))
                (cdr doc)))
  (let* ((doc (xlink:xml->sxml p (atom-ns-prefixes)))
         (roots (drop-preamble doc)))
    (cond ((null? roots)
           (atom-error 'read-atom-doc "No root element found"))
          ((null? (cdr roots))
           (if (memq (caar roots) '(atom:feed atom:entry))
               doc
               (atom-error 'read-atom-doc "Root must be atom:feed or atom:entry, found" (caar roots))))
          (else
           (atom-error 'read-atom-doc "Multiple root elements found")))))

;; Single element in doc guaranteed by read-atom-doc, and confirmed to be atom:feed or atom:entry.
(define (atom-doc-root doc)
  (or (element-child doc 'atom:feed)
      (element-child doc 'atom:entry)))

(define (atom-doc-encoding doc)
  "utf-8")                        ; should obtain from *PI* (or auto-detect)

;; element-text concatenates all immediate child text nodes of N.
;; It does not recurse into child nodes.
;; Returns "" even on #f node (such as returned by element-child),
;; because that's what sxml:text did, but it could be wrong.
(define (element-text E)
  (define (text->string t)
    (cond ((null? t) "")       ;; or maybe #f
          ((null? (cdr t)) (car t))
          (else (string-concatenate t))))
  (if E
      (text->string (filter string? (cdr E)))
      ""))

;; Return first child C of element E, or #f if no such child.
;; Passing any node as E other than an <Element> will cause an error.
;;   NB: recommended sxpath equivalent without error checking is
;;   ((if-car-sxpath '((C 1))) N) which is equivalent to sxpath-lolevel's
;;   ((node-reduce (select-kids (ntype?? C)) (node-pos 1)) N))
;;   plus a conditional CAR.  Hey, this isn't Haskell!
(define (element-child e c)
  (and ;; (pair? e)
       (assq c (cdr e))))

(define (element-children e c)
  (filter (lambda (x) (eq? (car x) c))
          (cdr e)))

;; Return an alist of attributes of element E.
(define (element-attributes e)
  (match e
         ((_ ('@ . attrs) . body)
          attrs)
         ((_ . body) '())))

;; Return value associated with KEY in attribute list of element E.
(define (element-attribute-value e key)
  (cond ((assq key (element-attributes e))
         => cadr)
        (else #f)))

;; Return contents of e (minus attributes) as a sexp.
;; Always returns a list.
(define (element-contents e)
  (match e
         ((_ ('@ . attrs) . body)
          body)
         ((_ . body)
          body)))

(define (node-typer type)
  (lambda (N)
    (and (pair? N) (eq? (car N) type))))

(define (node-checker type)
  (lambda (where N)
    (check! where type N)))

(define (check! where id obj)
  (or (and (pair? obj)
           (eq? id (car obj)))
      (atom-error where (string-append "not an " (->string id))
                  obj)))

(define-syntax element
  (syntax-rules () ((_ pred type)
                    (define pred (node-typer 'type)))))
(define-syntax text
  (syntax-rules () ((_ name type)
                    (define (name e)
                      (check! 'name 'type e)
                      (element-text e)))))
(define-syntax child
  (syntax-rules () ((_ name parent kid)
                    (define (name e)
                      (check! 'name 'parent e)
                      (element-child e 'kid)))))
(define-syntax children
  (syntax-rules () ((_ name parent kid)
                    (define (name e)
                      (check! 'name 'parent e)
                      (element-children e 'kid)))))
(define-syntax child-text
  (syntax-rules () ((_ name parent kid)
                    (define (name e)
                      (check! 'name 'parent e)
                      (element-text (element-child e 'kid))))))
(define-syntax attribute
  (syntax-rules () ((_ name type attr)
                    (define (name e)
                      (check! 'name 'type e)
                      (element-attribute-value e 'attr)))))
(define-syntax undefined-content     ;; "undefinedContent" has no meaning according to spec;
  (syntax-rules () ((_ name type)    ;; we just return the SXML contents, if any.
                    (define (name e)
                      (check! 'name 'type e)
                      (element-contents e)))))
(define-syntax derived
  (syntax-rules () ((_ name base id)
                    (define (name x)
                      (check! 'name 'id x)
                      (base x)))))

;;; feeds

(element    feed? atom:feed)
(children   feed-authors atom:feed atom:author)
(child      feed-author atom:feed atom:author)
(children   feed-categories atom:feed atom:category)
(children   feed-contributors atom:feed atom:contributor)
(child      feed-generator atom:feed atom:generator)
(child      feed-icon atom:feed atom:icon)
(child-text feed-id atom:feed atom:id)                     ;; REQUIRED
(children   feed-links atom:feed atom:link)
(child      feed-logo atom:feed atom:logo)
(child      feed-rights atom:feed atom:rights)
(child      feed-subtitle atom:feed atom:subtitle)
(child      feed-title atom:feed atom:title)               ;; REQUIRED
(child-text feed-updated atom:feed atom:updated)
(children   feed-entries atom:feed atom:entry)

;; (In theory, we could look at the feed author if entry authors are missing.
;;  Currently it's left up to the user.  e.g. (or (entry-author E) (feed-author F)))

;;; entries

(element    entry? atom:entry)
(children   entry-authors atom:entry atom:author)
(child      entry-author atom:entry atom:author)
(children   entry-categories atom:entry atom:category)
(children   entry-contributors atom:entry atom:contributor)
(child      entry-content atom:entry atom:content)
(child-text entry-id atom:entry atom:id)
(children   entry-links atom:entry atom:link)
(child-text entry-published atom:entry atom:published)
(child      entry-rights atom:entry atom:rights)
(child      entry-source atom:entry atom:source)
(child      entry-summary atom:entry atom:summary)
(child      entry-title atom:entry atom:title)
(child-text entry-updated atom:entry atom:updated)

;;; atomTextConstruct abstract base class

(define (text-type T)
  (let ((type (element-attribute-value T 'type))) ;; type should always be a string when read from XML
    (if type
        (cond ((string=? type "text") 'text)
              ((string=? type "html") 'html)
              ((string=? type "xhtml") 'xhtml)
              (else (atom-error 'text-type "type attribute must be text, html or xhtml" type T)))
        'text)))

(define (text-text T)   ;; Return concatenated text or HTML contents as a string.
  (element-text T))

(define (text-xhtml T) ;; Return xhtml:div if it is first child of T, or #f.  Ignores other children.
  (let ((c (element-contents T)))
    (and (pair? c) (pair? (car c))
                                        ;(null? (cdr c))            ;; Verify single element
         (eq? 'xhtml:div (caar c))
         (car c))))

;;; atomPersonConstruct abstract base class

(define (person-name P)
  (element-text (element-child P 'atom:name)))

(define (person-uri P)
  (element-text (element-child P 'atom:uri)))

(define (person-email P)
  (element-text (element-child P 'atom:email)))

;;; atom:author, atom:contributor

(element author? atom:author)
(derived author-name  person-name  atom:author)
(derived author-uri   person-uri   atom:author)
(derived author-email person-email atom:author)

(element contributor? atom:contributor)
(derived contributor-name  person-name  atom:contributor)
(derived contributor-uri   person-uri   atom:contributor)
(derived contributor-email person-email atom:contributor)

;;; rights, summary, title, subtitle: atomTextConstruct

(element rights? atom:rights)
(derived rights-type text-type atom:rights)
(derived rights-text text-text atom:rights)
(derived rights-xhtml text-xhtml atom:rights)

(element summary? atom:summary)
(derived summary-type text-type atom:summary)
(derived summary-text text-text atom:summary)
(derived summary-xhtml text-xhtml atom:summary)

(element title? atom:title)
(derived title-type text-type atom:title)
(derived title-text text-text atom:title)
(derived title-xhtml text-xhtml atom:title)

(element subtitle? atom:subtitle)
(derived subtitle-type text-type atom:subtitle)
(derived subtitle-text text-text atom:subtitle)
(derived subtitle-xhtml text-xhtml atom:subtitle)

;;; atom:generator

(element   generator? atom:generator)
(attribute generator-uri atom:generator uri)
(attribute generator-version atom:generator version)
(text      generator-agent atom:generator)

;;; atom:icon, atom:logo

;; Seems like these should return URIs directly in the parent (feed-icon).
;; But they are relative and, theoretically, I might want icon-base (?)

(element icon?    atom:icon)
(text    icon-uri atom:icon)

(element logo?    atom:logo)
(text    logo-uri atom:logo)

;;; atom:link

;; Link relation and mime type could return symbols instead of strings.

(element link? atom:link)
(attribute link-uri atom:link href)                ;; atomUri                  ;; REQUIRED
(attribute %link-relation atom:link rel)           ;; atomNCName | atomUri     ;; default: "alternate"
(attribute link-type atom:link type)               ;; atomMediaType            ;; mime type hint as string
(attribute link-uri-language atom:link hreflang)   ;; atomLanguageTag          ;; RFC3066 language tag
(attribute link-title atom:link title)             ;; text
(attribute %link-length atom:link length)          ;; text (integer # of octets)
(undefined-content link-contents atom:link)

(define (link-relation L)
  (or (%link-relation L) "alternate"))

(define (link-length L)
  (and-let* ((len (%link-length L)))
            (string->number len)))

;; aliases
(define link-href link-uri)
(define link-rel link-relation)

;;; atom:category

(element   category? atom:category)
(attribute category-term   atom:category term)
(attribute category-scheme atom:category scheme)
(attribute category-label  atom:category label)
(undefined-content category-contents atom:category)   ;; no meaning according to spec

;;; atom:source

(element    source? atom:source)
(children   source-authors atom:source atom:author)
(child      source-author atom:source atom:author)
(children   source-categories atom:source atom:category)
(children   source-contributors atom:source atom:contributor)
(child      source-generator atom:source atom:generator)
(child      source-icon atom:source atom:icon)
(child-text source-id atom:source atom:id)
(children   source-links atom:source atom:link)
(child      source-logo atom:source atom:logo)
(child      source-rights atom:source atom:rights)
(child      source-subtitle atom:source atom:subtitle)
(child      source-title atom:source atom:title)
(child-text source-updated atom:source atom:updated)

;;; atom:content


(element   content? atom:content)
(attribute content-source atom:content src)
(attribute %content-type atom:content type)
(derived   content-text text-text atom:content)   ;; use of text-* requires they do not test type attribute
(derived   content-xhtml text-xhtml atom:content)
(derived   content-xml inline-xml-content atom:content)
(derived   content-contents element-contents atom:content)

;; Not an official content type, but suggested by the spec: XML content is
;; usually a single child element used as the root of the XML document.
;; This procedure returns that element if so.  However, if the root has siblings,
;; all nodes are returned wrapped in '(*TOP* ...).  Finally, if no nodes are
;; present, {{#f}} is returned.
(define (inline-xml-content C)
  (let ((c (element-contents C)))
    (and (pair? c) (pair? (car c))
         (if (null? (cdr c))
             (car c)
             (cons '*TOP* c)))))

;; Process the type attribute and return the "kind" of content as a symbol:
;; 'text (text or text/plain), 'html (html or text/html),
;; 'xhtml (html or application/xhtml+xml), 'textual (begins with text/),
;; 'xml (ends with /xml or +xml), or 'binary (anything else).
(define content-kind
  (let ((rx:xml (regexp "[+/]xml$" 'i))
        (rx:text (regexp "^text/" 'i)))
    (lambda (C)
      (let ((src (content-source C)))
        (if src
            'external
            (let ((type (content-type C)))
              (cond ((string=? type "text/plain") 'text)
                    ((string=? type "text/html") 'html)
                    ((string=? type "application/xhtml+xml") 'xhtml)
                    ((or            ;(string=? type "text/xml")
                                        ;(string=? type "application/xml")
                      (string=? type "text/xml-external-parsed-entity")
                      (string=? type "application/xml-external-parsed-entity")
                      (string=? type "application/xml-dtd"))
                     'xml) ; RFC3023 XML media types -- almost certainly pointless
                    ((regexp-search rx:xml type) 'xml) ; text/xml is XML
                    ((regexp-search rx:text type) 'textual)
                    (else 'binary))))))))

;; Return the content MIME media type.  Normalized based on the type attribute;
;; "text", "html" and "xhtml" values are converted into their respective MIME types
;; and a missing value is considered "text/plain".
;; Returns a string; should we return a symbol?
(define (content-type C)
  (let ((type (%content-type C)))
    (cond ((not type) "text/plain")
          ((string=? type "text") "text/plain")
          ((string=? type "html") "text/html")
          ((string=? type "xhtml") "application/xhtml+xml")
          (else type))))

;;; atom:id

(define id? string?)

(define id=? string=?)

(define (entry=? e1 e2)
  (id=? (entry-id e1) (entry-id e2)))

(define (%make-id id) ;; internal -- users use strings
  `(atom:id ,id))

(define (%make-uri uri) ;; internal -- users use strings
  `(atom:uri ,uri))

;;; dateConstruct

(define datetime? string?)

;;; error handling

(define (atom-error where msg . args)
  (raise (list 'exn message: msg arguments: args location: where)))

;;; convenience functions

;; Convenience functions
(define (read-atom-feed p)
  (let ((root (atom-doc-root (read-atom-doc p))))
    (if (feed? root)
        root
        (atom-error 'read-atom-feed "Root element is not an atom:feed"))))

(define (read-atom-entry p)
  (let ((root (atom-doc-root (read-atom-doc p))))
    (if (entry? root)
        root
        (atom-error 'read-atom-feed "Root element is not an atom:entry"))))

;;; Write support

;;;; Keyword handling


(define (required-element where e pred? desc #!optional thunk)
  (if e
      (if (pred? e)
          (list (if thunk (thunk e) e))
          (atom-error where (string-append desc " expected, got") e))
      (atom-error where (string-append desc " required"))))

(define (optional-element where e pred? desc #!optional thunk)
  (if e
      (if (pred? e)
          (list (if thunk (thunk e) e))
          (atom-error where (string-append desc " expected, got") e))
      '()))

(define (required-element-list where L pred? desc)
  (cond ((null? L)
         (atom-error where (string-append desc " list required")))
        ((and (pair? L)
              (every pred? L))
         L)
        (else (atom-error where (string-append desc " list expected, got") L))))

(define (optional-element-list where L pred? desc)
  (cond ((null? L) '())
        ((and (pair? L)
              (every pred? L))
         L)
        (else (atom-error where (string-append desc " list expected, got") L))))

;;;; Documents

(define (make-atom-doc root #!key (declare-xml? #t) (encoding "utf-8") (headers '()))
  ;; We could skip the check for the root element (or use atom-doc-root), which would
  ;; allow us to prepend headers using CONS instead of headers:.
  (unless (or (feed? root) (entry? root))
          (atom-error 'make-atom-doc "root element must be an atom:feed or atom:entry"))
  `(*TOP*
    (@ (*NAMESPACES* . ,(map (match-lambda ((pref . uri) (list pref uri))) (atom-ns-prefixes))))
    ,@(if declare-xml?
          `((*PI* xml ,(string-append
                        "version=\"1.0\""
                        (let ((enc (optional-element 'make-atom-doc encoding
                                                     non-empty-string? "document encoding string"
                                                     (lambda (x) (string-append " encoding=\"" x "\"")))))
                          (if (null? enc) "" (car enc))))))
          '())
    ,@headers
    ,root))

(define (write-atom-doc doc #!optional (port (current-output-port)))
  (unless (atom-doc-root doc)
          (atom-error 'write-atom-doc "not an atom feed or entry document"))
  (srl:sxml->xml doc
                 port
                 ;;ns-prefixes: (atom-ns-prefixes)  ;; necessary?
                 ))

;;;; Feeds

(define (make-feed #!key (authors '()) (categories '()) (contributors '())
                   (generator (make-generator "atom egg for Chicken"
                                              uri: "http://3e8.org/chickadee/atom"
                                              version: +atom-egg-version+))
                   icon id (links '()) logo rights subtitle title
                   updated (entries '()))
  (let ((w 'make-feed))
    (let ((entries (optional-element-list w entries entry? "atom:entry")))
      (cons 'atom:feed
            (append
             (if (every entry-author entries)
                 (optional-element-list w authors author? "atom:author")
                 (required-element-list w authors author? "atom:author"))
             (optional-element-list w categories category? "atom:category")
             (optional-element-list w contributors contributor? "atom:contributor")
             (optional-element      w generator generator? "atom:generator")
             (optional-element      w icon icon? "atom:icon")
             (required-element      w id id? "atom:id string" %make-id)
             (optional-element-list w links link? "atom:link")
             (optional-element      w logo logo? "atom:icon")
             (optional-element      w rights rights? "atom:rights")
             (optional-element      w subtitle subtitle? "atom:subtitle")
             (required-element      w title title? "atom:title")
             (required-element      w updated datetime? "atom:updated datetime string"
                                    (lambda (x) `(atom:updated ,updated)))
             entries)))))

;;;; Entries

(define (make-entry #!key (authors '()) (categories '()) (contributors '())
                    content id (links '()) published rights source
                    summary title updated)
  (let ((w 'make-entry))
    (let ((content-list (optional-element w content content? "atom:content"))) ; needed for summary
      (cons 'atom:entry
            (append
             (optional-element-list w authors author? "atom:author")
             (optional-element-list w categories category? "atom:category")
             (optional-element-list w contributors contributor? "atom:contributor")
             content-list
             (required-element      w id id? "atom:id string" %make-id)
             (optional-element      w published datetime? "atom:published"
                                    (lambda (x) `(atom:published ,x)))
             (optional-element      w rights rights? "atom:rights")
             (optional-element      w source source? "atom:source")
             (required-element      w title title? "atom:title")
             (required-element      w updated datetime? "atom:updated datetime string"
                                    (lambda (x) `(atom:updated ,updated)))
             ;; Summary not required unless atom:content@src or content-kind 'binary.
             (let ((summary-list (optional-element w summary summary? "atom:summary")))
               (if (or (pair? summary-list) (null? content-list))
                   summary-list
                   ;; May be expensive, so tested only when summary is missing.
                   (let ((kind (content-kind content)))
                     (if (or (eq? kind 'external)
                             (eq? kind 'binary))
                         (required-element w summary summary? "atom:summary") ; will always throw an error
                         summary-list)))) ;;phew!

             ;; Links not required unless no content elt., then needs an atom:link @rel="alternate".
             (if content
                 (optional-element-list w links link? "atom:link")
                 (let ((links (optional-element-list w links link? "atom:link"))) ; optional bypasses error
                   (if (any (lambda (x) (string=? (link-relation x) "alternate")) links)
                       links
                       (atom-error w "atom:link with link-relation \"alternate\" required" links)))))))))

;;;; Persons

(define (make-person gi name uri email)
  (let ((w 'make-person))
    `(,gi ,@(required-element w name string? "atom:name string"
                              (lambda (x) `(atom:name ,name)))
          ,@(optional-element w uri string? "atom:uri string" %make-uri)
          ,@(optional-element w email string? "atom:email string"
                              (lambda (x) `(atom:email ,email))))))

(define (make-author #!key name uri email)
  (make-person 'atom:author name uri email))

(define (make-contributor #!key name uri email)
  (make-person 'atom:contributor name uri email))

;;; TextConstructs

;; hmmmmm
;; (make-rights "my rights")         ;; <-- i like this, but see how it compares with atom:content
;; (make-rights "my <b>rights</b>" type: 'html)
;; (make-rights html: "my <b>rights</b>")
;; (make-rights text: "my <b>rights</b>" type: 'html)

(define (make-text-constructor where what)
  (let ((desc (string-append (symbol->string what) " string")))
    (lambda (contents #!key (type 'text))
      (let ((elt
             (cond ((eq? type 'text)
                    (required-element where contents string? desc))
                   ((eq? type 'html)
                    (required-element where contents string? desc))
                   ((eq? type 'xhtml)
                    (required-element where contents
                                      (lambda (x)
                                        (and (pair? x) (eq? (car x) 'xhtml:div)))
                                      "xhtml:div element"))
                   (else
                    (atom-error where "type must be one of 'text, 'html or 'xhtml")))))
        `(,what (@ (type ,(symbol->string type))) ;; easier to always write type out
                ,@elt)))))

(define make-rights   (make-text-constructor 'make-rights 'atom:rights))
(define make-subtitle (make-text-constructor 'make-subtitle 'atom:subtitle))
(define make-summary  (make-text-constructor 'make-summary 'atom:summary))
(define make-title    (make-text-constructor 'make-title 'atom:title))

;;; Generators

(define (make-generator agent #!key uri version)
  (let ((w 'make-generator))
    `(atom:generator ,@(let ((attrs (append
                                     (optional-element w uri string? "generator URI string"
                                                       (lambda (x) `(uri ,x)))
                                     (optional-element w version string? "generator version string"
                                                       (lambda (x) `(version ,x))))))
                         (if (null? attrs)
                             '()
                             `((@ . ,attrs))))
                     ,@(required-element w agent string? "generator agent string"))))

;;; Icons, logos

(define (make-icon uri)
  `(atom:icon ,@(required-element 'make-icon uri string? "icon URI string")))

(define (make-logo uri)
  `(atom:logo ,@(required-element 'make-logo uri string? "logo URI string")))

;;; Categories

;; Not sure whether to make term a regular argument.  That would be like generator,
;; except that term is an attribute. ...
(define (make-category #!key term scheme label) ; term required
  (let ((w 'make-category))
    `(atom:category (@ ,@(required-element w term string? "category term string"
                                           (lambda (x) `(term ,x)))
                       ,@(optional-element w scheme string? "category URI string"
                                           (lambda (x) `(scheme ,x)))
                       ,@(optional-element w label string? "category label string"
                                           (lambda (x) `(label ,x)))))))

;;; Links

(define (non-empty-string? x)
  (and (string? x)
       (not (string=? x ""))))
(define (relation? x)   ;; Don't do anything special for relations.
  (non-empty-string? x))

(define (make-link #!key uri (relation "alternate")
                   type uri-language title length) ;; language?? or uri-language
  (let ((w 'make-link))
    `(atom:link (@ ,@(required-element w uri string? "link uri string"
                                       (lambda (x) `(href ,x)))
                   ,@(optional-element w relation relation? "link relation string"
                                       (lambda (x) `(rel ,x)))
                   ,@(optional-element w type (lambda (x) (or (symbol? x)
                                                         (string? x))) "link media type string or symbol"
                                                         (lambda (x) `(type ,(media-type x))))
                   ,@(optional-element w uri-language non-empty-string? "link uri language"
                                       (lambda (x) `(hreflang ,x)))
                   ,@(optional-element w title string? "link title string"
                                       (lambda (x) `(title ,x)))
                   ,@(optional-element w length exact? "link length (exact integer)"
                                       (lambda (x) `(length ,(number->string x))))))))

;;;; Source

(define (make-source #!key (authors '()) (categories '()) (contributors '()) generator
                     icon id (links '()) logo rights subtitle title updated)
  (let ((w 'make-source))
    (cons 'atom:source
          (append
           (optional-element-list w authors author? "atom:author")
           (optional-element-list w categories category? "atom:category")
           (optional-element-list w contributors contributor? "atom:contributor")
           (optional-element      w generator generator? "atom:generator")
           (optional-element      w icon icon? "atom:icon")
           (optional-element      w id id? "atom:id string" %make-id)
           (optional-element-list w links link? "atom:link")
           (optional-element      w logo logo? "atom:icon")
           (optional-element      w rights rights? "atom:rights")
           (optional-element      w subtitle subtitle? "atom:subtitle")
           (optional-element      w title title? "atom:title")
           (optional-element      w updated datetime? "atom:updated datetime string"
                                  (lambda (x) `(atom:updated ,updated)))))))

;;;; Content

;; Can derive type from media-type.  Maybe they should be the same keyword.
;; If you allow both, they will conflict.

;; (make-content #f src: "http://3e8.org/favico.ico" type: "image/jpeg")
;; (make-content "http://3e8.org/favico.ico" type: 'external media-type: "image/jpeg")
;; (make-content "a87a98a7sd9f8" type: 'binary media-type: "image/jpeg")
;; (make-content "a87a98a7sd9f8" media-type: "image/jpeg")
;; (make-content "a87a98a7sd9f8" type: "image/jpeg")
;; (make-content "abc" type: 'text)
;; (make-content "abc" type: 'textual media-type: "text/x-scheme")
;; (make-content "abc" media-type: "text/x-scheme")
;; (make-content "abc" type: "text/x-scheme")

;; Helper function to convert mime type symbols to mime type strings; strings pass through.
(define (media-type type)
  (cond ((string? type) type)
        ((assq type '((text . "text/plain") (html . "text/html")
                      (xhtml . "application/xhtml+xml")
                      (atom . "application/atom+xml")))
         ;; Could add more MIME type shortcuts here.
         => cdr)
        (else (atom-error 'media-type "type must be 'text, 'html, 'xhtml or a MIME type string"))))

;; Media type testing duplicated here.
(define make-content
  (let ((rx:xml (regexp "[+/]xml$" 'i))
        (rx:text (regexp "^text/" 'i))
        (w 'make-content))
    (lambda (content #!key (type 'text) source)
      (let ((src (optional-element w source string? "content source URI string"))
            (mtype (media-type type)))
        (cond
         ((pair? src)
          `(atom:content (@ (type ,mtype) (src ,(car src)))))
         ((or (equal? mtype "text/plain"))
          `(atom:content (@ (type "text"))
                         ,@(required-element w content string? "content text string")))
         ((or (equal? mtype "text/html"))
          `(atom:content (@ (type "html"))
                         ,@(required-element w content string? "content HTML string")))
         ((or (equal? mtype "application/xhtml+xml"))
          `(atom:content (@ (type "xhtml"))
                         ,@(required-element w content (lambda (x) (and (pair? x)
                                                                   (eq? (car x) 'xhtml:div)))
                                             "xhtml:div element")))
         ((or (equal? mtype "text/xml-external-parsed-entity")
              (equal? mtype "application/xml-external-parsed-entity")
              (equal? mtype "application/xml-dtd")
              (regexp-search rx:xml mtype))
          ;; Tricky.  If root node is *TOP*, strip out *TOP* and splice the child
          ;; nodes in.  Otherwise, just include the (single) root node.  This lets us
          ;; distinguish between a single node (common) and a node list (rare).
          (let ((doc (car (required-element w content (lambda (x) (or (pair? x) (null? x)))
                                            "SXML document"))))
            `(atom:content (@ (type ,mtype))
                           ,@(cond ((null? doc) doc)
                                   ((eq? '*TOP* (car doc)) (cdr doc))
                                   (else (list doc))))))
         ((regexp-search rx:text mtype)
          `(atom:content (@ (type ,mtype))
                         ,@(required-element w content string? "content textual string")))
         ((non-empty-string? mtype)
          `(atom:content (@ (type ,mtype))
                         ,@(required-element w content string? "encoded binary content string")))
         (else (atom-error w "invalid type" mtype)))))))
