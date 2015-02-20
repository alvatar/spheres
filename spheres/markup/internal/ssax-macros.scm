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
                     (xlink:warn port "Skipping PI: " target "\n")
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
