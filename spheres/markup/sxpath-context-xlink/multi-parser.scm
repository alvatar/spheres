;;!!! SSAX multi parser
;; Provides ID-index creation, SXML parent pointers and XLink grammar parsing
;;
;; This software is in Public Domain.
;; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;;
;; Please send bug reports and comments to:
;;   lisovsky@acm.org      Kirill Lisovsky
;;   lizorkin@hotbox.ru    Dmitry Lizorkin
;;
;; Primary features:
;;             '()
;;             '(parent)
;;             '(id)
;;             '(parent id)
;;             '(id xlink)
;;             '(parent id xlink)
;;
;; Parent seed
;; Parent-related part of the seed
;;  It is a list of one element:
;;      a function of no arguments which returns a pointer to element's parent
;;      or '*TOP-PTR* symbol for a root SXML element
;; Duuring an element construction it may be just a pointer to parents head,
;; because a parent itself may be under construction at the moment.
;;
;; A seed
;;  seed = (list  original-seed  parent:seed  id:seed  xlink:seed)
;;  original-seed - the seed of the original 'SSAX:XML->SXML' function. It
;; contains an SXML tree being constructed.
;;  parent:seed - parent-related part
;;  id:seed - id-related part
;;  xlink:seed - xlink-related part


;;! This is a multi parser constructor function
;;  parent, id, xlink - boolean parameters. #t means that we construct the
;; corresponding feature, #f - otherwise
;;  ns - for future development. Is not used anywhere in the function
(define (xlink:multi-parser . req-features)
  (define (cons* a1 a2 . rest)
    (if (null? rest)
        (cons a1 a2)
        (cons a1 (apply cons* (cons a2 rest)))))
  ;; This function is called by the NEW-LEVEL-SEED handler
  ;;  elem-name = (if(symbol? elem-gi) elem-gi (xlink:RES-NAME->SXML elem-gi)
  ;; A new 'parent:seed' is returned
  (define (multi-parser:parent:new-level-seed-handler elem-name)
    (let
        ((head (list elem-name)))
      (list (lambda () head))))
  ;; A function which constructs an element from its attributes, children
  ;; and delayed parent information
  ;;  parent:seed - contains a delayed pointer to element's parent
  ;;  attrs - element's attributes
  ;;  children - a list of child elements
  (define (multi-parser:parent:construct-element parent:parent-seed
                                                 parent:seed
                                                 attrs
                                                 children)
    (define (cons* a1 a2 . rest)
      (if (null? rest)
          (cons a1 a2)
          (cons a1 (apply cons* (cons a2 rest)))))
    ;; car gets the only element of parent seed - a pointer to a parent
    (let((parent-ptr (car parent:parent-seed))
         (head ((car parent:seed))))
      (set-cdr!
       head
       (cons* (cons '@ attrs)
              `(@@ (*PARENT* ,parent-ptr))
              children))
      head))
  ;; Seed constructor. #f seeds will be omitted.
  (define (make-seed . seeds)
    (let rpt
        ((s (cdr seeds)) (rzt (list (car seeds))))
      (cond
       ((null? s) (reverse rzt))
       ((car s) (rpt (cdr s)
                     (cons (car s) rzt)))
       (else (rpt (cdr s) rzt)))))
  ;; Accessors
  (define get-sxml-seed car)
  (define (bad-accessor type)
    (lambda x
      (display (string-append "SSAX multi-parser: FATAL ERROR!!!  -> " type "\n" x "\n")
               (current-error-port))
      (exit -1)))
  (let ((ns-assig '())
        (with-parent?  (memq 'parent req-features))
        (with-id?      (memq 'id req-features))
        (with-xlink?   (memq 'xlink req-features)))
    (call-with-values
        (lambda () (values
               (if with-parent?
                   cadr (bad-accessor 'par))
               (if with-id?
                   (if with-parent? caddr cadr)
                   (bad-accessor 'id))
               (if with-xlink?
                   (cond
                    ((and with-parent? with-id?)
                     cadddr)
                    ((or with-parent? with-id?)
                     caddr)
                    (else cadr))
                   (bad-accessor 'xlink))))
      (lambda (get-pptr-seed get-id-seed get-xlink-seed)
        (let ((initial-seed     ; Initial values for specialized seeds
               (make-seed
                '()
                (and with-parent? (list '*TOP-PTR*))
                (and with-id? (id:make-seed '() '()))
                (and with-xlink?
                     (xlink:make-small-seed 'general '() '(1) '())))))
          ;; Making a special function, which, if applyed to the final seed,
          ;; will construct a document
          (letrec
              ((ending-actions
                (cond
                 ((not (or with-id? with-xlink?))
                  (lambda (seed)
                    (let ((result (reverse (get-sxml-seed seed))))
                      (cons '*TOP* result))))
                 ((and with-id? (not with-xlink?)) ; with-id?
                  (lambda (seed)
                    (let((result (reverse (get-sxml-seed seed)))
                         (aux (list (id:ending-action (get-id-seed seed)))))
                      (cons* '*TOP*
                             (cons '@@ aux)
                             result))))
                 ((and with-id? with-xlink?) ; with-id, with-xlink
                  (lambda (seed)
                    (let((result (reverse (get-sxml-seed seed)))
                         (aux (list (xlink:ending-action (get-xlink-seed seed))
                                    (id:ending-action (get-id-seed seed)))))
                      (cons* '*TOP*
                             (cons '@@ aux)
                             result))))
                 (else
                  (display (string-append "ending-actions NIY: " with-parent? with-id? with-xlink? "\n")
                           (current-error-port))
                  (exit))))
               ;;------------------------------------
               ;; Some handlers
               ;; A special function
               ;; When given an input port, it becomes a handler for a NEW-LEVEL-SEED
               (new-level-seed-handler
                (cond
                 ((not (or with-parent? with-id? with-xlink?))
                  (lambda(port)
                    (lambda (elem-gi attributes namespaces expected-content seed)
                      (list '()))))
                 ((and with-parent? (not (or with-id? with-xlink?))) ; with-parent
                  (lambda(port)
                    (lambda (elem-gi attributes namespaces expected-content seed)
                      (make-seed
                       '()
                       (and with-parent?
                            (multi-parser:parent:new-level-seed-handler
                             (if (symbol? elem-gi)
                                 elem-gi
                                 (xlink:RES-NAME->SXML elem-gi))))
                       ))))
                 ((and with-id? (not (or with-parent? with-xlink?))) ; with-id
                  (lambda(port)
                    (lambda (elem-gi attributes namespaces expected-content seed)
                      (list             ; make-seed
                       '()
                       (id:new-level-seed-handler (get-id-seed seed))))))
                 ((and with-parent? with-id? (not with-xlink?)) ; parent, id
                  (lambda(port)
                    (lambda (elem-gi attributes namespaces expected-content seed)
                      (list             ; make-seed
                       '()
                       (multi-parser:parent:new-level-seed-handler
                        (if(symbol? elem-gi) elem-gi (xlink:RES-NAME->SXML elem-gi)))
                       (id:new-level-seed-handler (get-id-seed seed))))))
                 ((and with-id? with-xlink? (not with-parent?)) ; id, xlink
                  (lambda(port)
                    (lambda (elem-gi attributes namespaces expected-content seed)
                      (list             ; make-seed
                       '()
                       (id:new-level-seed-handler (get-id-seed seed))
                       (xlink:new-level-seed-handler
                        port attributes namespaces (get-xlink-seed seed))))))
                 ((and with-parent? with-id? with-xlink?) ; parent, id, xlink
                  (lambda(port)
                    (lambda (elem-gi attributes namespaces expected-content seed)
                      (list             ; make-seed
                       '()
                       (multi-parser:parent:new-level-seed-handler
                        (if(symbol? elem-gi) elem-gi (xlink:RES-NAME->SXML elem-gi)))
                       (id:new-level-seed-handler (get-id-seed seed))
                       (xlink:new-level-seed-handler
                        port attributes namespaces (get-xlink-seed seed))))))
                 (else (display (string-append "new-level NIY: " with-parent? with-id? with-xlink? "\n")
                                (current-error-port))
                       (exit))))
               ;; A special handler function for a FINISH-ELEMENT
               (finish-element-handler
                (cond
                 ((not (or with-parent? with-id? with-xlink?))
                  (lambda (elem-gi attributes namespaces parent-seed seed)
                    (let ((children (xlink:reverse-collect-str-drop-ws (get-sxml-seed seed)))
                          (attrs
                           (xlink:attlist-fold
                            (lambda (attr accum)
                              (cons (list
                                     (if (symbol? (car attr)) (car attr)
                                         (xlink:RES-NAME->SXML (car attr)))
                                     (cdr attr)) accum))
                            '() attributes)))
                      (list             ; make-seed
                       (cons
                        (cons
                         (if (symbol? elem-gi) elem-gi
                             (xlink:RES-NAME->SXML elem-gi))
                         (if (null? attrs) children
                             (cons (cons '@ attrs) children)))
                        (get-sxml-seed parent-seed))))))
                 ((and with-parent? (not (or with-id? with-xlink?))) ; parent
                  (lambda (elem-gi attributes namespaces parent-seed seed)
                    (let((children (xlink:reverse-collect-str-drop-ws (get-sxml-seed seed)))
                         (attrs
                          (xlink:attlist-fold
                           (lambda (attr accum)
                             (cons (list
                                    (if (symbol? (car attr)) (car attr)
                                        (xlink:RES-NAME->SXML (car attr)))
                                    (cdr attr)) accum))
                           '() attributes)))
                      (list             ; make-seed
                       (cons
                        (multi-parser:parent:construct-element
                         (get-pptr-seed parent-seed)
                         (get-pptr-seed seed)
                         attrs children)
                        (get-sxml-seed parent-seed))
                                        ; pptr- seed from parent seed is not modified:
                       (get-pptr-seed parent-seed)
                       ))))
                 ((and with-id? (not (or with-parent? with-xlink?))) ; id
                  (lambda (elem-gi attributes namespaces parent-seed seed)
                    (let((children (xlink:reverse-collect-str-drop-ws (get-sxml-seed seed)))
                         (attrs
                          (xlink:attlist-fold
                           (lambda (attr accum)
                             (cons (list
                                    (if (symbol? (car attr)) (car attr)
                                        (xlink:RES-NAME->SXML (car attr)))
                                    (cdr attr)) accum))
                           '() attributes)))
                      (let((element
                               (cons
                                (if(symbol? elem-gi)
                                   elem-gi
                                   (xlink:RES-NAME->SXML elem-gi))
                                (if(null? attrs)
                                   children
                                   (cons (cons '@ attrs) children)))))
                        (list           ; make-seed
                         (cons element (get-sxml-seed parent-seed))
                         (id:finish-element-handler
                          elem-gi attributes (get-id-seed seed) element))))))
                 ((and with-parent? with-id? (not with-xlink?)) ; parent, id
                  (lambda (elem-gi attributes namespaces parent-seed seed)
                    (let((children (xlink:reverse-collect-str-drop-ws (get-sxml-seed seed)))
                         (attrs
                          (xlink:attlist-fold
                           (lambda (attr accum)
                             (cons (list
                                    (if (symbol? (car attr)) (car attr)
                                        (xlink:RES-NAME->SXML (car attr)))
                                    (cdr attr)) accum))
                           '() attributes)))
                      (let((element
                               (multi-parser:parent:construct-element
                                (get-pptr-seed parent-seed) (get-pptr-seed seed)
                                attrs children)))
                        (list           ; make-seed
                         (cons element (get-sxml-seed parent-seed))
                                        ; pptr- seed from parent seed is not modified:
                         (get-pptr-seed parent-seed)
                         (id:finish-element-handler
                          elem-gi attributes (get-id-seed seed) element))))))
                 ((and with-id? with-xlink? (not with-parent?)) ; id, xlink
                  (lambda (elem-gi attributes namespaces parent-seed seed)
                    (let((children (xlink:reverse-collect-str-drop-ws (get-sxml-seed seed)))
                         (attrs
                          (xlink:attlist-fold
                           (lambda (attr accum)
                             (cons (list
                                    (if (symbol? (car attr)) (car attr)
                                        (xlink:RES-NAME->SXML (car attr)))
                                    (cdr attr)) accum))
                           '() attributes)))
                      (let((element
                               (cons
                                (if(symbol? elem-gi)
                                   elem-gi
                                   (xlink:RES-NAME->SXML elem-gi))
                                (if(null? attrs)
                                   children
                                   (cons (cons '@ attrs) children)))))
                        (list           ; make-seed
                         (cons element (get-sxml-seed parent-seed))
                         (id:finish-element-handler
                          elem-gi attributes (get-id-seed seed) element)
                         (xlink:finish-element-handler
                          (get-xlink-seed parent-seed)
                          (get-xlink-seed seed) element))))))
                 ((and with-parent? with-id? with-xlink?) ; parent, id, xlink
                  (lambda (elem-gi attributes namespaces parent-seed seed)
                    (let((children (xlink:reverse-collect-str-drop-ws (get-sxml-seed seed)))
                         (attrs
                          (xlink:attlist-fold
                           (lambda (attr accum)
                             (cons (list
                                    (if (symbol? (car attr)) (car attr)
                                        (xlink:RES-NAME->SXML (car attr)))
                                    (cdr attr)) accum))
                           '() attributes)))
                      (let((element
                               (multi-parser:parent:construct-element
                                (get-pptr-seed parent-seed) (get-pptr-seed seed)
                                attrs children)))
                        (list           ; make-seed
                         (cons element (get-sxml-seed parent-seed))
                                        ; pptr- seed from parent seed is not modified:
                         (get-pptr-seed parent-seed)
                         (id:finish-element-handler
                          elem-gi attributes (get-id-seed seed) element)
                         (xlink:finish-element-handler
                          (get-xlink-seed parent-seed)
                          (get-xlink-seed seed) element))))))
                 (else (display "finish-element: NIY\n" (current-error-port))
                       (exit))))
               ;; A special function
               ;; Given 'namespaces', it becomes a handler for a DOCTYPE
               (doctype-handler
                (if
                 (not with-id?)
                 (lambda (namespaces)
                   (lambda (port docname systemid internal-subset? seed)
                     (when internal-subset?
                           (xlink:warn port
                                       "Internal DTD subset is not currently handled ")
                           (xlink:skip-internal-dtd port))
                     (xlink:warn port "DOCTYPE DECL " docname " "
                                 systemid " found and skipped")
                     (values #f '() namespaces seed)))
                 (cond
                  ((not (or with-parent? with-xlink?)) ; with-id
                   (lambda (namespaces)
                     (lambda (port docname systemid internal-subset? seed)
                       (values
                        #f '() namespaces
                        (list           ; make-seed
                         (get-sxml-seed seed)
                         (id:doctype-handler port systemid internal-subset?))))))
                  ((and with-parent? (not with-xlink?)) ; with-parent, with-id
                   (lambda (namespaces)
                     (lambda (port docname systemid internal-subset? seed)
                       (values
                        #f '() namespaces
                        (list           ; make-seed
                         (get-sxml-seed seed)
                         (get-pptr-seed seed)
                         (id:doctype-handler port systemid internal-subset?))))))
                  ((and (not with-parent?) with-xlink?) ; with-id, with-xlink
                   (lambda (namespaces)
                     (lambda (port docname systemid internal-subset? seed)
                       (values
                        #f '() namespaces
                        (list           ; make-seed
                         (get-sxml-seed seed)
                         (id:doctype-handler port systemid internal-subset?)
                         (get-xlink-seed seed))))))
                  (else             ; with-parent, with-id, with-xlink
                   (lambda (namespaces)
                     (lambda (port docname systemid internal-subset? seed)
                       (values
                        #f '() namespaces
                        (list           ; make-seed
                         (get-sxml-seed seed)
                         (get-pptr-seed seed)
                         (id:doctype-handler port systemid internal-subset?)
                         (get-xlink-seed seed)))))))))) ; end of letrec
            ;; Constructing a special parser function
            (lambda (port)
              (let ((namespaces
                     (map (lambda (el)
                            (cons* #f (car el) (xlink:uri-string->symbol (cdr el))))
                          ns-assig)))
                (ending-actions
                 ((xlink:make-parser
                   NEW-LEVEL-SEED
                   (new-level-seed-handler port)
                   FINISH-ELEMENT
                   finish-element-handler
                   CHAR-DATA-HANDLER
                   (lambda (string1 string2 seed)
                     (cons
                      (if(string-null? string2)
                         (cons string1 (car seed))
                         (cons* string2 string1 (car seed)))
                      (cdr seed)))
                   DOCTYPE
                   (doctype-handler namespaces)
                   UNDECL-ROOT
                   (lambda (elem-gi seed)
                     (values #f '() namespaces seed))
                   PI
                   ((*DEFAULT* . (lambda (port pi-tag seed)
                                   (cons
                                    (cons
                                     (list '*PI* pi-tag
                                           (xlink:read-pi-body-as-string port))
                                     (car seed))
                                    (cdr seed))))))
                  port
                  initial-seed))))))))))
