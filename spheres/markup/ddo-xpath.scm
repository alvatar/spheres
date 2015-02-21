;;!!! DDO SXPath
;;
;; The optimized SXPath that implements distinct document order (DDO) of the
;; nodeset produced.
;;
;; Unlike conventional SXPath and SXPath with context, DDO SXPath guarantees that
;; the execution time is at worst polynomial of the XPath expression size and of
;; the SXML document size.
;;
;; The API of DDO SXPath is compatible of that in conventional SXPath. The main
;; following kinds of optimization methods are designed and implemented in DDO
;; SXPath:
;;
;; - All XPath axes are implemented to keep a nodeset in distinct document
;;   order (DDO). An axis can now be considered as a converter:
;;    nodeset_in_DDO --> nodeset_in_DDO
;; - Type inference for XPath expressions allows determining whether a
;;   predicate involves context-position implicitly;
;; - Faster evaluation for particular kinds of XPath predicates that involve
;;   context-position, like:  [position() > number] or [number];
;; - Sort-merge join algorithm implemented for XPath EqualityComparison of
;;   two nodesets;
;; - Deeply nested XPath predicates are evaluated at the very beginning of the
;;   evaluation phase, to guarantee that evaluation of deeply nested predicates
;;   is performed no more than once for each combination of
;;   (context-node, context-position, context-size)

;; Please send bug reports and comments to:
;;   lizorkin@hotbox.ru    Dmitry Lizorkin
;;
;; The implementation of axes is based on the idea of context


(cond-expand
 (gambit (declare (fixnum)))
 (else (void)))


(define (ddo:filter proc lst)
  (let recur ((lst lst))
    (if (null? lst)
        '()
        (let ((head (car lst)))
          (if (proc head)
              (cons head (recur (cdr lst)))
              (recur (cdr lst)))))))

;;-------------------------------------------------------------------------------
;;!! Miscellaneous helpers

;;! Returns the last member of the lst
;; lst is expected to be non-empty
(define (ddo:list-last lst)
  (if (null? (cdr lst))
      (car lst)
      (ddo:list-last (cdr lst))))

;;! Selects all attribute and child nodes of a given 'node'
;; Node is not supposed to be a context
(define (ddo:attr-child node)
  (cond
   ((or (not (pair? node))                           ; Leaf node
        (null? (cdr node))                           ; empty element
        (memq (car node) '(*PI* *COMMENT* *ENTITY*)) ; PI, Comment or Entity
        )                                            ; => no children
    '())
   ((and (pair? (cadr node))
         (eq? '@ (caadr node)))
                                        ;; attribute node presented
    (append (cdadr node)                ; attributes
            (ddo:filter sxml:node? (cddr node))))
   (else                                ; no attributes
    (ddo:filter sxml:node? (cdr node)))))

;;! For a given node, returns its attribute nodes and attribute value nodes in
;; document order
;; Node is not supposed to be a context
(define (ddo:attrs-and-values node)
  (apply append
         (map                           ; attribute and its content
          (lambda (a) (cons a (cdr a)))
          (sxml:attr-list node))))

;;! Removes those members of the input 'nodeset' that are attributes or
;; attribute values of a given 'node'. Nodeset is supposed to be in distinct
;; document order. The order of attribute nodes in the 'nodeset' is supposed
;; to be the same as in the original SXML document
;; Works for ordinary nodes are well as for contexts
(define (ddo:discard-attributes node nodeset)
  (let loop ((attrs (ddo:attrs-and-values (sxml:context->node node)))
             (nset nodeset))
    (if (or (null? attrs) (null? nset))
        nset
        (loop (cdr attrs)
              (if (eq? (car attrs) (sxml:context->node (car nset)))
                  (cdr nset) nset)))))


;;-------------------------------------------------------------------------------
;;!! XPath axes for location steps not involving position-based predicates
;; In this section, all axes expect the argument node-set in distinct document
;; order, and return the result in distinct document order

;;! Ancestor axis
;; In general, every two nodes have have some common ancestors (at least the
;; root of the document). When we obtain ancestors of the context node, only
;; those of them must be added to the result which are different from the
;; ancestors of the previous node in the input node-set
(define (ddo:ancestor test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (prev-ancestors '())
                 (res '()))
        (if
         (null? src)                    ; everyone processed
         (reverse res)
         (let ((curr (car src)))
           (if
            (sxml:context? curr)
            (let rpt ((curr-ancs (reverse
                                  (sxml:context->ancestors-u curr)))
                      (dupl '()))
              (cond
               ((null? curr-ancs)       ; no new ancestors
                (loop (cdr src) prev-ancestors res))
               ((memq (car curr-ancs) prev-ancestors)
                                        ; member of the ancestorial chain
                (rpt (cdr curr-ancs)
                     (cons (car curr-ancs) dupl)))
               (else   ; the first different ancestor in a chain found
                (let creat ((new-ancestors dupl)
                            (curr-ancs curr-ancs)
                            (res res))
                  (cond
                   ((null? curr-ancs)   ; everyone processed
                    (loop (cdr src)
                          new-ancestors
                          res))
                   ((test-pred? (car curr-ancs))
                                        ; add to the result
                    (creat (cons (car curr-ancs) new-ancestors)
                           (cdr curr-ancs)
                           (cons
                            (draft:smart-make-context
                             (car curr-ancs)
                             new-ancestors
                             num-anc)
                            res)))
                   (else     ; this node doesn't satisfy the node test
                    (creat (cons (car curr-ancs) new-ancestors)
                           (cdr curr-ancs)
                           res)))))))
            ;; no ancestors for this node
            (loop (cdr src) prev-ancestors res))))))))

;;! Ancestor-or-self axis
;; See the comment for ddo:ancestor
(define (ddo:ancestor-or-self test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (prev-ancestors '())
                 (res '()))
        (if
         (null? src)                    ; everyone processed
         (reverse res)
         (let rpt ((curr-ancs (reverse
                               (sxml:context->content (car src))))
                   (dupl '()))
           (cond
            ((null? curr-ancs)          ; no new ancestors
             (loop (cdr src) prev-ancestors res))
            ((memq (car curr-ancs) prev-ancestors)
                                        ; member of the ancestorial chain
             (rpt (cdr curr-ancs)
                  (cons (car curr-ancs) dupl)))
            (else      ; the first different ancestor in a chain found
             (let creat ((new-ancestors dupl)
                         (curr-ancs curr-ancs)
                         (res res))
               (cond
                ((null? curr-ancs)      ; everyone processed
                 (loop (cdr src)
                       new-ancestors
                       res))
                ((test-pred? (car curr-ancs))
                                        ; add to the result
                 (creat (cons (car curr-ancs) new-ancestors)
                        (cdr curr-ancs)
                        (cons
                         (draft:smart-make-context
                          (car curr-ancs)
                          new-ancestors
                          num-anc)
                         res)))
                (else        ; this node doesn't satisfy the node test
                 (creat (cons (car curr-ancs) new-ancestors)
                        (cdr curr-ancs)
                        res))))))))))))

;;! Attribute axis
;; The alias for drart:attribute, since no reordering or duplicate elimination
;; is required
(define ddo:attribute draft:attribute)

;;! Child axis
;; If the input node is such that node of the nodes is the descendant of
;; another, draft:child will produce the result in distinct document
;; order
;; In the general case, the implementation for child axis is more
;; complicated, since it must provide the correct ordering
(define (ddo:child test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?)))
    (letrec
        ((child4this
          ;; Arguments
          ;;  curr-node - current node (or context) to process
          ;;  src - the remaining nodes of the input node-set
          ;; Returns: (values res src)
          (lambda (curr-node src)
            (let iter-attrs ((src src)
                             (res '()))
              (cond
               ((null? src)             ; the zoo is completely over
                (values
                 (append res
                         ((draft:child test-pred? num-anc) curr-node))
                 ;; null
                 src))
               ((memq (sxml:context->node (car src))
                      (sxml:attr-list (sxml:context->node curr-node)))
                ;; next in src is the attribute of the curr-node
                (iter-attrs
                 (cdr src)
                 (append res ((draft:child test-pred? num-anc) (car src)))))
               (else                    ; normal behaviour
                (let ((res-ancestors
                       (sxml:context->content curr-node)))
                  (let iter-cands ((res-candidates
                                    (child (sxml:context->node curr-node)))
                                   (src src)
                                   (res res))
                    (cond
                     ((null? src)       ; this zoo is over
                      (values
                       (append
                        res
                        (draft:siblings->context-set
                         ((sxml:filter test-pred?) res-candidates)
                         (draft:list-head res-ancestors num-anc)))
                       ;; always null
                       src))
                     ((null? res-candidates)
                      (values res src))
                     (else      ; processing the current res-candidate
                      (let rpt ((more (list (car res-candidates)))
                                (next (sxml:context->node (car src)))
                                (src src)
                                (res
                                 (if
                                  (test-pred? (car res-candidates))
                                  (append
                                   res
                                   (list
                                    (draft:smart-make-context
                                     (car res-candidates)
                                     res-ancestors num-anc)))
                                  res)))
                        (cond
                         ((null? more)
                          ;; no more src members
                          (iter-cands (cdr res-candidates) src res))
                         ((eq? (car more) next)
                          ;; next node is a descendant-or-self candidate
                          ;; or the attribute of its descendants
                          (call-with-values
                              (lambda () (child4this (car src) (cdr src)))
                            (lambda (add-res new-src)
                              (if
                               (null? new-src)
                               (iter-cands ; will exit there
                                (cdr res-candidates)
                                new-src
                                (append res add-res))
                               (rpt
                                (cdr more) ; kids processed by recursive
                                (sxml:context->node (car new-src))
                                new-src
                                (append res add-res))))))
                         (else
                          (rpt
                           (append (ddo:attr-child (car more))
                                   (cdr more))
                           next src res))))))))))))))
      (lambda (node)                         ; node or nodeset
        (if
         (nodeset? node)
         (let iter ((nset node)
                    (res '()))
           (if
            (null? nset)
            res
            (call-with-values
                (lambda () (child4this (car nset) (cdr nset)))
              (lambda (add-res new-nset)
                (iter new-nset (append res add-res))))))
         ((draft:child test-pred? num-anc) node))))))

;;! Descendant axis
;; We should take into account that one node in the input node set may be the
;; descendant of the other node in the input node-set. Evaluation of descendant
;; axis should not take the former node into account then, since its descendants
;; won't add any new nodes to the result with respects to descendants of the
;; latter node.
;; Note that if the input node is such that node of the nodes is the descendant
;; of another, draft:descendant will produce the result in distinct document
;; order
(define (ddo:descendant test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (sxml:child sxml:node?))
         (desc (draft:descendant test-pred? num-anc)))
    (lambda (node)                           ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (next-node #f)
                 (content-to-scan '())
                 (res '()))
        (if
         (null? content-to-scan)
         (cond
          ((null? src)                  ; everyone processed
           (reverse res))
          ((null? (cdr src)) ; of length 1 => never produces duplicates
           (append (reverse res)
                   (desc (car src))))
          (else
           (loop (cdr src)
                 (sxml:context->node (cadr src))
                 (let ((cntnt (sxml:context->content (car src))))
                   (map
                    (lambda (c) (cons c cntnt))
                    (child (sxml:context->node (car src)))))
                 res)))
         (let ((curr-cntnt (car content-to-scan)))
           (call-with-values
               (lambda ()
                 (if
                  ;; next input node should be removed from consideration
                  (eq? (car curr-cntnt) next-node)
                  (values
                   (cdr src)
                   (if (null? (cdr src)) ; no next node
                       #f
                       (sxml:context->node (cadr src))))
                  (values src next-node)))
             (lambda (new-src new-next)
               (loop new-src
                     new-next
                     (append
                      (map
                       (lambda (c) (cons c curr-cntnt))
                       (child (car curr-cntnt)))
                      (cdr content-to-scan))
                     (if
                      (test-pred? (car curr-cntnt)) ; satisfies the node test
                      (cons
                       (draft:smart-make-context
                        (car curr-cntnt) (cdr curr-cntnt) num-anc)
                       res)
                      res))))))))))

;;! Descendant-or-self axis
;; See the comment for ddo:descendant
;; Note that if the input node is such that node of the nodes is the descendant
;; of another, draft:descendant-or-self will produce the result in distinct
;; document order
(define (ddo:descendant-or-self test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (sxml:child sxml:node?))
         (desc-os (draft:descendant-or-self test-pred? num-anc)))
    (lambda (node)                           ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (next-node #f)
                 (content-to-scan '())
                 (res '()))
        (if
         (null? content-to-scan)
         (cond
          ((null? src)                  ; everyone processed
           (reverse res))
          ((null? (cdr src)) ; of length 1 => never produces duplicates
           (append (reverse res)
                   (desc-os (car src))))
          (else
           (loop (cdr src)
                 (sxml:context->node (cadr src))
                 (list (sxml:context->content (car src)))
                 res)))
         (let ((curr-cntnt (car content-to-scan)))
           (call-with-values
               (lambda ()
                 (if
                  ;; next input node should be removed from consideration
                  (eq? (car curr-cntnt) next-node)
                  (values
                   (cdr src)
                   (if (null? (cdr src)) ; no next node
                       #f
                       (sxml:context->node (cadr src))))
                  (values src next-node)))
             (lambda (new-src new-next)
               (loop new-src
                     new-next
                     (append
                      (map
                       (lambda (c) (cons c curr-cntnt))
                       (child (car curr-cntnt)))
                      (cdr content-to-scan))
                     (if
                      (test-pred? (car curr-cntnt)) ; satisfies the node test
                      (cons
                       (draft:smart-make-context
                        (car curr-cntnt) (cdr curr-cntnt) num-anc)
                       res)
                      res))))))))))

;;! Following axis
;; The implementation exploits the idea expressed in
;; http://pi3.informatik.uni-mannheim.de/publications/TR-02-011.pdf,
;; that is, it is sufficient to calculate following for the first_dmax
;; member of the input nodeset
(define (ddo:following test-pred? . num-ancestors)
  (let ((child (sxml:child sxml:node?))
        (foll (apply draft:following (cons test-pred? num-ancestors))))
    (lambda (node)                           ; node or nodeset
      (cond
       ((null? node)                   ; empty nodeset - nothing to do
        '())
       ((and (pair? node) (not (symbol? (car node)))) ; non-empty nodeset
        (if
         (null? (cdr node))             ; a singleton nodeset
         (foll (car node))
         (let loop ((candidate (car node))
                    (next (sxml:context->node (cadr node)))
                    (more (cdr node))
                    (descendants (list (sxml:context->node (car node)))))
           (cond
            ((null? descendants)
             ;; none of the node-set members are descendants of the candidate
             ;; => apply following axis
             (foll candidate))
            ((eq? (car descendants) next)
             ;; the next node is the new candidate
             (if (null? (cdr more)) ; the next node is the final candidate
                 (foll (car more))
                 (loop (car more)
                       (sxml:context->node (cadr more))
                       (cdr more)
                       (list next))))
            ((memq next (ddo:attrs-and-values (car descendants)))
             ;; next node in src is an attribute node or attribute value node
             (foll (car more)))
            (else                       ; proceed deeper in a tree
             (loop candidate next more
                   (append (child (car descendants)) (cdr descendants))))))))
       (else                            ; a single node
        (foll node))))))

;;! Following-sibling axis
(define (ddo:following-sibling test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?))
        (all-following-siblings
         (lambda (node)                      ; not a nodeset
           (if
            (and (sxml:context? node)
                 (not (null? (sxml:context->ancestors-u node))))
            (cond
             ((memq (sxml:context->node-u node)
                    (cdr ; parent is an element => cdr gives its children
                     (car (sxml:context->ancestors-u node))))
              => (lambda (x) x))
             (else                      ; no following siblings
              '()))
            ;; no parent => no siblings
            '()))))
    (letrec
        ((reordering
          ;; Arguments
          ;;  res-candidates = (listof node) - candidates for the result, not
          ;; yet filtered with a node test
          ;;  res-ancestors - ancestors of res-candidates
          ;;  src - the remaining nodes of the input node-set
          ;; Returns: (values res src)
          (lambda (res-candidates res-ancestors src)
            (let loop ((res-candidates res-candidates)
                       (src src)
                       (res '())
                       (nonself? #f))
              (cond
               ((null? res-candidates)
                (values res src))
               ((null? src)             ; this zoo is over
                (values
                 (append
                  res
                  (draft:siblings->context-set
                   ((sxml:filter test-pred?)
                    (if nonself?
                        res-candidates
                        (cdr res-candidates)))
                   (draft:list-head res-ancestors num-anc)))
                 src                    ; always null
                 ))
               ((eq? (car res-candidates) (sxml:context->node (car src)))
                (loop res-candidates (cdr src) res nonself?))
               (else            ; processing the current res-candidate
                (let ((res-candidate (car res-candidates)))
                  (let rpt ((more (list res-candidate))
                            (next (sxml:context->node (car src)))
                            (src src)
                            (res (if
                                  (and nonself? (test-pred? res-candidate))
                                  (append
                                   res
                                   (list
                                    (draft:smart-make-context
                                     res-candidate res-ancestors num-anc)))
                                  res)))
                    (cond
                     ((null? more)
                                        ; no more src members among res-candidate descendants
                      (loop (cdr res-candidates) src res #t))
                     ((eq? (car more) next)
                                        ; next node is a descendant-or-self of res-candidate
                      (call-with-values
                          (lambda ()
                            (reordering
                             (all-following-siblings (car src))
                             (sxml:context->ancestors (car src))
                             (cdr src)))
                        (lambda (add-res new-src)
                          (if
                           (null? new-src)
                           (loop (cdr res-candidates)
                                 new-src
                                 (append res add-res)
                                 #t)
                           (rpt (cdr more) ; kids processed by recursive
                                (sxml:context->node (car new-src))
                                new-src
                                (append res add-res))))))
                     ((memq next (ddo:attrs-and-values (car more)))
                                        ; the next node is the attribute node or
                                        ; attribute value node => it has no siblings
                      (if
                       (null? (cdr src))
                       (loop (cdr res-candidates)
                             (cdr src)  ; null
                             res
                             #t)
                       (rpt more      ; check for the other attributes
                            (sxml:context->node (car src))
                            (cdr src)
                            res)))
                     (else
                      (rpt (append (child (car more)) (cdr more))
                           next src res)))))))))))
      (lambda (node)                    ; node or nodeset
        (if
         (nodeset? node)
         (let iter ((nset node)
                    (res '()))
           (if
            (null? nset)
            res
            (call-with-values
                (lambda ()
                  (reordering (all-following-siblings (car nset))
                              (sxml:context->ancestors (car nset))
                              (cdr nset)))
              (lambda (add-res new-nset)
                (iter new-nset (append res add-res))))))
         ((draft:following-sibling test-pred? num-anc) node))))))

;;! Namespace axis
;; The alias for drart:namespace, since no reordering or duplicate elimination
;; is required
(define ddo:namespace draft:namespace)

;;! Parent axis
;; When locating a parent, the thing we should care about is that several nodes
;; in the input node-set may have the same parent node
(define (ddo:parent test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (prev-parents '())
                 (res '()))
        (if
         (null? src)
         (reverse res)
         (let ((curr (car src)))
           (if
            (and (sxml:context? curr)
                 (not (null? (sxml:context->ancestors-u curr))))
            (let ((curr-parent (car (sxml:context->ancestors-u curr))))
              (if
               (memq curr-parent prev-parents) ; a duplicate node
               ;; this node is already in the result
               (loop (cdr src) prev-parents res)
               (loop (cdr src)
                     (cons curr-parent prev-parents)
                     (if
                      (test-pred? curr-parent)
                      (cons
                       (draft:smart-make-context
                        curr-parent
                        (cdr (sxml:context->ancestors-u curr))
                        num-anc)
                       res)
                      res))))
            ;; no parent
            (loop (cdr src) prev-parents res))))))))

;;! Preceding axis
;; The implementation exploits the idea expressed in
;; http://pi3.informatik.uni-mannheim.de/publications/TR-02-011.pdf,
;; that is, it is sufficient to calculate preceding for the last_dmin member
;; of the input nodeset
(define (ddo:preceding test-pred? . num-ancestors)
  (let ((prec (apply draft:preceding (cons test-pred? num-ancestors))))
    (lambda (node)                           ; node or nodeset
      (cond
       ((null? node)                   ; empty nodeset - nothing to do
        '())
       ((and (pair? node) (not (symbol? (car node)))) ; non-empty nodeset
        (if
         (null? (cdr node))             ; a singleton nodeset
         (prec (car node))
         (let ((node (reverse node)))
           (let loop ((candidate (car node))
                      (next (sxml:context->node (cadr node)))
                      (more (cdr node))
                      (descendants
                       (reverse
                        (ddo:attr-child (sxml:context->node (car node))))))
             (cond
              ((null? descendants)
               ;; none of the node-set members are descendants of the candidate
               ;; => apply following axis
               (reverse (prec candidate)))
              ((eq? (car descendants) next)
                                        ; the next node is the new candidate
               (if (null? (cdr more)) ; the next node is the final candidate
                   (reverse (prec (car more)))
                   (loop (car more)
                         (sxml:context->node (cadr more))
                         (cdr more)
                         (reverse (ddo:attr-child next)))))
              (else                     ; proceed deeper in a tree
               (loop candidate next more
                     (append (reverse (ddo:attr-child (car descendants)))
                             (cdr descendants)))))))))
       (else                            ; a single node
        (reverse (prec node)))))))

;;! Preceding-sibling axis
(define (ddo:preceding-sibling test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?))
        (all-preceding-siblings
         ;; Selects preceding siblings of the node (should be context)
         (lambda (node)                      ; not a nodeset
           (if
            (and (sxml:context? node)
                 (not (null? (sxml:context->ancestors-u node))))
            (cond
             ((memq (sxml:context->node-u node)
                    (reverse
                     (cdr ; parent is an element => cdr gives its children
                      (car (sxml:context->ancestors-u node)))))
              => cdr)
             (else                      ; no preceding siblings
              '()))
            ;; no parent => no siblings
            '()))))
    (letrec
        ((reordering
          ;; Arguments
          ;;  res-candidates = (listof node) - candidates for the result, not
          ;; yet filtered with a node test
          ;;  res-ancestors - ancestors of res-candidates
          ;;  src - the remaining nodes of the input node-set
          ;; Returns: (values res src)
          (lambda (res-candidates res-ancestors src)
            (let loop ((res-candidates res-candidates)
                       (src src)
                       (res '()))
              (cond
               ((null? res-candidates)
                (values res src))
               ((null? src)             ; this zoo is over
                (values
                 (append
                  res
                  (draft:siblings->context-set
                   ((sxml:filter test-pred?) res-candidates)
                   (draft:list-head res-ancestors num-anc)))
                 src                    ; always null
                 ))
               ((eq? (car res-candidates) (sxml:context->node (car src)))
                (loop res-candidates (cdr src) res))
               (else            ; processing the current res-candidate
                (let ((res-candidate (car res-candidates)))
                  (let rpt ((more (reverse (child res-candidate)))
                            (next (sxml:context->node (car src)))
                            (src src)
                            (res res))
                    (cond
                     ((null? more)
                      ;; no more src members among res-candidate descendants
                      (loop
                       (cdr res-candidates)
                       src
                       (if (test-pred? res-candidate)
                           (append res
                                   (list
                                    (draft:smart-make-context
                                     res-candidate res-ancestors num-anc)))
                           res)))
                     ((eq? (car more) next)
                      ;; next node is a descendant-or-self of res-candidate
                      (call-with-values
                          (lambda ()
                            (reordering
                             (all-preceding-siblings (car src))
                             (sxml:context->ancestors (car src))
                             (cdr src)))
                        (lambda (add-res new-src)
                          (let ((new-src
                                 (cond
                                  ((null? new-src) new-src)
                                  ((eq? res-candidate
                                        (sxml:context->node (car new-src)))
                                   (cdr new-src))
                                  (else new-src))))
                            (if
                             (null? new-src)
                             (loop (cdr res-candidates)
                                   new-src
                                   (if
                                    (test-pred? res-candidate)
                                    (append
                                     res
                                     add-res
                                     (list
                                      (draft:smart-make-context
                                       res-candidate res-ancestors num-anc)))
                                    (append res add-res)))
                             (rpt (cdr more) ; kids processed by recursive
                                  (sxml:context->node (car new-src))
                                  new-src
                                  (append res add-res)))))))
                     (else
                      (rpt (append (reverse (child (car more))) (cdr more))
                           next src res)))))))))))
      (lambda (node)                         ; node or nodeset
        (if
         (nodeset? node)
         (let iter ((nset (reverse node))
                    (res '()))
           (if
            (null? nset)
            (reverse res)
            (call-with-values
                (lambda ()
                  (reordering (all-preceding-siblings (car nset))
                              (sxml:context->ancestors (car nset))
                              (cdr nset)))
              (lambda (add-res new-nset)
                (iter new-nset (append res add-res))))))
         ((draft:following-sibling test-pred? num-anc) node))))))

;;! Self axis
;; The alias for drart:self, since no reordering or duplicate elimination
;; is required
;; num-ancestors is not used here
(define ddo:self draft:self)

;;! Particular case: all nodes in the input node-set are on the same level of
;; hierarchy within a document
;; In this case, some axes can be implemented more effectively

;;! Following axis for special case
;; According to
;; http://pi3.informatik.uni-mannheim.de/publications/TR-02-011.pdf,
;; it is sufficient to calculate following for the first member of the input
;; nodeset
(define (ddo:following-single-level test-pred? . num-ancestors)
  (let ((foll (apply draft:following
                     (cons test-pred? num-ancestors))))
    (lambda (node)                           ; node or nodeset
      (cond
       ((null? node)                   ; empty nodeset - nothing to do
        '())
       ((and (pair? node) (not (symbol? (car node)))) ; non-empty nodeset
        (foll (car node)))
       (else                            ; a single node
        (foll node))))))

;;! Following-sibling axis for the special case
;; We need only to care of duplicates removal, and ordering would be
;; achieved automatically
(define (ddo:following-sibling-single-level test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (res '()))
        (if
         (null? src)                    ; everyone processed
         (reverse res)
         (let ((curr (car src)))
           (if
            (and (sxml:context? curr)
                 (not (null? (sxml:context->ancestors-u curr))))
            (cond
             ((memq (sxml:context->node-u curr)
                    (cdr ; parent is an element => cdr gives its children
                     (car (sxml:context->ancestors-u curr))))
              => (lambda (foll-siblings)
                   (let rpt ((foll-siblings (cdr foll-siblings))
                             (src (cdr src))
                             (res res))
                     (cond
                      ((null? foll-siblings)
                       (loop src res))
                      ((null? src) ; no more source nodes in document order
                       (append
                        (reverse res)
                        (draft:siblings->context-set
                         ((sxml:filter test-pred?) foll-siblings)
                         (draft:list-head
                          (sxml:context->ancestors-u curr) num-anc))))
                      (else
                       (rpt
                        (cdr foll-siblings)
                        (if (eq? (car foll-siblings)
                                 (sxml:context->node (car src)))
                            (cdr src)   ; remove the first source node
                            src)
                        (if (test-pred? (car foll-siblings))
                            (cons
                             (draft:smart-make-context
                              (car foll-siblings)
                              (sxml:context->ancestors-u curr)
                              num-anc)
                             res)
                            res)))))))
             (else                      ; no following siblings
              (loop (cdr src) res)))
            ;; no parent => no siblings
            (loop (cdr src) res))))))))

;;! Parent axis for the case when all nodes in the input node-set are located
;; on the same level of hierarchy within a document
;; In this case the parent axis can be computed with the O(n) complexity, n
;; is the number of nodes in the document, compared to O(n^2) complexity for
;; ddo:parent
(define (ddo:parent-single-level test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; node or nodeset
      (let loop ((src (as-nodeset node))
                 (prev-parent #f)
                 (res '()))
        (if
         (null? src)
         (reverse res)
         (let ((curr (car src)))
           (if
            (and (sxml:context? curr)
                 (not (null? (sxml:context->ancestors-u curr))))
            (let ((curr-parent (car (sxml:context->ancestors-u curr))))
              (if
               ;; this condition would never evaluate to #t when prev-parent=#f
               (eq? curr-parent prev-parent) ; a duplicate node
               ;; this node is already in the result
               (loop (cdr src) prev-parent res)
               (loop (cdr src) curr-parent
                     (if
                      (test-pred? curr-parent)
                      (cons
                       (draft:smart-make-context
                        curr-parent
                        (cdr (sxml:context->ancestors-u curr))
                        num-anc)
                       res)
                      res))))
            ;; no parent
            (loop (cdr src) prev-parent res))))))))

;;! Preceding axis for the special case
;; The implementation exploits the idea expressed in
;; http://pi3.informatik.uni-mannheim.de/publications/TR-02-011.pdf,
;; that is, it is sufficient to calculate preceding for the last member of the
;; input nodeset
(define (ddo:preceding-single-level test-pred? . num-ancestors)
  (let ((prec (apply draft:preceding
                     (cons test-pred? num-ancestors))))
    (lambda (node)                           ; node or nodeset
      (cond
       ((null? node)                   ; empty nodeset - nothing to do
        '())
       ((and (pair? node) (not (symbol? (car node)))) ; non-empty nodeset
        (reverse                        ; restore document order
         (prec (ddo:list-last node))))
       (else                            ; a single node
        (reverse (prec node)))))))

;;! Preceding-sibling axis for the special case
(define (ddo:preceding-sibling-single-level test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; node or nodeset
      (let loop ((src (reverse (as-nodeset node)))
                 (res '()))
        (if
         (null? src)                    ; everyone processed
         res
         (let ((curr (car src)))
           (if
            (and (sxml:context? curr)
                 (not (null? (sxml:context->ancestors-u curr))))
            (cond
             ((memq (sxml:context->node-u curr)
                    (reverse
                     (cdr ; parent is an element => cdr gives its children
                      (car (sxml:context->ancestors-u curr)))))
              => (lambda (prec-siblings)
                   (let rpt ((prec-siblings (cdr prec-siblings))
                             (src (cdr src))
                             (res res))
                     (cond
                      ((null? prec-siblings)
                       (loop src res))
                      ((null? src)      ; no more source nodes
                       (append
                        (reverse
                         (draft:siblings->context-set
                          ((sxml:filter test-pred?) prec-siblings)
                          (draft:list-head
                           (sxml:context->ancestors-u curr) num-anc)))
                        res))
                      (else
                       (rpt
                        (cdr prec-siblings)
                        (if (eq? (car prec-siblings)
                                 (sxml:context->node (car src)))
                            (cdr src)   ; remove the first source node
                            src)
                        (if (test-pred? (car prec-siblings))
                            (cons
                             (draft:smart-make-context
                              (car prec-siblings)
                              (sxml:context->ancestors-u curr)
                              num-anc)
                             res)
                            res)))))))
             (else                      ; no preceding siblings
              (loop (cdr src) res)))
            ;; no parent => no siblings
            (loop (cdr src) res))))))))


;;-------------------------------------------------------------------------------
;;! XPath axes for location steps probably involving position-based predicates
;; Result is represented in the form of
;;  pos-result ::= (listof pos-nodeset)
;;  pos-nodeset ::= (listof (cons node order-num))
;; Each pos-nodeset is a result of applying the axis to a single node in the
;; input nodeset. Pos-result can be informally considered as
;;  (map axis-pos input-nodeset)
;; Each node in the pos-nodeset comes with its order number. An order-num is
;; an integer, possibly a negative one. A node precedes another node in
;; document order if the order-num of the former node is less than the order-num
;; of the latter node. Equal order-nums (in different pos-nodesets) correspond
;; to equal nodes.
;; Each pos-nodeset is sorted in accordance with the position() of each of its
;; members. Consequently, order-nums increase within pos-nodeset for forward
;; XPath axes and decrease for reverse XPath axes.

;;! Ancestor axis, for position-based filtering
(define (ddo:ancestor-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (letrec
        ;; A hybrid of assq and memq
        ((assmemq
          (lambda (key lst)
            (cond
             ((null? lst) #f)
             ((eq? key (caar lst)) lst)
             (else (assmemq key (cdr lst)))))))
      (lambda (node)                         ; node or nodeset
        (let loop ((src (as-nodeset node))
                   (prev-ancestors '())
                   (ancs-alist '())
                   (pos-res '())
                   (vacant-num 1))
                                        ; ancs-alist ::= (listof (cons node pos-nodeset))
          (if
           (null? src)                  ; everyone processed
           pos-res
           (let ((curr (car src)))
             (cond
              ((or (not (sxml:context? curr))
                   (null? (sxml:context->ancestors-u curr)))
                                        ; no ancestors for this node
               (loop (cdr src) prev-ancestors ancs-alist pos-res vacant-num))
              ((and (not (null? prev-ancestors))
                    (eq? (car (sxml:context->ancestors-u curr))
                         (car prev-ancestors)))
                                        ; The common case of removing (some) duplicate result node-sets
                                        ; from consideration.
               (loop (cdr src) prev-ancestors ancs-alist pos-res vacant-num))
              (else
               (let rpt ((curr-ancs (sxml:context->ancestors-u curr))
                         (new-content '()))
                                        ; new content - that didn't repeat with the previous
                                        ; ancestors
                 (cond
                  ((or (null? curr-ancs) ; all ancestors are new
                                        ; the first repeated found
                       (memq (car curr-ancs) prev-ancestors))
                   => (lambda (prev-tail)
                        (call-with-values
                            (lambda()
                              (if
                               (pair? prev-tail)
                               (let ((t
                                      (assmemq (car prev-tail) ancs-alist)))
                                 (values prev-tail t (cdar t)))
                               (values '() '() '())))
                          (lambda (prev-ancestors ancs-alist this-nset)
                            (let creat ((prev-ancestors prev-ancestors)
                                        (ancs-alist ancs-alist)
                                        (vacant-num vacant-num)
                                        (this-nset this-nset)
                                        (new-content new-content))
                              (if
                               (null? new-content) ; everyone processed
                               (loop (cdr src)
                                     prev-ancestors
                                     ancs-alist
                                     (cons this-nset pos-res)
                                     vacant-num)
                               (let ((new-this-nset
                                      (if
                                       (test-pred? (caar new-content))
                                        ; add to the result
                                       (cons
                                        (cons
                                         (draft:smart-make-context
                                          (caar new-content)
                                          (cdar new-content)
                                          num-anc)
                                         vacant-num)
                                        this-nset)
                                       this-nset)))
                                 (creat (car new-content)
                                        (cons
                                         (cons
                                          (caar new-content)
                                          new-this-nset)
                                         ancs-alist)
                                        (+ vacant-num 1)
                                        new-this-nset
                                        (cdr new-content)))))))))
                  (else
                   (rpt (cdr curr-ancs)
                        (cons curr-ancs new-content))))))))))))))

;;! Ancestor-or-self axis, for position-based filtering
(define (ddo:ancestor-or-self-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (letrec
        (                               ; A hybrid of assq and memq
         (assmemq
          (lambda (key lst)
            (cond
             ((null? lst) #f)
             ((eq? key (caar lst)) lst)
             (else (assmemq key (cdr lst)))))))
      (lambda (node)                         ; node or nodeset
        (let loop ((src (as-nodeset node))
                   (prev-ancestors '())
                   (ancs-alist '())
                   (pos-res '())
                   (vacant-num 1))
          ;; ancs-alist ::= (listof (cons node pos-nodeset))
          (if
           (null? src)                  ; everyone processed
           pos-res
           (let rpt ((curr-ancs (sxml:context->content (car src)))
                     (new-content '()))
             ;; new content - that didn't repeat with the previous
             ;; ancestors
             (cond
              ((or (null? curr-ancs)    ; all ancestors are new
                   ;; or the first repeated found
                   (memq (car curr-ancs) prev-ancestors))
               => (lambda (prev-tail)
                    (call-with-values
                        (lambda ()
                          (if
                           (pair? prev-tail)
                           (let ((t (assmemq (car prev-tail) ancs-alist)))
                             (values prev-tail t (cdar t)))
                           (values '() '() '())))
                      (lambda (prev-ancestors ancs-alist this-nset)
                        (let creat ((prev-ancestors prev-ancestors)
                                    (ancs-alist ancs-alist)
                                    (vacant-num vacant-num)
                                    (this-nset this-nset)
                                    (new-content new-content))
                          (if
                           (null? new-content) ; everyone processed
                           (loop (cdr src)
                                 prev-ancestors
                                 ancs-alist
                                 (cons this-nset pos-res)
                                 vacant-num)
                           (let ((new-this-nset
                                  (if
                                   (test-pred? (caar new-content))
                                   ;; add to the result
                                   (cons
                                    (cons
                                     (draft:smart-make-context
                                      (caar new-content)
                                      (cdar new-content)
                                      num-anc)
                                     vacant-num)
                                    this-nset)
                                   this-nset)))
                             (creat (car new-content)
                                    (cons
                                     (cons
                                      (caar new-content)
                                      new-this-nset)
                                     ancs-alist)
                                    (+ vacant-num 1)
                                    new-this-nset
                                    (cdr new-content)))))))))
              (else
               (rpt (cdr curr-ancs)
                    (cons curr-ancs new-content)))))))))))

;;! Child axis, for position-based filtering
(define (ddo:child-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?)))
    (letrec
        ;; Creates a pos-nodeset
        ((create-pos-nset
          (lambda (nset ancestors vacant-num)
            (if (null? nset)
                '()
                (cons
                 (cons (if (null? ancestors)
                           (car nset)
                           (draft:make-context (car nset) ancestors))
                       vacant-num)
                 (create-pos-nset (cdr nset) ancestors (+ vacant-num 1))))))
         (src-walk
          ;; curr-node - current input node (probably context)
          ;; src - the remaining nodes in the input nodeset
          ;; order-num - the order number of the current result node
          ;; Returns: (values pos-result new-src new-order-num)
          (lambda (curr-node src order-num)
            (let ((curr-children
                   (child (sxml:context->node curr-node))))
              (if
               (null? curr-children) ; nothing to do for this curr-node
               (values '() src order-num)
               (let ((curr-ancestors (draft:list-head
                                      (sxml:context->content curr-node)
                                      num-anc)))
                 (if
                  (null? src)  ; no searching for descendants required
                  (values (list (create-pos-nset
                                 ((sxml:filter test-pred?) curr-children)
                                 curr-ancestors order-num))
                          src      ; always null
                          #f       ; nobody cares of order-num anymore
                          )
                  (let loop ((src src)
                             (next-node (sxml:context->node (car src)))
                             (curr-children (cdr curr-children))
                             (desc-to-scan (list (car curr-children)))
                             (this-res
                              (if
                               (test-pred? (car curr-children))
                               (list
                                (cons
                                 (if (null? curr-ancestors)
                                     (car curr-children)
                                     (draft:make-context
                                      (car curr-children) curr-ancestors))
                                 order-num))
                               '()))
                             (pos-result '())
                             (order-num (+ order-num 1)))
                    (cond
                     ((null? desc-to-scan)
                                        ; we can proceed to next curr-children
                      (if
                       (null? curr-children)
                       (values (cons (reverse this-res) pos-result)
                               src
                               order-num)
                       (loop src next-node
                             (cdr curr-children)
                             (list (car curr-children))
                             (if
                              (test-pred? (car curr-children))
                              (cons
                               (cons
                                (if (null? curr-ancestors)
                                    (car curr-children)
                                    (draft:make-context
                                     (car curr-children) curr-ancestors))
                                order-num)
                               this-res)
                              this-res)
                             pos-result
                             (+ order-num 1))))
                                        ; There are descendants to be scanned
                     ((eq? (car desc-to-scan) next-node)
                      (call-with-values
                          (lambda ()
                            (src-walk (car src)
                                      (cdr src)
                                      order-num))
                        (lambda (new-pos-res new-src new-order-num)
                          (if
                           (null? new-src) ; no more nodes in src nodeset
                           (values
                            (cons
                             (append
                              (reverse this-res)
                              (create-pos-nset
                               ((sxml:filter test-pred?) curr-children)
                               curr-ancestors order-num))
                             (append pos-result new-pos-res))
                            new-src     ; always null
                            ;; nobody cares of this number anymore
                            #f)
                           (loop new-src
                                 (sxml:context->node (car new-src))
                                 curr-children
                                 (cdr desc-to-scan) ; descendants processed
                                 this-res
                                 (append pos-result new-pos-res)
                                 new-order-num)))))
                     (else            ; proceed to the next descendant
                      (loop src next-node curr-children
                            (append     ; content-to-scan
                             (ddo:attr-child (car desc-to-scan))
                             (cdr desc-to-scan))
                            this-res
                            pos-result
                            order-num)))))))))))
      (lambda (node)                         ; node or nodeset
        (let rpt ((src (as-nodeset node))
                  (pos-result '())
                  (order-num 1))
          (if
           (null? src)                ; all processed
           (ddo:filter                    ; removing empty result nodesets
            (lambda (x) (not (null? x)))
            pos-result)
           (call-with-values
               (lambda () (src-walk (car src) (cdr src) order-num))
             (lambda (new-pos-res new-src new-order-num)
               (rpt new-src
                    (append pos-result new-pos-res)
                    new-order-num)))))))))

;;! Descendant axis, for position-based filtering
;; When no node in the input node-set is a descendant of another node in the
;; input node-set, the ordinary draft:descendant function can be used
(define (ddo:descendant-pos test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (sxml:child sxml:node?))
         (desc (draft:descendant test-pred? num-anc)))
    (letrec
        ((src-walk
          ;; curr-node - current input node (probably context)
          ;; src - the remaining nodes in the input nodeset
          ;; order-num - the order number of the current result node
          ;; Returns: (values pos-result new-src new-order-num)
          (lambda (curr-node src order-num)
            (let loop ((src src)
                       (next-node (if (null? src)
                                      #f
                                      (sxml:context->node (car src))))
                       (content-to-scan
                        (let ((cntnt (sxml:context->content curr-node)))
                          (map
                           (lambda (c) (cons c cntnt))
                           (child (sxml:context->node curr-node)))))
                       (this-res '())
                       (pos-result '())
                       (order-num order-num))
              (if
               (null? content-to-scan)
               (values (cons (reverse this-res) pos-result)
                       src
                       (+ order-num 1))
               (let ((curr-cntnt (car content-to-scan)))
                 (if
                  (eq? (car curr-cntnt) next-node)
                  (call-with-values
                      (lambda () (src-walk (car src)
                                      (cdr src)
                                      (+ order-num 1)))
                    (lambda (new-pos-res new-src new-order-num)
                      (loop new-src
                            (if (null? new-src)
                                #f
                                (sxml:context->node (car new-src)))
                            (cdr content-to-scan) ; descendants processed
                            (append
                             (reverse (car new-pos-res))
                             (if        ; this res
                              (test-pred? (car curr-cntnt))
                              (cons
                               (cons
                                (draft:smart-make-context
                                 (car curr-cntnt) (cdr curr-cntnt) num-anc)
                                order-num)
                               this-res)
                              this-res))
                            (append pos-result new-pos-res)
                            new-order-num)))
                  (loop src
                        next-node
                        (append         ; content-to-scan
                         (map
                          (lambda (c) (cons c curr-cntnt))
                          (child (car curr-cntnt)))
                         (cdr content-to-scan))
                        (if                            ; this res
                         (test-pred? (car curr-cntnt)) ; satisfies the node test
                         (cons
                          (cons
                           (draft:smart-make-context
                            (car curr-cntnt) (cdr curr-cntnt) num-anc)
                           order-num)
                          this-res)
                         this-res)
                        pos-result
                        (+ order-num 1)))))))))
      (lambda (node)                         ; node or nodeset
        (let rpt ((src (as-nodeset node))
                  (pos-result '())
                  (order-num 1))
          (if
           (null? src)                ; all processed
           (ddo:filter                    ; removing empty result nodesets
            (lambda (x) (not (null? x)))
            pos-result)
           (call-with-values
               (lambda () (src-walk (car src) (cdr src) order-num))
             (lambda (new-pos-res new-src new-order-num)
               (rpt new-src
                    (append pos-result new-pos-res)
                    new-order-num)))))))))

;;! Descendant-or-selt axis, for position-based filtering
;; When no node in the input node-set is a descendant of another node in the
;; input node-set, the ordinary draft:descendant function can be used
(define (ddo:descendant-or-self-pos test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (sxml:child sxml:node?)))
    (letrec
        ((src-walk
          ;; curr-node - current input node (probably context)
          ;; src - the remaining nodes in the input nodeset
          ;; order-num - the order number of the current result node
          ;; Returns: (values pos-result new-src new-order-num)
          (lambda (curr-node src order-num)
            (let loop ((src src)
                       (next-node (if (null? src)
                                      #f
                                      (sxml:context->node (car src))))
                       (content-to-scan
                        (list (sxml:context->content curr-node)))
                       (this-res '())
                       (pos-result '())
                       (order-num order-num))
              (if
               (null? content-to-scan)
               (values (cons (reverse this-res) pos-result)
                       src
                       (+ order-num 1))
               (let ((curr-cntnt (car content-to-scan)))
                 (if
                  (eq? (car curr-cntnt) next-node)
                  (call-with-values
                      (lambda () (src-walk (car src) (cdr src) order-num))
                    (lambda (new-pos-res new-src new-order-num)
                      (loop new-src
                            (if (null? new-src)
                                #f
                                (sxml:context->node (car new-src)))
                            (cdr content-to-scan) ; descendants processed
                            (append
                             (reverse (car new-pos-res))
                             this-res)
                            (append pos-result new-pos-res)
                            new-order-num)))
                  (loop src
                        next-node
                        (append         ; content-to-scan
                         (map
                          (lambda (c) (cons c curr-cntnt))
                          (child (car curr-cntnt)))
                         (cdr content-to-scan))
                        (if                            ; this res
                         (test-pred? (car curr-cntnt)) ; satisfies the node test
                         (cons
                          (cons
                           (draft:smart-make-context
                            (car curr-cntnt) (cdr curr-cntnt) num-anc)
                           order-num)
                          this-res)
                         this-res)
                        pos-result
                        (+ order-num 1)))))))))
      (lambda (node)                         ; node or nodeset
        (let rpt ((src (as-nodeset node))
                  (pos-result '())
                  (order-num 1))
          (if
           (null? src)                ; all processed
           (ddo:filter                    ; removing empty result nodesets
            (lambda (x) (not (null? x)))
            pos-result)
           (call-with-values
               (lambda () (src-walk (car src) (cdr src) order-num))
             (lambda (new-pos-res new-src new-order-num)
               (rpt new-src
                    (append pos-result new-pos-res)
                    new-order-num)))))))))

;;! Following-sibling axis, for position-based filtering
(define (ddo:following-sibling-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?)))
    (letrec
        ((associate-num
          (lambda (nset ancestors vacant-num)
            (if (null? nset)
                nset
                (cons
                 (cons
                  (if (null? ancestors)
                      (car nset)
                      (draft:make-context (car nset) ancestors))
                  vacant-num)
                 (associate-num (cdr nset) ancestors (+ vacant-num 1))))))
         ;; curr - current context to be processed
         ;; src - remaining source contexts
         ;; vacant-num - order number for the result node
         ;; Returns:  (values pos-result new-src new-vacant-num)
         (process-single
          (lambda (curr src vacant-num)
            (if
             (or (not (sxml:context? curr))
                 (null? (sxml:context->ancestors-u curr)))
                                        ; Siblings cannot be identified
             (values '() src vacant-num)
             (cond
              ((memq (sxml:context->node-u curr)
                     (cdr ; parent is an element => cdr gives its children
                      (car (sxml:context->ancestors-u curr))))
               =>
               (lambda (foll-siblings)
                 (let ((ancestors
                        (draft:list-head
                         (sxml:context->ancestors-u curr) num-anc)))
                                        ; Scanning descendants of the context node
                   (let loop ((foll-siblings (cdr foll-siblings))
                              (descs (child (car foll-siblings)))
                              (src (ddo:discard-attributes
                                    (car foll-siblings) src))
                              (vacant-num vacant-num)
                              (res '())
                              (pos-res '()))
                     (cond
                      ((null? src)
                       (values
                        (cons
                         (append
                          (reverse res)
                          (associate-num
                           foll-siblings ancestors vacant-num))
                         pos-res)
                        src      ; always null
                        #f       ; nobody cares of this number anymore
                        ))
                      ((null? descs) ; descendants of current foll-sibling
                       (if
                        (null? foll-siblings) ; that stuff is over
                        (values (cons (reverse res) pos-res)
                                src
                                vacant-num)
                        (let ((new-res
                               (if (test-pred? (car foll-siblings))
                                   (cons
                                    (cons
                                     (if (null? ancestors)
                                         (car foll-siblings)
                                         (draft:make-context
                                          (car foll-siblings) ancestors))
                                     vacant-num)
                                    res)
                                   res)))
                          (if
                           (eq? (car foll-siblings)
                                (sxml:context->node (car src)))
                           (call-with-values
                               (lambda ()
                                 (process-single
                                  (car src) (cdr src) (+ vacant-num 1)))
                             (lambda (new-pos-res new-src new-vacant)
                               (values (cons
                                        (append
                                         (reverse new-res)
                                         (if (null? new-pos-res)
                                             '() (car new-pos-res)))
                                        (append pos-res new-pos-res))
                                       new-src
                                       new-vacant)))
                           (loop (cdr foll-siblings)
                                 (ddo:attr-child (car foll-siblings))
                                 (ddo:discard-attributes
                                  (car foll-siblings) src)
                                 (+ vacant-num 1)
                                 new-res
                                 pos-res)))))
                      ((eq? (car descs) (sxml:context->node (car src)))
                                        ; His siblings are on the way
                       (call-with-values
                           (lambda ()
                             (process-single
                              (car src) (cdr src) vacant-num))
                         (lambda (new-pos-res new-src new-vacant)
                           (loop foll-siblings
                                 (cdr descs) ; descendants processed
                                 new-src
                                 new-vacant
                                 res
                                 (cons pos-res new-pos-res)))))
                      (else
                       (loop foll-siblings
                             (append (child (car descs)) (cdr descs))
                             (ddo:discard-attributes (car descs) src)
                             vacant-num
                             res
                             pos-res)))))))
              (else
               (values '() src vacant-num)))))))
      (lambda (node)                         ; node or nodeset
        (if
         (nodeset? node)
         (let iter ((src node)
                    (pos-res '())
                    (vacant-num 1))
           (if
            (null? src)
            (ddo:filter                   ; removing empty result nodesets
             (lambda (x) (not (null? x)))
             pos-res)
            (call-with-values
                (lambda () (process-single (car src) (cdr src) vacant-num))
              (lambda (new-pos-res new-src new-vacant)
                (iter new-src
                      (append pos-res new-pos-res)
                      new-vacant))))))))))

;;! Parent axis, for position-based filtering
;; We won't reinvent the wheel here. We'll use ddo:parent and apply the fact
;; that for every single input node the parent axis produces no more than a
;; single result node
(define (ddo:parent-pos test-pred? . num-ancestors)
  (let ((parent (apply ddo:parent (cons test-pred? num-ancestors))))
    (letrec
        ((add-order-num
          (lambda (num nset)
            (if (null? nset)
                nset
                (cons (list (cons (car nset) num))
                      (add-order-num (+ num 1) (cdr nset)))))))
      (lambda (node)                         ; node or nodeset
        (add-order-num 1 (parent node))))))

;;! Preceding-sibling axis, for position-based filtering
(define (ddo:preceding-sibling-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
        (child (sxml:child sxml:node?))
        (reverse-desc                   ; reverse descendants of node
         (lambda (node)
           (let scan ((more (ddo:attr-child node))
                      (res '()))
             (if (null? more)           ; result formed
                 res
                 (scan (append (ddo:attr-child (car more))
                               (cdr more))
                       (cons (car more) res)))))))
    (letrec
        ((associate-num
          (lambda (nset ancestors vacant-num)
            (if (null? nset)
                nset
                (cons
                 (cons
                  (if (null? ancestors)
                      (car nset)
                      (draft:make-context (car nset) ancestors))
                  vacant-num)
                 (associate-num (cdr nset) ancestors (- vacant-num 1))))))
                                        ; curr - current context to be processed
                                        ; src - remaining source contexts
                                        ; vacant-num - order number for the result node
                                        ; Returns:  (values pos-result new-src new-vacant-num)
         (process-single
          (lambda (curr src vacant-num)
            (if
             (or (not (sxml:context? curr))
                 (null? (sxml:context->ancestors-u curr)))
                                        ; Siblings cannot be identified
             (values '() src vacant-num)
             (cond
              ((memq (sxml:context->node-u curr)
                     (reverse
                      (cdr ; parent is an element => cdr gives its children
                       (car (sxml:context->ancestors-u curr)))))
               =>
               (lambda (prec-siblings)  ; prec-siblings + self
                 (if
                  (null? (cdr prec-siblings)) ; no preceding siblings
                  (values '() src vacant-num)
                  (let ((ancestors
                         (draft:list-head
                          (sxml:context->ancestors-u curr) num-anc)))
                                        ; Scanning descendants of the context node
                    (let loop ((prec-siblings (cdr prec-siblings))
                               (descs (reverse-desc (cadr prec-siblings)))
                               (src src)
                               (vacant-num vacant-num)
                               (res '())
                               (pos-res '()))
                      (cond
                       ((null? src)
                        (values
                         (cons
                          (append
                           (reverse res)
                           (associate-num
                                        ; DL: was: (if nonself? prec-siblings (cdr prec-siblings))
                            prec-siblings
                            ancestors vacant-num))
                          pos-res)
                         src     ; always null
                         #f      ; nobody cares of this number anymore
                         ))
                       ((null? descs) ; descendants of current prec-sibling
                        (let ((new-res
                               (if (test-pred? (car prec-siblings))
                                   (cons
                                    (cons
                                     (if (null? ancestors)
                                         (car prec-siblings)
                                         (draft:make-context
                                          (car prec-siblings) ancestors))
                                     vacant-num)
                                    res)
                                   res)))
                          (cond
                           ((eq? (car prec-siblings) ; to be now added
                                 (sxml:context->node (car src)))
                            (call-with-values
                                (lambda ()
                                  (process-single
                                   (car src) (cdr src) (- vacant-num 1)))
                              (lambda (new-pos-res new-src new-vacant)
                                (values (cons
                                         (append
                                          (reverse new-res)
                                          (if (null? new-pos-res)
                                              '() (car new-pos-res)))
                                         (append pos-res new-pos-res))
                                        new-src
                                        new-vacant))))
                           ((null? (cdr prec-siblings)) ; that stuff is over
                            (values (cons (reverse new-res) pos-res)
                                    src
                                    vacant-num))
                           (else
                            (loop (cdr prec-siblings)
                                  (reverse-desc (cadr prec-siblings))
                                  src
                                  (- vacant-num 1)
                                  new-res
                                  pos-res)))))
                       ((eq? (car descs) (sxml:context->node (car src)))
                                        ; His siblings are on the way
                        (call-with-values
                            (lambda ()
                              (process-single
                               (car src) (cdr src) vacant-num))
                          (lambda (new-pos-res new-src new-vacant)
                            (loop prec-siblings
                                  (cdr descs) ; descendants processed
                                  new-src
                                  new-vacant
                                  res
                                  (append pos-res new-pos-res)))))
                       (else
                        (loop prec-siblings
                              (cdr descs)
                              src
                              vacant-num
                              res
                              pos-res))))))))
              (else
               (values '() src vacant-num)))))))
      (lambda (node)                    ; node or nodeset
        (if
         (nodeset? node)
         (let iter ((src (reverse node))
                    (pos-res '())
                    (vacant-num -1))
           (if
            (null? src)
            (ddo:filter                   ; removing empty result nodesets
             (lambda (x) (not (null? x)))
             pos-res)
            (call-with-values
                (lambda () (process-single (car src) (cdr src) vacant-num))
              (lambda (new-pos-res new-src new-vacant)
                (iter new-src
                      (append new-pos-res pos-res)
                      new-vacant))))))))))


;; Particular case: all nodes in the input node-set are on the same level of
;; hierarchy within a document
;; In this case, some axes can be implemented more effectively

;;! Following axis for the special case
(define (ddo:following-single-level-pos test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (descend (draft:descendant-or-self test-pred? num-anc))
         (follow (draft:following test-pred? num-anc)))
    (letrec
        ;; curr - current context to be processed
        ;; src - remaining source contexts
        ;; vacant-num - order number for the result node
        ;; Returns:  pos-result
        ((process-single
          (lambda (curr src vacant-num)
            (cond
             ((null? src)               ; no more nodes
              (let add-labels ((to-scan (follow curr))
                               (res '())
                               (vacant-num vacant-num))
                (if (null? to-scan)
                    (list (reverse res))
                    (add-labels (cdr to-scan)
                                (cons (cons (car to-scan) vacant-num) res)
                                (+ vacant-num 1)))))
             ((not (sxml:context? curr)) ; can't find following nodes
              (cons '() (process-single (car src) (cdr src) vacant-num)))
             (else
              (let ((next (sxml:context->node (car src))))
                (let loop ((this-level (sxml:context->node-u curr))
                           (ancs-to-view (sxml:context->ancestors-u curr))
                           (content-set '())
                           (pos-nset '())
                           (vacant-num vacant-num))
                  (cond
                   ((null? content-set) ; no one to scan at this level
                    (cond
                     ((null? ancs-to-view)
                      (cons
                       (reverse pos-nset)
                       (process-single (car src) (cdr src) vacant-num)))
                     ((memq next (sxml:attr-list (car ancs-to-view)))
                                        ; next is an attribute
                      (let ((pos-result
                             (process-single (car src) (cdr src) vacant-num)))
                        (cons
                         (append (reverse pos-nset) (car pos-result))
                         pos-result)))
                     (else              ; go to the next level
                      (loop
                       (car ancs-to-view)
                       (cdr ancs-to-view)
                       (map
                        (lambda (n) (cons n (cdr ancs-to-view)))
                        (cond
                         ((memq this-level
                                (cdr ; parent is an element => children
                                 (car ancs-to-view)))
                          => cdr)
                         (else        ; curr-node is an attribute node
                          ((select-kids sxml:node?) (car ancs-to-view)))))
                       pos-nset
                       vacant-num))))
                   ((memq next (sxml:attr-list (caar ancs-to-view)))
                                        ; next node is an attribute of currently scanned node
                    (let ((pos-result
                           (process-single (car src) (cdr src) vacant-num)))
                      (cons
                       (append (reverse pos-nset) (car pos-result))
                       pos-result)))
                   ((eq? (caar content-set) next)
                                        ; current node is eq to the next one in src
                    (let add-desc ((to-scan
                                    (descend
                                     (draft:smart-make-context
                                      (caar content-set)
                                      (cdar content-set)
                                      num-anc)))
                                   (pos-nset pos-nset)
                                   (vacant-num vacant-num))
                      (if
                       (null? to-scan)
                       (let ((pos-result
                              (process-single
                               (car src) (cdr src) vacant-num)))
                         (cons
                          (append (reverse pos-nset) (car pos-result))
                          pos-result))
                       (add-desc (cdr to-scan)
                                 (cons (cons (car to-scan) vacant-num)
                                       pos-nset)
                                 (+ vacant-num 1)))))
                   (else                ; go on to scan the next node
                    (loop
                     this-level
                     ancs-to-view
                     (append
                      (map
                       (lambda (n) (cons n (car content-set)))
                       ((sxml:child sxml:node?) (caar content-set)))
                      (cdr content-set))
                     (if
                      (test-pred? (caar content-set))
                      (cons (cons (draft:smart-make-context
                                   (caar content-set) (cdar content-set)
                                   num-anc)
                                  vacant-num)
                            pos-nset)
                      pos-nset)
                     (+ vacant-num 1)))))))))))
      (lambda (node)                    ; node or nodeset
        (let ((nset (as-nodeset node)))
          (if (null? nset)              ; nothing to be done
              nset
              (ddo:filter                 ; removing empty result nodesets
               (lambda (x) (not (null? x)))
               (process-single (car nset) (cdr nset) 1))))))))

;;! Following-sibling axis for the special case
(define (ddo:following-sibling-single-level-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (letrec
        ;; curr - current context to be processed
        ;; src - remaining source contexts
        ;; vacant-num - order number for the result node
        ;; Returns:  pos-result
        ((process-single
          (lambda (curr src vacant-num)
            (if
             (or (not (sxml:context? curr))
                 (null? (sxml:context->ancestors-u curr)))
             ;; Siblings cannot be identified
             (if (null? src)            ; recursion is over
                 '()
                 (process-single (car src) (cdr src) vacant-num))
             (cond
              ((memq (sxml:context->node-u curr)
                     (cdr ; parent is an element => cdr gives its children
                      (car (sxml:context->ancestors-u curr))))
               => (lambda (foll-siblings)
                    (let ((ancestors
                           (draft:list-head
                            (sxml:context->ancestors-u curr) num-anc)))
                      (if
                       (null? src)      ; no more nodes
                       (let no-more ((foll-siblings (cdr foll-siblings))
                                     (res '())
                                     (vacant-num vacant-num))
                         (if
                          (null? foll-siblings) ; everyone processed
                          (list (reverse res))
                          (no-more
                           (cdr foll-siblings)
                           (if (test-pred? (car foll-siblings))
                               (cons
                                (cons
                                 (if (null? ancestors)
                                     (car foll-siblings)
                                     (draft:make-context
                                      (car foll-siblings) ancestors))
                                 vacant-num)
                                res)
                               res)
                           (+ vacant-num 1))))
                                        ; else there are more nodes in src
                       (let ((next (sxml:context->node (car src))))
                         (let more ((foll-siblings (cdr foll-siblings))
                                    (res '())
                                    (vacant-num vacant-num))
                           (if
                            (null? foll-siblings) ; everyone processed
                            (cons
                             (reverse res)
                             (process-single (car src) (cdr src) vacant-num))
                            (let ((new-res
                                   (if (test-pred? (car foll-siblings))
                                       (cons
                                        (cons
                                         (if (null? ancestors)
                                             (car foll-siblings)
                                             (draft:make-context
                                              (car foll-siblings) ancestors))
                                         vacant-num)
                                        res)
                                       res)))
                              (if
                               (eq? (car foll-siblings) next) ; recursion
                               (let ((pos-res
                                      (process-single
                                       (car src)
                                       (cdr src)
                                       (+ vacant-num 1))))
                                 (if
                                  (null? pos-res) ; this shouldn't occur
                                  (list (reverse new-res))
                                  (cons (append
                                         (reverse new-res) (car pos-res))
                                        pos-res)))
                               (more (cdr foll-siblings)
                                     new-res
                                     (+ vacant-num 1)))))))))))
              (else                     ; no following siblings
               (if (null? src)          ; recursion is over
                   '()
                   (process-single (car src) (cdr src) vacant-num))))))))
      (lambda (node)                         ; node or nodeset
        (let ((nset (as-nodeset node)))
          (if (null? nset)              ; nothing to be done
              nset
              (ddo:filter                 ; removing empty result nodesets
               (lambda (x) (not (null? x)))
               (process-single (car nset) (cdr nset) 1))))))))

;;! Parent axis, for position-based filtering
;; This function has very much the same with ddo:parent-pos. In future, we
;; should use a meta-function for paremetrization of these two
(define (ddo:parent-single-level-pos test-pred? . num-ancestors)
  (let ((parent
         (apply ddo:parent-single-level (cons test-pred? num-ancestors))))
    (letrec
        ((add-order-num
          (lambda (num nset)
            (if (null? nset)
                nset
                (cons (list (cons (car nset) num))
                      (add-order-num (+ num 1) (cdr nset)))))))
      (lambda (node)                         ; node or nodeset
        (add-order-num 1 (parent node))))))

;;! Preceding axis for the special case
(define (ddo:preceding-single-level-pos test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (descend (draft:descendant-or-self test-pred? num-anc))
         (precede (draft:preceding test-pred? num-anc)))
    (letrec
        ;; curr - current context to be processed
        ;; src - remaining source contexts
        ;; vacant-num - order number for the result node
        ;; Returns:  pos-result
        ((process-single
          (lambda (curr src vacant-num)
            (cond
             ((null? src)               ; no more nodes
              (let add-labels ((to-scan (precede curr))
                               (res '())
                               (vacant-num vacant-num))
                (if (null? to-scan)
                    (list (reverse res))
                    (add-labels (cdr to-scan)
                                (cons (cons (car to-scan) vacant-num) res)
                                (- vacant-num 1)))))
             ((not (sxml:context? curr)) ; can't find following nodes
              (cons '() (process-single (car src) (cdr src) vacant-num)))
             (else
              (let ((next (sxml:context->node (car src))))
                (let loop ((this-level (sxml:context->node-u curr))
                           (ancs-to-view (sxml:context->ancestors-u curr))
                           (content-set '())
                           (pos-nset '())
                           (vacant-num vacant-num))
                  (cond
                   ((null? content-set) ; no one to scan at this level
                    (if
                     (null? ancs-to-view)
                     (cons
                      (reverse pos-nset)
                      (process-single (car src) (cdr src) vacant-num))
                     (loop
                      (car ancs-to-view)
                      (cdr ancs-to-view)
                      (reverse
                       (map
                        sxml:context->content
                        (descend
                         (map
                          (lambda (n)
                            (draft:smart-make-context
                             n (cdr ancs-to-view) num-anc))
                          (cond
                           ((memq this-level
                                  (reverse
                                   ((select-kids sxml:node?)
                                    (car ancs-to-view))))
                            => (lambda (nset) (reverse (cdr nset))))
                           (else      ; curr-node is an attribute node
                            '()))))))
                      pos-nset
                      vacant-num)))
                   ((eq? (caar content-set) next)
                                        ; current node is eq to the next one in src
                    (let ((pos-result
                           (process-single
                            (car src)
                            (cdr src)
                            (- vacant-num 1))))
                      (cons
                       (append
                        (reverse
                         (if
                          (test-pred? (caar content-set))
                          (cons (cons (draft:smart-make-context
                                       (caar content-set) (cdar content-set)
                                       num-anc)
                                      vacant-num)
                                pos-nset)
                          pos-nset))
                        (car pos-result))
                       pos-result)))
                   (else                ; go on to scan the next node
                    (loop
                     this-level
                     ancs-to-view
                     (cdr content-set)
                     (if
                      (test-pred? (caar content-set))
                      (cons (cons (draft:smart-make-context
                                   (caar content-set) (cdar content-set)
                                   num-anc)
                                  vacant-num)
                            pos-nset)
                      pos-nset)
                     (- vacant-num 1)))))))))))
      (lambda (node)                    ; node or nodeset
        (let ((nset (reverse (as-nodeset node))))
          (if (null? nset)              ; nothing to be done
              nset
              (ddo:filter                 ; removing empty result nodesets
               (lambda (x) (not (null? x)))
               (process-single (car nset) (cdr nset) -1))))))))

;;! Preceding-sibling axis for the special case
(define (ddo:preceding-sibling-single-level-pos test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (letrec
        ;; curr - current context to be processed
        ;; src - remaining source contexts
        ;; vacant-num - order number for the result node
        ;; Returns:  pos-result
        ((process-single
          (lambda (curr src vacant-num)
            (if
             (or (not (sxml:context? curr))
                 (null? (sxml:context->ancestors-u curr)))
                                        ; Siblings cannot be identified
             (if (null? src)            ; recursion is over
                 '()
                 (process-single (car src) (cdr src) vacant-num))
             (cond
              ((memq (sxml:context->node-u curr)
                     (reverse
                      (cdr ; parent is an element => cdr gives its children
                       (car (sxml:context->ancestors-u curr)))))
               => (lambda (prec-siblings)
                    (let ((ancestors
                           (draft:list-head
                            (sxml:context->ancestors-u curr) num-anc)))
                      (if
                       (null? src)      ; no more nodes
                       (let no-more ((prec-siblings (cdr prec-siblings))
                                     (res '())
                                     (vacant-num vacant-num))
                         (if
                          (null? prec-siblings) ; everyone processed
                          (list (reverse res))
                          (no-more
                           (cdr prec-siblings)
                           (if (test-pred? (car prec-siblings))
                               (cons
                                (cons
                                 (if (null? ancestors)
                                     (car prec-siblings)
                                     (draft:make-context
                                      (car prec-siblings) ancestors))
                                 vacant-num)
                                res)
                               res)
                           (- vacant-num 1))))
                                        ; else there are more nodes in src
                       (let ((next (sxml:context->node (car src))))
                         (let more ((prec-siblings (cdr prec-siblings))
                                    (res '())
                                    (vacant-num vacant-num))
                           (if
                            (null? prec-siblings) ; everyone processed
                            (cons
                             (reverse res)
                             (process-single (car src) (cdr src) vacant-num))
                            (let ((new-res
                                   (if (test-pred? (car prec-siblings))
                                       (cons
                                        (cons
                                         (if (null? ancestors)
                                             (car prec-siblings)
                                             (draft:make-context
                                              (car prec-siblings) ancestors))
                                         vacant-num)
                                        res)
                                       res)))
                              (if
                               (eq? (car prec-siblings) next) ; recursion
                               (let ((pos-res
                                      (process-single
                                       (car src)
                                       (cdr src)
                                       (- vacant-num 1))))
                                 (if
                                  (null? pos-res) ; this shouldn't occur
                                  (list (reverse new-res))
                                  (cons (append
                                         (reverse new-res) (car pos-res))
                                        pos-res)))
                               (more (cdr prec-siblings)
                                     new-res
                                     (- vacant-num 1)))))))))))
              (else                     ; no preceding siblings
               (if (null? src)          ; recursion is over
                   '()
                   (process-single (car src) (cdr src) vacant-num))))))))
      (lambda (node)                         ; node or nodeset
        (let ((nset (reverse (as-nodeset node))))
          (if (null? nset)              ; nothing to be done
              nset
              (reverse
               (ddo:filter                ; removing empty result nodesets
                (lambda (x) (not (null? x)))
                (process-single (car nset) (cdr nset) -1)))))))))





;;===============================================================================
;; XPath implementation with distinct document order support
;
;; This software is in Public Domain.
;; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
;; Please send bug reports and comments to:
;;   lizorkin@hotbox.ru    Dmitry Lizorkin

;;-------------------------------------------------------------------------------
;;!! Miscellaneous

;;! Implement 'or' as a function, so that we could 'apply' it
(define (ddo:or . args)
  (if (null? args) #f (or (car args) (apply ddo:or (cdr args)))))

(define (ddo:foldr op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (ddo:foldr op init (cdr lst)))))

;;! Definition of types
(define ddo:type-nodeset 'ddo:type-nodeset)

(define ddo:type-number 'ddo:type-number)

(define ddo:type-string 'ddo:type-string)

(define ddo:type-boolean 'ddo:type-boolean)

(define ddo:type-any 'ddo:type-any)

;;-------------------------------------------------------------------------------
;;!! Comparison for nodesets
;; In order to compare nodesets produced by conventional SXPath and SXPath with
;; distinct document order support, we must take into account that members in
;; each of the nodesets being compared can be ordered differently.

;;! Whether all members from the first nodeset are contained in the second
;; nodeset
(define (ddo:nset-contained? nodeset1 nodeset2)
  (cond
   ((null? nodeset1) #t)
   ((memq (car nodeset1) nodeset2)
    (ddo:nset-contained? (cdr nodeset1) nodeset2))
   (else #f)))

(define (ddo:nset-equal? nodeset1 nodeset2)
  (and (ddo:nset-contained? nodeset1 nodeset2)
       (ddo:nset-contained? nodeset2 nodeset1)))


;;-------------------------------------------------------------------------------
;;!! Different cases of nodeset filtering

;; Filtering pos-result with (position-based) predicates and combining
;; a filtered pos-result into a distinct document order nodeset
;;  pos-result ::= (listof pos-nodeset)
;;  pos-nodeset ::= (listof (cons node order-num))
;; Each pos-nodeset is a result of applying the axis to a single node in the
;; input nodeset. Pos-result can be informally considered as
;;  (map axis-pos input-nodeset)
;; Each node in the pos-nodeset comes with its order number. An order-num is
;; an integer, possibly a negative one. A node precedes another node in
;; document order if the order-num of the former node is less than the order-num
;; of the latter node. Equal order-nums (in different pos-nodesets) correspond
;; to equal nodes.
;; Each pos-nodeset is sorted in accordance with the position() of each of its
;; members. Consequently, order-nums increase within pos-nodeset for forward
;; XPath axes and decrease for reverse XPath axes.

;;! Whether pos-result in a forward order
;; Return #t if in document order, #f if in reverse document order
(define (ddo:pos-result-forward? pos-result)
  (let loop ((pos-res pos-result))
    (cond
     ((null? pos-res)         ; every pos-nodeset has the length of <2
      #t)
     ((or (null? (car pos-res)) (null? (cdar pos-res)))
                                        ; this pos-nodeset has the length of less or equal to 1
      (loop (cdr pos-res)))
     (else
      (< (cdaar pos-res) (cdadar pos-res))))))

;;! Unites pos-result into a nodeset in distinct document order
(define (ddo:pos-result->nodeset pos-result)
  (letrec (                ; Combines 2 pos-nodesets into a single one
           (combine-2-pos-nodesets
            (lambda (chain1 chain2)
              (cond
               ((null? chain1) chain2)
               ((null? chain2) chain1)
                                        ; None of the chains are null
               ((eq? (caar chain1) (caar chain2)) ; equal nodes
                                        ; the same with (= (cdar chain1) (cdar chain2))
                (cons (car chain1)
                      (combine-2-pos-nodesets (cdr chain1) (cdr chain2))))
               ((< (cdar chain1) (cdar chain2))
                (cons (car chain1)
                      (combine-2-pos-nodesets (cdr chain1) chain2)))
               (else
                (cons (car chain2)
                      (combine-2-pos-nodesets chain1 (cdr chain2))))))))
    (if
     (null? pos-result)                 ; nothing to do
     pos-result
     (let ((pos-result (if (ddo:pos-result-forward? pos-result)
                           pos-result
                           (map reverse pos-result))))
       (let loop ((res (car pos-result))
                  (to-scan (cdr pos-result)))
         (if (null? to-scan)
             res
             (loop (combine-2-pos-nodesets res (car to-scan))
                   (cdr to-scan))))))))

;;!  pos-axis-impl ::= lambda
;;  pred-impl-lst ::= (listof lambda)
;; Every predicate is called with respect to each node
;; Returns:  lambda
;;  lambda ::= (lambda (nodeset position+size var-binding) ...)
(define (ddo:location-step-pos pos-axis-impl pred-impl-lst)
  (lambda (nodeset position+size var-binding)
    (map
     car
     (ddo:pos-result->nodeset
      (map
       (lambda (pos-nodeset)
         (let iter-preds ((nset pos-nodeset)
                          (preds pred-impl-lst))
           (if
            (null? preds)
            nset
            (let ((size (length nset))) ; context size
              (let iter-pairs ((nset nset)
                               (res '())
                               (pos 1))
                (if
                 (null? nset)       ; continue with the next predicate
                 (iter-preds (reverse res) (cdr preds))
                 (let ((val ((car preds) ; predicate value
                             (list (caar nset)) (cons pos size) var-binding)))
                   (iter-pairs (cdr nset)
                               (if (if (number? val)
                                       (= val pos)
                                       (sxml:boolean val))
                                   (cons (car nset) res)
                                   res)
                               (+ pos 1)))))))))
       (pos-axis-impl nodeset))))))

;;-------------------------------------------------------------------------------
;;!! Implementation for location step for the other cases

;;! A location step for the axis which doesn't return a result in the form of
;; a pos-nodeset, but instead resulting nodesets for each input node are in
;; document order
;;  pos-axis-impl ::= lambda
;;  pred-impl-lst ::= (listof lambda)
;; Every predicate is called with respect to each node
;; Returns:  lambda
;;  lambda ::= (lambda (nodeset position+size var-binding) ...)
;; This function is somewhat similar to 'sxml:xpath-nodeset-filter' from
;; "txpath.scm"
(define (ddo:location-step-non-intersect axis-impl pred-impl-lst)
  (lambda (nodeset position+size var-binding)
    (map-union
     (lambda (node)
       (let iter-preds ((nset (axis-impl node))
                        (preds pred-impl-lst))
         (if
          (null? preds)
          nset
          (let ((size (length nset)))   ; context size
            (let iter-nodes ((nset nset)
                             (res '())
                             (pos 1))
              (if
               (null? nset)         ; continue with the next predicate
               (iter-preds (reverse res) (cdr preds))
               (let ((val ((car preds)  ; predicate value
                           (list (car nset)) (cons pos size) var-binding)))
                 (iter-nodes (cdr nset)
                             (if (if (number? val)
                                     (= val pos)
                                     (sxml:boolean val))
                                 (cons (car nset) res)
                                 res)
                             (+ pos 1)))))))))
     nodeset)))

;;! A location step doesn't contain position-based predicates
(define (ddo:location-step-non-pos axis-impl pred-impl-lst)
  (lambda (nodeset position+size var-binding)
    (let iter-preds ((nset (axis-impl nodeset))
                     (preds pred-impl-lst))
      (if
       (null? preds)
       nset
       (let ((curr-pred (car preds)))
         (iter-preds
          (ddo:filter
           (lambda (node)
             (sxml:boolean
              (curr-pred (list node)
                         (cons 1 1)     ; dummy
                         var-binding)))
           nset)
          (cdr preds)))))))

;;-------------------------------------------------------------------------------
;;!! Implementations for FilterExpr

;;! Implementing FilterExpr in the general case, for position-based predicates
(define (ddo:filter-expr-general expr-impl pred-impl-lst)
  (lambda (nodeset position+size var-binding)
    (let ((prim-res (expr-impl nodeset position+size var-binding)))
      (cond
       ((not (nodeset? prim-res))
        (sxml:xpointer-runtime-error
         "expected - nodeset instead of " prim-res)
        '())
       (else
        (let iter-preds ((nset prim-res)
                         (preds pred-impl-lst))
          (if
           (null? preds)
           nset
           (let ((size (length nset)))  ; context size
             (let iter-nodes ((nset nset)
                              (res '())
                              (pos 1))
               (if
                (null? nset)        ; continue with the next predicate
                (iter-preds (reverse res) (cdr preds))
                (let ((val ((car preds) ; predicate value
                            (list (car nset)) (cons pos size) var-binding)))
                  (iter-nodes (cdr nset)
                              (if (if (number? val)
                                      (= val pos)
                                      (sxml:boolean val))
                                  (cons (car nset) res)
                                  res)
                              (+ pos 1)))))))))))))

;;! A FilterExpr doesn't contain position-based predicates
;; NOTE: This function is very similar to 'ddo:location-step-non-pos'
;;  Should think of combining them.
(define (ddo:filter-expr-non-pos expr-impl pred-impl-lst)
  (lambda (nodeset position+size var-binding)
    (let ((prim-res (expr-impl nodeset position+size var-binding)))
      (cond
       ((not (nodeset? prim-res))
        (sxml:xpointer-runtime-error
         "expected - nodeset instead of " prim-res)
        '())
       (else
        (let iter-preds ((nset prim-res)
                         (preds pred-impl-lst))
          (if
           (null? preds)
           nset
           (let ((curr-pred (car preds)))
             (iter-preds
              (ddo:filter
               (lambda (node)
                 (sxml:boolean
                  (curr-pred (list node)
                             (cons 1 1) ; dummy
                             var-binding)))
               nset)
              (cdr preds))))))))))

;;!  Filter expression, with a single predicate of the special structure, like
;;  [position()=1]
;; special-pred-impl ::= (lambda (nodeset) ...)  - filters the nodeset
(define (ddo:filter-expr-special-predicate expr-impl special-pred-impl)
  (lambda (nodeset position+size var-binding)
    (let ((prim-res (expr-impl nodeset position+size var-binding)))
      (if
       (not (nodeset? prim-res))
       (begin
         (sxml:xpointer-runtime-error
          "expected - nodeset instead of " prim-res)
         '())
       (special-pred-impl prim-res)))))


;;-------------------------------------------------------------------------------
;;!! Uniting context-sets, preserving distinct document order
;; Is required for XPath UnionExpr

;;! Returns all contexts of the document, including the ones for attribute nodes
;; and for attribute value nodes. All contexts are returned in document order,
;; attribute value nodes immediately follow attribute nodes
(define (ddo:all-contexts-in-doc doc)
  (let iter-nodes ((contents (map
                              (lambda (kid) (list kid doc))
                              ((sxml:child sxml:node?) doc)))
                   (res (list doc)))
    (cond
     ((null? contents)                  ; every content processed
      (reverse res))
     ((not ((ntype?? '*) (caar contents))) ; text node or PI or etc.
      (iter-nodes (cdr contents)
                  (cons
                   (draft:make-context (caar contents) (cdar contents))
                   res)))
     (else                              ; element node
      (let iter-attrs ((attrs (sxml:attr-list (caar contents)))
                       (res (cons
                             (draft:make-context
                              (caar contents) (cdar contents))
                             res)))
        (cond
         ((null? attrs)  ; all attributes of a given element processed
          (iter-nodes
           (append (map
                    (lambda (kid) (cons kid (car contents)))
                    ((sxml:child sxml:node?) (caar contents)))
                   (cdr contents))
           res))
         ((not (sxml:node? (car attrs))) ; aux node of SXML 3.0
          (iter-attrs (cdr attrs) res))
         ((null? (cdar attrs))          ; singular attribute
          (iter-attrs (cdr attrs)
                      (cons
                       (draft:make-context (car attrs) (car contents))
                       res)))
         (else                          ; an attribute has a value
          (iter-attrs
           (cdr attrs)
           (cons                        ; attribute value
            (draft:make-context (cadar attrs)
                                (cons (car attrs) (car contents)))
            (cons
             (draft:make-context (car attrs) (car contents))
             res))))))))))

;;! Every context in both context-sets must contain all the ancestors of the
;; context node (this corresponds to the num-ancestors=#f)
;; All nodes must have one and the same root node (i.e. this function cannot
;; correctly unite context-sets whose members belong to different documents)
;; Returns the context-set that is a distinct-document-order union of the
;; argument context-sets
(define (ddo:unite-2-contextsets cntset1 cntset2)
  (if
   (null? cntset1)                      ; nothing to do
   cntset2
   (let loop ((order (ddo:all-contexts-in-doc
                      (draft:list-last
                       (sxml:context->content (car cntset1)))))
              (cntset1 cntset1)
              (cntset2 cntset2)
              (res '()))
     (cond
      ((null? cntset1)
       (append (reverse res) cntset2))
      ((null? cntset2)
       (append (reverse res) cntset1))
                                        ; order should never be null
      ((eq? (sxml:context->node (car order))
            (sxml:context->node (car cntset1)))
       (loop (cdr order)
             (cdr cntset1)
             (if (eq? (sxml:context->node (car cntset1))
                      (sxml:context->node (car cntset2)))
                 (cdr cntset2)
                 cntset2)
             (cons (car cntset1) res)))
      ((eq? (sxml:context->node (car order))
            (sxml:context->node (car cntset2)))
       (loop (cdr order)
             cntset1
             (cdr cntset2)
             (cons (car cntset2) res)))
      (else
       (loop (cdr order) cntset1 cntset2 res))))))

;;! Based on the function for uniting 2 context-sets, unites multiple
;; context-sets
(define (ddo:unite-multiple-context-sets . context-sets)
  (if (null? context-sets)              ; nothing to do
      '()
      (let loop ((res (car context-sets))
                 (more (cdr context-sets)))
        (if (null? more)
            res
            (loop (ddo:unite-2-contextsets res (car more))
                  (cdr more))))))


;;-------------------------------------------------------------------------------
;;!! Optimizing special predicates like [position()=1] and the like

;;! Similar to R5RS list-tail, but returns an empty list when k > (length lst)
(define (ddo:list-tail lst k)
  (if (or (null? lst) (<= k 0))
      lst
      (ddo:list-tail (cdr lst) (- k 1))))

;;! Takes the first k members of the list
;; The whole list is taken when k > (length lst)
(define (ddo:list-head lst k)
  (if (or (null? lst) (<= k 0))
      '()
      (cons (car lst) (ddo:list-head (cdr lst) (- k 1)))))

;;! Similar to R5RS list-tail, but returns an empty list when
;; (or (< k 0) (> k (length lst))
(define (ddo:list-ref lst k)
  (cond ((null? lst) lst)
        ((zero? k) (car lst))
        (else (ddo:list-ref (cdr lst) (- k 1)))))

;;-------------------------------------------------------------------------------
;;!! Checks for a special structure of the predicate in its AST representation

;;! Checks whether the given op is the AST representation to a function call
;; to position()
(define ddo:check-ast-position?
  (let ((ddo:ast-for-position-fun-call  ; evaluate just once
         (txp:expr->ast "position()")))
    (lambda (op)
      (equal? op ddo:ast-for-position-fun-call))))

;;! If the given op is the AST representation for a number and this number is
;; exact, returns this number. Otherwise returns #f
(define (ddo:check4ast-number op)
  (if
   (eq? (car op) 'number)
   (let ((number (cadr op)))
     (if (and (number? number) (exact? number))
         number #f))
   #f))

;;!  In case when the predicate has one of the following forms:
;; SpecialPredicate ::= [ Number ]
;;                      | [ position() CmpOp Number ]
;;                      | [ Number CmpOp position() ]
;; CmpOp ::= > | < | >= | <= | =
;; Number - an integer
;;  than returns (lambda (nodeset) ...), where the lambda performs the required
;;  filtering as specified by the predicate.
;;  For a different sort of a predicate, returns #f
;;  The function doesn't signal of any semantic errors.
(define (ddo:check-special-predicate op)
  (if
   (not (eq? (car op) 'predicate))
   #f                                   ; an improper AST
   (let ((expr (cadr op)))
     (cond
      ((ddo:check4ast-number expr)
       => (lambda (num)
            (lambda (nodeset) (ddo:list-ref nodeset (- num 1)))))
      ((and (memq (car expr) '(= > < >= <=))
            (= (length expr) 3))
       (call-with-values
           (lambda ()
             (cond
              ((and (ddo:check-ast-position? (cadr expr))
                    (ddo:check4ast-number (caddr expr)))
               => (lambda (num) (values (car expr) num)))
              ((and (ddo:check-ast-position? (caddr expr))
                    (ddo:check4ast-number (cadr expr)))
               => (lambda (num)
                    (values
                     (cond              ; invert the cmp-op
                      ((assq (car expr)
                             '((< . >) (> . <) (>= . <=) (<= . >=)))
                       => cdr)
                      (else (car expr)))
                     num)))
              (else
               (values #f #f))))
         (lambda (cmp-op num)
           (if
            (not num)
            #f
            (case cmp-op
              ((=)
               (lambda (nodeset) (ddo:list-ref nodeset (- num 1))))
              ((>)
               (lambda (nodeset) (ddo:list-tail nodeset num)))
              ((>=)
               (lambda (nodeset) (ddo:list-tail nodeset (- num 1))))
              ((<)
               (lambda (nodeset) (ddo:list-head nodeset (- num 1))))
              ((<=)
               (lambda (nodeset) (ddo:list-head nodeset num)))
              (else                     ; internal error
               #f))))))
      (else      ; not an equality or relational expr with 2 arguments
       #f)))))


;;-------------------------------------------------------------------------------
;;!! Some simple rewrites for XPath AST

;;! Whether a given AST node is the representation of the location step
;; "descendant-or-self::node()", which is the full syntax for its abbreviated
;; equivalent "//"
(define ddo:check-ast-desc-os?
  (let ((ddo:ast-for-desc-os         ; evaluate just once
         (cadr                       ; selects the first location step
          (txp:xpath->ast "//dummy"))))
    (lambda (op)
      (equal? op ddo:ast-for-desc-os))))

;;! Rewrites the sequence of location steps, by combining the two consecutive
;; steps "//para" into a single one "descendant::para"
;; Returns the reconstructed list of steps
(define (ddo:rewrite-step* op-lst)
  (cond
   ((or (null? op-lst) (null? (cdr op-lst))) ; nothing to rewrite
    op-lst)
   ;; There are at least 2 steps in a sequence of steps
   ((and (ddo:check-ast-desc-os? (car op-lst))
         ;; Next step uses a child axis specifier
         (equal? (txp:step-axis (cadr op-lst)) '(child))
         ;; Next step doesn't use any predicates
         (null? (txp:step-preds (cadr op-lst))))
    (cons
     (txp:construct-step
      '(descendant)                     ; rewrite into descendant axis
      ;; Node test of the next step
      (txp:step-node-test (cadr op-lst)))
     (ddo:rewrite-step* (cddr op-lst))))
   (else                                ; Any other case
    (cons (car op-lst)
          (ddo:rewrite-step* (cdr op-lst))))))


;;-------------------------------------------------------------------------------
;;!! Optimization for deeply nested predicates
;; For predicates whose level of nesting exceeds 3, these predicates are likely
;; to be called for more than n^3 times, where n is the number of nodes in an
;; SXML document being processed. For such predicates, it is desirable to
;; evaluate them in advance, for every combination of context node, context
;; position and context size (the latter two components are not even required
;; if the predicate doesn't use position).
;; Such an optimization allows achieving a polinomial-time complexity for any
;; XPath expression

(define (ddo:generate-pred-id)
  (string->symbol
   (string-append "*predicate-" (symbol->string (gensym)) "*")))

;;-------------------------------------------------------------------------------
;;!! Search for predicate values
;; Predicate values are added to var-binding

;;! Predicate value for a predicate that doesn't require position
;; Predicate values are stored in the form of
;; pred-values ::= (listof  (cons  node  pred-value))
;; NOTE: A node (and not a context) is used as a key in the alist
(define (ddo:get-pred-value pred-id)
  (lambda (nodeset position+size var-binding)
    (cond
     ((not (and (nodeset? nodeset)
                (null? (cdr nodeset))))
      (sxml:xpointer-runtime-error
       "internal DDO SXPath error - "
       "a predicate is supplied with a non-singleton nodeset: " pred-id)
      #f)
     ((or (null? var-binding)
          (not (eq? (caar var-binding) '*var-vector*)))
      (sxml:xpointer-runtime-error
       "internal DDO SXPath error - predicate value not found: " pred-id)
      #f)
                                        ; predicate value as expected
     ((assq (sxml:context->node (car nodeset))
            (vector-ref (cdar var-binding) pred-id))
      => (lambda (pair) (force (cdr pair)))
                                        ; => cdr   ; DL: was
      )
     (else              ; predicate value for the given node not found
      (sxml:xpointer-runtime-error
       "internal DDO SXPath error - no predicate value for node: "
       pred-id (sxml:context->node (car nodeset)))
      #f))))

;;! Predicate value for a predicate that requires position
;; Predicate values are stored in the form of
;; pred-values ::=
;;        (listof
;;         (cons node
;;               (listof
;;                (cons size
;;                      (listof
;;                       (cons position pred-value))))))
;; NOTE: A node (and not a context) is used as a key in the alist
(define (ddo:get-pred-value-pos pred-id)
  (lambda (nodeset position+size var-binding)
    (cond
     ((not (and (nodeset? nodeset)
                (null? (cdr nodeset))))
      (sxml:xpointer-runtime-error
       "internal DDO SXPath error - "
       "a predicate is supplied with a non-singleton nodeset: " pred-id)
      #f)
     ((or (null? var-binding)
          (not (eq? (caar var-binding) '*var-vector*)))
      (sxml:xpointer-runtime-error
       "internal DDO SXPath error - predicate value not found: " pred-id)
      #f)
                                        ; predicate value as expected
     ((assq (sxml:context->node (car nodeset))
            (vector-ref (cdar var-binding) pred-id))
      => (lambda (size-pair)
           (if
            (> (cdr position+size)      ; context size
               (vector-length (cdr size-pair)))
            (begin
              (sxml:xpointer-runtime-error
               "internal DDO SXPath error - "
               "vector member for context size not found: " pred-id)
              #f)
            (let ((pos-vect (vector-ref (cdr size-pair)
                                        (- (cdr position+size) 1))))
              (if
               (> (car position+size)   ; context position
                  (vector-length pos-vect))
               (begin
                 (sxml:xpointer-runtime-error
                  "internal DDO SXPath error - "
                  "vector member for context position not found: "
                  pred-id)
                 #f)
               (force (vector-ref pos-vect
                                  (- (car position+size) 1))))))))
     (else              ; predicate value for the given node not found
      (sxml:xpointer-runtime-error
       "internal DDO SXPath error - no predicate value for node: "
       pred-id (sxml:context->node (car nodeset)))
      #f))))

;;! Value that results from evaluating the absolute location path
;; The argument is named `pred-id' for the sake of mere unification with
;; deep predicates
(define (ddo:get-abs-lpath-value pred-id)
  (lambda (nodeset position+size var-binding)
    (if
     (or (null? var-binding)
         (not (eq? (caar var-binding) '*var-vector*)))
     (begin
       (sxml:xpointer-runtime-error
        "internal DDO SXPath error - "
        "value for absolute location path not found: " pred-id)
       ;; the value defaults to an empty nodeset
       '())
     (vector-ref (cdar var-binding) pred-id))))

;;-------------------------------------------------------------------------------
;;!! Construct predicate values

;;! Construct alist of values for a predicate that doesn't require position
;; pred-impl - lambda that implements the predicate
;; context-set - set of contexts for all nodes in the source document
;; var-bindings - include variables supplied by user and the ones formed by
;;  deeper level predicates
(define (ddo:construct-pred-values pred-impl context-set var-binding)
  (map
   (lambda (context)
     (cons (sxml:context->node context)
           (delay
             (sxml:boolean        ; since return type cannot be number
              (pred-impl (list context)
                         (cons 1 1)  ; dummy context position and size
                         var-binding)))))
   context-set))

;;! Construct alist of values for a predicate that requires position
;;  pred-impl - lambda that implements the predicate
;;  context-set - set of contexts for all nodes in the source document
;;  var-bindings - include variables supplied by user and the ones formed by
;; deeper level predicates
;;  max-size - maximal context size possible in the document
(define (ddo:construct-pred-values-pos pred-impl context-set var-binding max-size)
  (map
   (lambda (context)
     (cons
      (sxml:context->node context)
      (let ((context (list context)))
        (let iter-size ((size 1)
                        (size-lst '()))
          (if
           (> size max-size)            ; iteration is over
           (list->vector (reverse size-lst))
           (let iter-pos ((position 1)
                          (pos-lst '()))
             (if
              (> position size)         ; iteration is over
              (iter-size
               (+ size 1)
               (cons (list->vector (reverse pos-lst))
                     size-lst))
              (iter-pos
               (+ position 1)
               (cons
                (delay
                  (let ((pred-value
                         (pred-impl
                          context (cons position size) var-binding)))
                    (if (number? pred-value)
                        (= pred-value position)
                        (sxml:boolean pred-value))))
                pos-lst)))))))))
   context-set))

;;-------------------------------------------------------------------------------
;;!! Optimization for achieving constant access time to XPath variables

;;! Allocates the new vector from `vect' with the exception of position `k' which
;; is replaced with `obj'
(define (ddo:vector-copy-set vect k obj)
  (let loop ((src (vector->list vect))
             (pos 0)
             (res '()))
    (if
     (null? src)                        ; iteration is over
     (list->vector (reverse res))
     (loop (cdr src)
           (+ pos 1)
           (cons
            (if (= pos k) obj (car src))
            res)))))

;;! Extends `var-binding' with a vector data structure for binding variable
;; values and values for deep predicates.
;; Returns extended var-binding, which is constructed as follows:
;; (cons (cons '*var-vector* ,vector)
;;       var-binding)
(define (ddo:add-vector-to-var-binding
         vars2offsets deep-predicates doc var-binding)
  (let ((cons-var-vector              ; cons var-vector to var-binding
         (lambda (var-vector var-binding)
           (cons (cons '*var-vector* var-vector)
                 var-binding))))
    (if
     (and (null? deep-predicates) (null? var-binding))
     var-binding                        ; nothing to add
     (let* ((var-tree
             (if
              (< (length var-binding) 100) ; not too many variables
              #f                           ; do not need any tree
              (ddo:var-binding->tree var-binding)))
            (var-vector
             (let iter-offsets ((pos (- (car vars2offsets) 1))
                                (vars-alist (cdr vars2offsets))
                                (lst '()))
               (cond
                ((< pos 0)              ; iteration is over
                 (list->vector lst))
                ((or (null? vars-alist) ; no more vars in the alist
                     (not (= pos (cdar vars-alist))))
                 (iter-offsets (- pos 1)
                               vars-alist
                               (cons #f lst) ; cons a dummy value
                               ))
                (else ; this position is in the 1st member of vars-alist
                 (iter-offsets
                  (- pos 1)
                  (cdr vars-alist)
                  (cons
                   (cond ; more sophisticated way of searching for value
                    (var-tree      ; access variables through var-tree
                     (ddo:get-var-value-from-tree ; checks for declared var
                      (caar vars-alist) var-tree))
                    ((assq (caar vars-alist) var-binding)
                     => cdr)
                    (else
                     (sxml:xpointer-runtime-error "unbound variable - "
                                                  (cdar vars-alist))
                     '()))
                   lst)))))))
       (if
        (null? deep-predicates)
        (cons-var-vector var-vector var-binding)
        (let* ((context-set
                (if (null?              ; just absolute location paths
                     (ddo:filter
                      (lambda (triple)
                        (not (eq? (cadr triple) 'absolute-location-path)))
                      deep-predicates))
                    '()                 ; dummy
                    (ddo:all-contexts-in-doc doc)))
               (max-size
                (if ; position-required? for at least one deep predicate
                 (not (null? (ddo:filter cadr deep-predicates)))
                 (length context-set)
                 1                      ; dummy
                 )))
          (let iter-preds ((deep-predicates deep-predicates)
                           (var-vector var-vector))
            (if
             (null? deep-predicates)    ; iteration is over
             (cons-var-vector var-vector var-binding)
             (iter-preds
              (cdr deep-predicates)
              (ddo:vector-copy-set
               var-vector
               (caar deep-predicates)   ; pred-id
               (cond
                ((eq? (cadar deep-predicates) 'absolute-location-path)
                 ((caddar deep-predicates) ; absolute lpath impl
                  (as-nodeset doc)
                  (cons 1 1)         ; dummy context position and size
                  (cons-var-vector var-vector var-binding)))
                ((cadar deep-predicates) ; requires-position?
                 (ddo:construct-pred-values-pos
                  (caddar deep-predicates) ; pred-impl
                  context-set
                  (cons-var-vector var-vector var-binding)
                  max-size))
                (else
                 (ddo:construct-pred-values
                  (caddar deep-predicates) ; pred-impl
                  context-set
                  (cons-var-vector var-vector var-binding))))))))))))))


;;-------------------------------------------------------------------------------
;;!! Methods similar to radix sort for linear access time for all variables

;;! Represents a list of chars as a branch in the string-tree
;; The list of chars must be non-empty
(define (ddo:charlst->branch lst value)
  (if (null? (cdr lst))  ; this is the last character in the lst
      (list (car lst) (cons 'value value))
      `(,(car lst) #f ,(ddo:charlst->branch (cdr lst) value))))

;;! Adds a new string to string-tree
(define (ddo:add-var-to-tree var-name var-value tree)
  (letrec
      ((add-lst-to-tree               ; adds the list of chars to tree
        (lambda (lst tree)
          (if
           (null? lst)                  ; the lst is over
           (cons (car tree)
                 (cons (cons 'value var-value) ; replace variable value
                       (cddr tree)))
           (let ((curr-char (car lst)))
             (let iter-alist ((alist (cddr tree))
                              (res (list (cadr tree) (car tree))))
               (cond
                ((null? alist)          ; branch not in a tree
                 (reverse
                  (cons
                   (ddo:charlst->branch lst var-value)
                   res)))
                ((char=? (caar alist) curr-char) ; entry found
                 (if
                  (null? (cdr alist))   ; nothing more in the alist
                  (reverse
                   (cons
                    (add-lst-to-tree (cdr lst) (car alist))
                    res))
                  (append
                   (reverse
                    (cons
                     (add-lst-to-tree (cdr lst) (car alist))
                     res))
                   (cdr alist))))
                ((char>? (caar alist) curr-char)
                 (if
                  (null? (cdr alist))   ; nothing more in the alist
                  (reverse
                   (cons
                    (car alist)
                    (cons (ddo:charlst->branch lst var-value)
                          res)))
                  (append
                   (reverse
                    (cons
                     (ddo:charlst->branch lst var-value)
                     res))
                   alist)))
                (else
                 (iter-alist (cdr alist)
                             (cons (car alist) res))))))))))
    (add-lst-to-tree (string->list (symbol->string var-name))
                     tree)))

;;! Convert var-binding to their tree representation
;; var-binding is supposed to be non-null
(define (ddo:var-binding->tree var-binding)
  (let loop ((var-binding (cdr var-binding))
             (tree
              (list '*top*
                    #f
                    (ddo:charlst->branch
                     (string->list
                      (symbol->string (caar var-binding))) ; var name
                     (cdar var-binding)))))
    (if (null? var-binding)
        tree
        (loop (cdr var-binding)
              (ddo:add-var-to-tree
               (caar var-binding) (cdar var-binding) tree)))))

;;! Obtain variable value from the tree
(define (ddo:get-var-value-from-tree var-name tree)
  (let loop ((lst (string->list (symbol->string var-name)))
             (tree tree))
    (cond
     ((and (not (null? lst))
           (assv (car lst) (cddr tree)))
      => (lambda (new-tree)
           (loop (cdr lst) new-tree)))
     ;; lst is over
     ((and (null? lst)
           ;; value for variable in the tree supplied
           (cadr tree))
      (cdadr tree))
     (else
      (sxml:xpointer-runtime-error "unbound variable - " var-name)
      ;; dummy value
      '()))))


;;-------------------------------------------------------------------------------
;;!! XPath AST processing
;; AST is considered to be properly formed
;; In the signature of functions below, the following terms are taken:
;;  op - S-expression which represents the operation
;;  num-anc - how many ancestors are required in the context after that
;;   operation
;;
;; {5} <AxisSpecifier> ::= (axis-specifier  <AxisName> )
;; {6} <AxisName> ::= (ancestor)
;;                    | (ancestor-or-self)
;;                    | (attribute)
;;                    | (child)
;;                    | (descendant)
;;                    | (descendant-or-self)
;;                    | (following)
;;                    | (following-sibling)
;;                    | (namespace)
;;                    | (parent)
;;                    | (preceding)
;;                    | (preceding-sibling)
;;                    | (self)
;;
;; single-level? - whether all nodes in the input nodeset are located on the
;;  same level of tree hierarchy
;; requires-position? - whether context position or context size are required to
;;  filter the result produced by the axis
;;
;; For requires-position?=#f, the function returns
;;  (list  axis-lambda
;;         num-anc-it-requires
;;         single-level?)
;; For requires-position?=#t, the function returns
;;  (list  axis-lambda
;;         num-anc-it-requires
;;         single-level?
;;         pos-result?)
;;  single-level? - whether nodes are in the single level after the axis
;;  pos-result? - whether the result of the axis has the form of pos-result.
;;   If #f, the axis returns its result in the form of the common nodeset
(define (ddo:ast-axis-specifier op num-anc single-level? requires-position?)
  (cond
   ((not (eq? (car op) 'axis-specifier)) ; AST error
    (draft:signal-semantic-error "not an AxisSpecifier - " op))
   (requires-position?
    (case (caadr op)                    ; AxisName
      ((ancestor)
       (list ddo:ancestor-pos
             #f #f #t))
      ((ancestor-or-self)
       (list ddo:ancestor-or-self-pos
             #f #f #t))
      ((attribute)
       (list draft:attribute
             (draft:na-minus-nneg num-anc 1) single-level? #f))
      ((child)
       (if single-level?
           (list draft:child
                 (draft:na-minus-nneg num-anc 1) #t #f)
           (list ddo:child-pos
                 (draft:na-minus-nneg num-anc 1) #f #t)))
      ((descendant)
       (if single-level?
           (list draft:descendant
                 (draft:na-minus-nneg num-anc 1) #f #f)
           (list ddo:descendant-pos
                 (draft:na-minus-nneg num-anc 1) #f #t)))
      ((descendant-or-self)
       (if single-level?
           (list draft:descendant-or-self
                 num-anc #f #f)
           (list ddo:descendant-or-self-pos
                 num-anc #f #t)))
      ((following)
                                        ; DL: this is incorrect for single-level?=#f
       (list ddo:following-single-level-pos
             #f #f #t))
      ((following-sibling)
       (list (if single-level?
                 ddo:following-sibling-single-level-pos
                 ddo:following-sibling-pos)
             (draft:na-max num-anc 1) single-level? #t))
      ((namespace)
       (list draft:namespace
             (draft:na-minus-nneg num-anc 1) single-level? #f))
      ((parent)
       (list (if single-level? ddo:parent-single-level-pos ddo:parent-pos)
             (draft:na+ num-anc 1) single-level? #t))
      ((preceding)
                                        ; DL: this is incorrect for single-level?=#f
       (list ddo:preceding-single-level-pos
             #f #f #t))
      ((preceding-sibling)
       (list (if single-level?
                 ddo:preceding-sibling-single-level-pos
                 ddo:preceding-sibling-pos)
             (draft:na-max num-anc 1) single-level? #t))
      ((self)
       (list draft:self num-anc single-level? #f))
      (else
       (draft:signal-semantic-error "unknown AxisName - " op))))
   (else                            ; doesn't require to keep position
    (case (caadr op)                ; AxisName
      ((ancestor)
       (list ddo:ancestor #f #f))
      ((ancestor-or-self)
       (list ddo:ancestor-or-self #f #f))
      ((attribute)
       (list draft:attribute
             (draft:na-minus-nneg num-anc 1) single-level?))
      ((child)
       (list (if single-level? draft:child ddo:child)
             (draft:na-minus-nneg num-anc 1) single-level?))
      ((descendant)
       (list (if single-level? draft:descendant ddo:descendant)
             (draft:na-minus-nneg num-anc 1) #f))
      ((descendant-or-self)
       (list (if single-level?
                 draft:descendant-or-self ddo:descendant-or-self)
             num-anc #f))
      ((following)
       (list (if single-level? ddo:following-single-level ddo:following)
             #f #f))
      ((following-sibling)
       (list (if single-level?
                 ddo:following-sibling-single-level ddo:following-sibling)
             (draft:na-max num-anc 1) single-level?))
      ((namespace)
       (list draft:namespace
             (draft:na-minus-nneg num-anc 1) single-level?))
      ((parent)
       (list (if single-level? ddo:parent-single-level ddo:parent)
             (draft:na+ num-anc 1) single-level?))
      ((preceding)
       (list (if single-level? ddo:preceding-single-level ddo:preceding)
             #f #f))
      ((preceding-sibling)
       (list (if single-level?
                 ddo:preceding-sibling-single-level ddo:preceding-sibling)
             (draft:na-max num-anc 1) single-level?))
      ((self)
       (list draft:self num-anc single-level?))
      (else
       (draft:signal-semantic-error "unknown AxisName - " op))))))

;; {7} <NodeTest> ::= (node-test (*))
;;                    | (node-test (namespace-uri  <String> ))
;;                    | (node-test (namespace-uri  <String> )?
;;                                 (local-name  <String> ))
;;                    | (node-test (comment))
;;                    | (node-test (text))
;;                    | (node-test (pi <String>? ))
;;                    | (node-test (point))
;;                    | (node-test (range))
;; For processing a node test, 'draft:ast-node-test' from "xpath-context.scm"
;; can be used

;;-------------------------------------------------------------------------------
;;!! In this section, each function accepts 5 arguments
;;  op - S-expression which represents the operation
;;  num-anc - how many ancestors are required in the context after that
;;   operation
;;  single-level? - for grammar rules that consume the nodeset type as input:
;;   whether all nodes in the nodeset are located on the single level of the
;;   tree hierarchy. If this is the case, most axes can be evaluated ealier than
;;   in the general case.
;;  pred-nesting - nesting of the expression being processed within predicates.
;;   In particular, pred-nesting=0 denotes the outer expression, pred-nesting=1
;;   denotes the expression enclosed into a predicate, pred-nesting=2 for an
;;   expression that is enclosed into 2 predicates, etc
;;  vars2offsets - mapping from variable names to their offsets in a var-vector
;;  vars2offsets ::= (cons vacant-offset
;;                         (listof (cons var-name var-offset))
;;  vacant-offset - a number, initially starts from 0
;;  var-offset - offset for a particular variable within a vector
;
;; AST processing functions return either #f, which signals of a
;; semantic error, or
;;  (list (lambda (nodeset position+size var-binding) ...)
;;        num-anc-it-requires
;;        single-level?
;;        requires-position?
;;        expr-type
;;        deep-predicates
;;        vars2offsets )
;;  position+size - the same to what was called 'context' in TXPath-1
;;  requires-position? - whether position() or last() functions are encountered
;;   in the internal expression
;;  expr-type - the type returned by the expression being process. The type is
;;   determined by symbols. Possible types: number, string, boolean, nodeset and
;;   any
;;  deep-predicates - an associative list that contains deeply nested predicates,
;;   whose pred-nesting>3:
;;  deep-predicates ::= (listof (list  pred-id  requires-position?  impl))
;;  pred-id - a symbol that identifies the predicate among others
;;  impl - the implementation for this predicate

;;! {1} <LocationPath> ::= <RelativeLocationPath>
;;                         | <AbsoluteLocationPath>
(define (ddo:ast-location-path
         op num-anc single-level? pred-nesting vars2offsets)
  (case (car op)
    ((absolute-location-path)
     (ddo:ast-absolute-location-path
      op num-anc single-level? pred-nesting vars2offsets))
    ((relative-location-path)
     (ddo:ast-relative-location-path
      op num-anc single-level? pred-nesting vars2offsets))
    (else
     (draft:signal-semantic-error "improper LocationPath - " op))))

;;! {2} <AbsoluteLocationPath> ::= (absolute-location-path  <Step>* )
;; NOTE: single-level? is dummy here, since AbsoluteLocationPath always
;; starts from a single node - the root of the document
(define (ddo:ast-absolute-location-path
         op num-anc single-level? pred-nesting vars2offsets)
  (cond
   ((not (eq? (car op) 'absolute-location-path))
    (draft:signal-semantic-error "not an AbsoluteLocationPath - " op))
   ((null? (cdr op))                    ; no Steps
    (list
     (lambda (nodeset position+size var-binding)
       (draft:reach-root nodeset))
     #f                                 ; requires all ancestors
     #t                                 ; on single level
     #f                                 ; doesn't require position
     ddo:type-nodeset
     '()                                ; no deep predicates
     vars2offsets))
   (else
    (and-let*
     ((steps-res (ddo:ast-step-list
                  (cdr op) num-anc #t pred-nesting vars2offsets)))
     (let ((impl        ; implementation of the absolute location path
            (if
             (null? (cdar steps-res))   ; only a single step
             (let ((step-impl (caar steps-res)))
               (lambda (nodeset position+size var-binding)
                 (step-impl
                  (draft:reach-root nodeset) position+size var-binding)))
             (let ((converters (car steps-res)))
               (lambda (nodeset position+size var-binding)
                 (let rpt ((nset (draft:reach-root nodeset))
                           (fs converters))
                   (if (null? fs)
                       nset
                       (rpt ((car fs) nset position+size var-binding)
                            (cdr fs)))))))))
       (if
        (> pred-nesting 0) ; absolute location path inside a predicate
        (let ((vars2offsets (list-ref steps-res 6)))
          (list
           (ddo:get-abs-lpath-value (car vars2offsets))
           #f                           ; all ancestors required
           (caddr steps-res)            ; single-level
           #f                           ; doesn't require position
           ddo:type-nodeset
           (cons
            (list (car vars2offsets)      ; identifier
                  'absolute-location-path ; flag to denote absolute lpath
                  impl)
            ;; deep-predicates
            (list-ref steps-res 5))
           (cons (+ (car vars2offsets) 1)
                 (cdr vars2offsets))))
        (cons impl
              (cons #f                  ; all ancestors required
                    ;; the remaining parameters
                    (cddr steps-res)))))))))

;;! {3} <RelativeLocationPath> ::= (relative-location-path  <Step>+ )
(define (ddo:ast-relative-location-path
         op num-anc single-level? pred-nesting vars2offsets)
  (if
   (not (eq? (car op) 'relative-location-path))
   (draft:signal-semantic-error "not a RelativeLocationPath - " op)
   (and-let*
    ((steps-res
      (ddo:ast-step-list
       (cdr op) num-anc single-level? pred-nesting vars2offsets)))
    (cons
     (if
      (null? (cdar steps-res))          ; only a single step
      (caar steps-res)
      (let ((converters (car steps-res)))
        (lambda (nodeset position+size var-binding)
          (let rpt ((nset nodeset)
                    (fs converters))
            (if (null? fs)
                nset
                (rpt ((car fs) nset position+size var-binding)
                     (cdr fs)))))))
     ;; the remaining parameters
     (cdr steps-res)))))

;;! {4} <Step> ::= (step  <AxisSpecifier> <NodeTest> <Predicate>* )
;;                 | (range-to  (expr <Expr>)  <Predicate>* )
(define (ddo:ast-step op num-anc single-level? pred-nesting vars2offsets)
  (cond
   ((eq? (car op) 'range-to)
    (draft:signal-semantic-error "range-to function not implemented"))
   ((eq? (car op) 'filter-expr)
    (ddo:ast-filter-expr op num-anc single-level? pred-nesting vars2offsets))
   ((eq? (car op) 'lambda-step)         ; created by sxpath
    (let ((proc (cadr op)))
      (list
       (if
        (and num-anc (zero? num-anc))   ; no ancestors required
        (lambda (nodeset position+size var-binding)
          (proc (draft:contextset->nodeset (as-nodeset nodeset))
                (if (and (pair? var-binding) ; non-null
                         (eq? (caar var-binding) '*var-vector*))
                    (cdr var-binding) var-binding)))
        (lambda (nodeset position+size var-binding)
          (draft:find-proper-context
           (proc (draft:contextset->nodeset (as-nodeset nodeset))
                 (if (and (pair? var-binding) ; non-null
                          (eq? (caar var-binding) '*var-vector*))
                     (cdr var-binding) var-binding))
           (map sxml:context->content   ; TODO: should add variables
                (as-nodeset nodeset))
           num-anc)))
       num-anc                         ; num-ancestors
       #f                              ; single-level? after this step
       #f                              ; position-required?
       ddo:type-any
       '()                              ; no deep predicates
       vars2offsets
       )))
   ((eq? (car op) 'step)
    (if
     (null? (cdddr op))                 ; no Predicates
     (and-let*
      ((axis-lst (ddo:ast-axis-specifier
                  (cadr op) num-anc single-level? #f))
       (ntest (draft:ast-node-test (caddr op))))
      (let ((axis ((car axis-lst) ntest num-anc)))
        (list
         (lambda (nodeset position+size var-binding)
           (axis nodeset))
         (cadr axis-lst)
         (caddr axis-lst)
         #f
         ddo:type-nodeset
         '()                            ; no deep predicates
         vars2offsets
         )))
                                        ; There are Predicates
     (and-let*
      ((preds-res (ddo:ast-predicate-list
                   (cdddr op) 0 #t (+ pred-nesting 1) vars2offsets))
       (preds-res
        (if (and (list-ref preds-res 3) ; position required for the predicate
                 (< pred-nesting 3))    ; level of nesting matters
            (ddo:ast-predicate-list     ; the second pass
             (cdddr op) 0 #t
             (+ pred-nesting 2) ; called for quadratic number of times
             vars2offsets
             )
            preds-res                 ; do not need to change anything
            ))
       (axis-lst (ddo:ast-axis-specifier
                  (cadr op)
                  (draft:na-max num-anc (cadr preds-res))
                  single-level?
                  (list-ref preds-res 3) ; whether position required
                  ))
       (ntest (draft:ast-node-test (caddr op))))
      (let ((axis ((car axis-lst)
                   ntest (draft:na-max num-anc (cadr preds-res))))
            (pred-impl-lst (car preds-res)))
        (list
         (cond
          ((not (list-ref preds-res 3)) ; whether position required
           (ddo:location-step-non-pos axis pred-impl-lst))
          ((list-ref axis-lst 3)        ; pos-result?
           (ddo:location-step-pos axis pred-impl-lst))
          (else                         ; non-intersect
           (ddo:location-step-non-intersect axis pred-impl-lst)))
         (cadr axis-lst)               ; num-ancestors
         (caddr axis-lst)              ; single-level? after this step
         #f                            ; position-required?
         ddo:type-nodeset
         (list-ref preds-res 5)         ; deep predicates
         ;; new var-binding
         (list-ref preds-res 6))))))
   (else
    (draft:signal-semantic-error "not a Step - " op))))

;;! {4a} ( <Step>+ )
;; Returns (list (listof step-impl)
;;               num-anc single-level? requires-position? expr-type
;;               deep-predicates vars2offsets)
;; or #f
;; TECHNICAL NOTE: To calculate 'single-level?', we need to process steps in
;; straight orger. To calculate 'num-anc', we need to process steps in reverse
;; order. This thus has to be implemented in 2 passes
(define (ddo:ast-step-list
         step-lst num-anc single-level? pred-nesting vars2offsets)
  (let ((step-lst (ddo:rewrite-step* step-lst))
        ;; Calculates single-level? for each step in the step-lst
        ;; Returns: (listof single-level?)
        ;; where each member of the REVERSED result list corresponds to the step
        ;; in the corresponding position of a step-lst
        ;; We can notice that when single-level?=#f for some step, it remains
        ;; #f for all the subsequent steps
        (calculate-single-level
         (lambda (step-lst single-level?)
           (let iter-steps ((steps step-lst)
                            (sl? single-level?)
                            (res '()))
             (cond
              ((null? steps) res)
              ((or (memq (caar steps) '(range-to filter-expr lambda-step))
                   (not sl?))
               ;; #f for the remaining steps
               (append (map
                        (lambda (step) #f)
                        steps)          ; DL: was: step-lst
                       res))
              (else      ; evaluate single-level? for the current step
               (and-let*
                ((axis-lst (ddo:ast-axis-specifier
                            (cadar steps) ; is to be axis specifier
                            0 sl? #f)))
                (iter-steps (cdr steps)
                            (caddr axis-lst) ; single-level for next step
                            (cons sl? res)))))))))
    (and-let*
     ((single-level-lst (calculate-single-level step-lst single-level?)))
     (let loop ((steps-to-view (reverse step-lst))
                (sl?-lst single-level-lst)
                (res-lst '())
                (num-anc num-anc)
                (deep-predicates '())
                (vars2offsets vars2offsets))
       (if
        (null? steps-to-view)           ; everyone processed
        (list res-lst
              num-anc (car single-level-lst) #f
              ddo:type-nodeset deep-predicates vars2offsets)
        (and-let*
         ((step-res
           (ddo:ast-step
            (car steps-to-view) num-anc (car sl?-lst)
            pred-nesting vars2offsets)))
         (loop
          (cdr steps-to-view)
          (cdr sl?-lst)
          (cons (car step-res) res-lst)
          (cadr step-res)
          (append (list-ref step-res 5) deep-predicates)
          ;; new vars2offsets
          (list-ref step-res 6))))))))

;;! {8} <Predicate> ::= (predicate  <Expr> )
;; NOTE: num-anc is dummy here, since it is always 0 for Predicates
;; NOTE: single-level? is dummy here, since a Predicate is always called for
;;  a single node to be filtered
;; NOTE: Unlike 'draft:ast-predicate', we don't implement any filtering here,
;;  because it depends on the particular axis in the step. Filtering is
;;  performed on the higher level
(define (ddo:ast-predicate op num-anc single-level? pred-nesting vars2offsets)
  (if
   (not (eq? (car op) 'predicate))
   (draft:signal-semantic-error "not an Predicate - " op)
   (and-let*
    ((expr-res (ddo:ast-expr (cadr op) 0 #t pred-nesting vars2offsets)))
    (let ((requires-position?
           (or (cadddr expr-res) ; predicate expression requires position
               (memq (list-ref expr-res 4) ; involves position implicitly
                     '(ddo:type-number ddo:type-any))))
          (vars2offsets (list-ref expr-res 6)))
      (call-with-values
          (lambda ()
            (if
             (or                        ; this is a deep predicate
              ;; DL: theoretically reasonable although impractical condition:
              ;;(and (not requires-position?) (> pred-nesting 1))
              (> pred-nesting 3))
             (let ((pred-id (car vars2offsets))) ; was: (ddo:generate-pred-id)
               (values
                ((if requires-position?
                     ddo:get-pred-value-pos ddo:get-pred-value)
                 pred-id)
                (cons
                 (list pred-id
                       requires-position?
                       ;; implementation
                       (car expr-res))
                 ;; deep-predicates
                 (list-ref expr-res 5)
                 )
                (cons (+ (car vars2offsets) 1)
                      (cdr vars2offsets))))
             (values (car expr-res)     ; implementation
                     (list-ref expr-res 5)
                     vars2offsets)))
        (lambda (pred-impl deep-predicates vars2offsets)
          (list pred-impl
                (cadr expr-res)        ; num-ancestors required
                (caddr expr-res)       ; single-level? - we don't care
                requires-position?
                (list-ref expr-res 4)   ; return type
                deep-predicates
                vars2offsets)))))))

;;! {8a} ( <Predicate>+ )
;; Returns (list (listof pred-impl)
;;               num-anc single-level? requires-position? expr-type
;;               deep-predicates)
;; or #f
;; NOTE: num-anc is dummy here, since it is always 0 for Predicates
;; NOTE: single-level? is dummy here, since a Predicate is always called for
;;  a single node to be filtered
;; NOTE: information about the type for each Predicate is lost
(define (ddo:ast-predicate-list
         op-lst num-anc single-level? pred-nesting vars2offsets)
  (let ((pred-res-lst
         (ddo:foldr
          (lambda (op init)
            (cons
             (ddo:ast-predicate
              op 0 #t pred-nesting
              (if (or (null? init)      ; called for the first time
                      (not (car init)))
                  vars2offsets
                  (list-ref (car init) 6) ; vars2offsets from previous pred
                  ))
             init))
          '()
          op-lst)))
    (and
     (not (memv #f pred-res-lst))       ; error detected
     (list (map car pred-res-lst)
           (apply draft:na-max (map cadr pred-res-lst))
           #t
           (apply ddo:or (map cadddr pred-res-lst))
           ddo:type-any
           (apply append                ; deep-predicates
                  (map
                   (lambda (pred-res) (list-ref pred-res 5))
                   pred-res-lst))
           (list-ref (car pred-res-lst) 6))))) ; vars2offsets

;;! {9} <Expr> ::= <OrExpr>
;;                 | <AndExpr>
;;                 | <EqualityExpr>
;;                 | <RelationalExpr>
;;                 | <AdditiveExpr>
;;                 | <MultiplicativeExpr>
;;                 | <UnionExpr>
;;                 | <PathExpr>
;;                 | <FilterExpr>
;;                 | <VariableReference>
;;                 | <Literal>
;;                 | <Number>
;;                 | <FunctionCall>
;;                 | <LocationPath>
(define (ddo:ast-expr op num-anc single-level? pred-nesting vars2offsets)
  (case (car op)
    ((or)
     (ddo:ast-or-expr op num-anc single-level? pred-nesting vars2offsets))
    ((and)
     (ddo:ast-and-expr op num-anc single-level? pred-nesting vars2offsets))
    ((= !=)
     (ddo:ast-equality-expr op num-anc single-level? pred-nesting vars2offsets))
    ((< > <= >=)
     (ddo:ast-relational-expr
      op num-anc single-level? pred-nesting vars2offsets))
    ((+ -)
     (ddo:ast-additive-expr op num-anc single-level? pred-nesting vars2offsets))
    ((* div mod)
     (ddo:ast-multiplicative-expr
      op num-anc single-level? pred-nesting vars2offsets))
    ((union-expr)
     (ddo:ast-union-expr op num-anc single-level? pred-nesting vars2offsets))
    ((path-expr)
     (ddo:ast-path-expr op num-anc single-level? pred-nesting vars2offsets))
    ((filter-expr)
     (ddo:ast-filter-expr op num-anc single-level? pred-nesting vars2offsets))
    ((variable-reference)
     (ddo:ast-variable-reference
      op num-anc single-level? pred-nesting vars2offsets))
    ((literal)
     (ddo:ast-literal op num-anc single-level? pred-nesting vars2offsets))
    ((number)
     (ddo:ast-number op num-anc single-level? pred-nesting vars2offsets))
    ((function-call)
     (ddo:ast-function-call op num-anc single-level? pred-nesting vars2offsets))
    ((absolute-location-path)
     (ddo:ast-absolute-location-path
      op num-anc single-level? pred-nesting vars2offsets))
    ((relative-location-path)
     (ddo:ast-relative-location-path
      op num-anc single-level? pred-nesting vars2offsets))
    (else
     (draft:signal-semantic-error "unknown Expr - " op))))

;;! Applies AST processing to a list of operations
(define (ddo:apply-ast-procedure
         ast-procedure op-lst num-anc single-level? pred-nesting vars2offsets)
  (ddo:foldr
   (lambda (expr init)
     (cons
      (ast-procedure
       expr num-anc single-level? pred-nesting
       (if (or (null? init)     ; called for the first time
               (not (car init)) ; error during previously processed expr
               )
           vars2offsets
           (list-ref (car init) 6)   ; vars2offsets from previous expr
           ))
      init))
   '()
   op-lst))

;;! {10} <OrExpr> ::= (or <Expr> <Expr>+ )
;; NOTE: num-anc is dummy here, since it is always 0 for OrExpr
(define (ddo:ast-or-expr op num-anc single-level? pred-nesting vars2offsets)
  (let ((expr-res-lst
         (ddo:apply-ast-procedure
          ddo:ast-expr
          (cdr op) 0 single-level? pred-nesting vars2offsets)))
    (and
     (not (memv #f expr-res-lst))       ; error detected
     (let ((expr-impls (map car expr-res-lst)))
       (list
        (lambda (nodeset position+size var-binding)
          (let rpt ((fs expr-impls))
            (cond
             ((null? fs) #f)
             ((sxml:boolean ((car fs) nodeset position+size var-binding)) #t)
             (else (rpt (cdr fs))))))
        (apply draft:na-max (map cadr expr-res-lst)) ; num-ancestors
        #t                             ; single-level? after this step
        (apply ddo:or (map cadddr expr-res-lst)) ; position-required?
        ddo:type-boolean
        (apply append                   ; deep-predicates
               (map
                (lambda (expr-res) (list-ref expr-res 5))
                expr-res-lst))
        (list-ref (car expr-res-lst) 6)))))) ; vars2offsets

;;! {11} <AndExpr> ::= (and <Expr> <Expr>+ )
;; NOTE: num-anc is dummy here, since it is always 0 for AndExpr
(define (ddo:ast-and-expr op num-anc single-level? pred-nesting vars2offsets)
  (let ((expr-res-lst
         (ddo:apply-ast-procedure
          ddo:ast-expr
          (cdr op) 0 single-level? pred-nesting vars2offsets)))
    (and
     (not (memv #f expr-res-lst))       ; error detected
     (let ((expr-impls (map car expr-res-lst)))
       (list
        (lambda (nodeset position+size var-binding)
          (let rpt ((fs expr-impls))
            (cond
             ((null? fs) #t)
             ((not
               (sxml:boolean ((car fs) nodeset position+size var-binding)))
              #f)
             (else (rpt (cdr fs))))))
        (apply draft:na-max (map cadr expr-res-lst)) ; num-ancestors
        #t                             ; single-level? after this step
        (apply ddo:or (map cadddr expr-res-lst)) ; position-required?
        ddo:type-boolean
        (apply append                   ; deep-predicates
               (map
                (lambda (expr-res) (list-ref expr-res 5))
                expr-res-lst))
        (list-ref (car expr-res-lst) 6)))))) ; vars2offsets

;;! {12} <EqualityExpr> ::= (=  <Expr> <Expr> )
;;                         | (!=  <Expr> <Expr> )
;; NOTE: num-anc is dummy here, since it is always 0 for EqualityExpr
(define (ddo:ast-equality-expr
         op num-anc single-level? pred-nesting vars2offsets)
  (and-let*
   ((left-lst
     (ddo:ast-expr (cadr op) 0 single-level? pred-nesting vars2offsets))
    (right-lst
     (ddo:ast-expr (caddr op) 0 single-level? pred-nesting
                   (list-ref left-lst 6) ; vars2offsets for left part
                   )))
   (let ((cmp-op (cadr (assq (car op) `((= ,sxml:equal?)
                                        (!= ,sxml:not-equal?)))))
         (left (car left-lst))
         (right (car right-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (cmp-op
         (draft:contextset->nodeset
          (left nodeset position+size var-binding))
         (draft:contextset->nodeset
          (right nodeset position+size var-binding))))
      (draft:na-max (cadr left-lst) (cadr right-lst)) ; num-ancestors
      #t                               ; single-level? after this step
      (or (cadddr left-lst) (cadddr right-lst)) ; position-required?
      ddo:type-boolean
      (append (list-ref left-lst 5)     ; deep-predicates
              (list-ref right-lst 5))
      ;; vars2offsets for right part
      (list-ref right-lst 6)))))

;;! {13} <RelationalExpr> ::= (<  <Expr> <Expr> )
;;                            | (>  <Expr> <Expr> )
;;                            | (<=  <Expr> <Expr> )
;;                            | (>=  <Expr> <Expr> )
;; NOTE: num-anc is dummy here, since it is always 0 for RelationalExpr
(define (ddo:ast-relational-expr
         op num-anc single-level? pred-nesting vars2offsets)
  (and-let*
   ((left-lst
     (ddo:ast-expr (cadr op) 0 single-level? pred-nesting vars2offsets))
    (right-lst
     (ddo:ast-expr (caddr op) 0 single-level? pred-nesting
                   ;; vars2offsets for left part
                   (list-ref left-lst 6))))
   (let ((cmp-op
          (sxml:relational-cmp
           (cadr (assq (car op) `((< ,<) (> ,>) (<= ,<=) (>= ,>=))))))
         (left (car left-lst))
         (right (car right-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (cmp-op
         (draft:contextset->nodeset
          (left nodeset position+size var-binding))
         (draft:contextset->nodeset
          (right nodeset position+size var-binding))))
      (draft:na-max (cadr left-lst) (cadr right-lst)) ; num-ancestors
      #t                               ; single-level? after this step
      (or (cadddr left-lst) (cadddr right-lst)) ; position-required?
      ddo:type-boolean
      (append (list-ref left-lst 5)     ; deep-predicates
              (list-ref right-lst 5))
      (list-ref right-lst 6)))))        ; vars2offsets for right part

;;! {14} <AdditiveExpr> ::= (+  <Expr> <Expr> )
;;                         | (-  <Expr> <Expr>? )
;; NOTE: num-anc is dummy here, since it is always 0 for AdditiveExpr
(define (ddo:ast-additive-expr
         op num-anc single-level? pred-nesting vars2offsets)
  (let ((expr-res-lst
         (ddo:apply-ast-procedure
          ddo:ast-expr
          (cdr op) 0 single-level? pred-nesting vars2offsets)))
    (and
     (not (memv #f expr-res-lst))       ; error detected
     (let ((add-op (cadr (assq (car op) `((+ ,+) (- ,-)))))
           (expr-impls (map car expr-res-lst)))
       (list
        (lambda (nodeset position+size var-binding)
          (apply
           add-op
           (map
            (lambda (expr)
              (sxml:number
               (draft:contextset->nodeset
                (expr nodeset position+size var-binding))))
            expr-impls)))
        (apply draft:na-max (map cadr expr-res-lst)) ; num-ancestors
        #t                             ; single-level? after this step
        (apply ddo:or (map cadddr expr-res-lst)) ; position-required?
        ddo:type-number
        (apply append                   ; deep-predicates
               (map
                (lambda (expr-res) (list-ref expr-res 5))
                expr-res-lst))
        ;; vars2offsets
        (list-ref (car expr-res-lst) 6))))))

;;! {15} <MultiplicativeExpr> ::= (*  <Expr> <Expr> )
;;                                | (div  <Expr> <Expr> )
;;                                | (mod  <Expr> <Expr> )
;; NOTE: num-anc is dummy here, since it is always 0 for MultiplicativeExpr
(define (ddo:ast-multiplicative-expr
         op num-anc single-level? pred-nesting vars2offsets)
  (and-let*
   ((left-lst
     (ddo:ast-expr (cadr op) 0 single-level? pred-nesting vars2offsets))
    (right-lst
     (ddo:ast-expr (caddr op) 0 single-level? pred-nesting
                   (list-ref left-lst 6) ; vars2offsets for left part
                   )))
   (let ((mul-op
          (sxml:relational-cmp
           (cadr (assq (car op) `((* ,*) (div ,/) (mod ,remainder))))))
         (left (car left-lst))
         (right (car right-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (mul-op
         (sxml:number
          (draft:contextset->nodeset
           (left nodeset position+size var-binding)))
         (sxml:number
          (draft:contextset->nodeset
           (right nodeset position+size var-binding)))))
      (draft:na-max (cadr left-lst) (cadr right-lst)) ; num-ancestors
      #t                               ; single-level? after this step
      (or (cadddr left-lst) (cadddr right-lst)) ; position-required?
      ddo:type-number
      (append (list-ref left-lst 5)     ; deep-predicates
              (list-ref right-lst 5))
      ;; vars2offsets for right part
      (list-ref right-lst 6)))))

;;! {16} <UnionExpr> ::= (union-expr  <Expr> <Expr>+ )
;; TECHNICAL NOTE: For implementing the union while supporting distinct document
;; order, we need num-ancestors=#f for the arguments of the union-expr. This
;; operation is time-consuming and should be avoided
(define (ddo:ast-union-expr op num-anc single-level? pred-nesting vars2offsets)
  (let ((expr-res-lst
         (ddo:foldr
          (lambda (expr init)
            (let ((expr-res
                   (if
                    (or (null? init)    ; called for the first time
                        (not (car init)))
                    (ddo:ast-expr
                     expr num-anc      ; not necessarily all ancestors
                     single-level? pred-nesting vars2offsets)
                    (ddo:ast-expr
                     expr #f single-level? pred-nesting
                     (list-ref (car init) 6) ; vars2offsets from previous expr
                     ))))
              (cons
               (if
                (not (or (eq? (list-ref expr-res 4) ddo:type-nodeset)
                         (eq? (list-ref expr-res 4) ddo:type-any)))
                (draft:signal-semantic-error
                 "expression to be unioned evaluates to a non-nodeset - "
                 expr)
                expr-res)
               init)))
          '()
          (cdr op))))
    (and
     (not (memv #f expr-res-lst))       ; error detected
     (let ((expr-impls (map car expr-res-lst)))
       (list
        (lambda (nodeset position+size var-binding)
          (let rpt ((res '())
                    (fs expr-impls))
            (if
             (null? fs)
             res
             (let ((nset ((car fs) nodeset position+size var-binding)))
               (rpt
                (ddo:unite-2-contextsets
                 res
                 (cond
                  ((not (nodeset? nset))
                   (sxml:xpointer-runtime-error
                    "expected - nodeset instead of " nset)
                   '())
                  (else nset)))
                (cdr fs))))))
        #f                             ; num-ancestors
        #f                             ; single-level? after this step
        (apply ddo:or (map cadddr expr-res-lst)) ; position-required?
        ddo:type-nodeset
        (apply append                   ; deep-predicates
               (map
                (lambda (expr-res) (list-ref expr-res 5))
                expr-res-lst))
        ;; vars2offsets
        (list-ref (car expr-res-lst) 6))))))

;;! {17} <PathExpr> ::= (path-expr  <FilterExpr> <Step>+ )
;; TECHNICAL NOTE: To calculate 'single-level?', we need to process components
;; in straight orger. To calculate 'num-anc', we need to process steps in
;; reverse order. It is too expensive to make the 2 passes, that's why we
;; consider single-level?=#f for steps
(define (ddo:ast-path-expr op num-anc single-level? pred-nesting vars2offsets)
  (and-let*
   ((steps-res (ddo:ast-step-list
                (cddr op) num-anc
                #f        ; consider single-level?=#f after FilterExpr
                pred-nesting
                vars2offsets))
    (filter-lst (ddo:ast-filter-expr
                 (cadr op)
                 (cadr steps-res)       ; num-ancestors
                 single-level?
                 pred-nesting
                 ;; vars2offsets from steps-list
                 (list-ref steps-res 6))))
   (if
    (not (or (eq? (list-ref filter-lst 4) ddo:type-nodeset)
             (eq? (list-ref filter-lst 4) ddo:type-any)))
    (draft:signal-semantic-error
     "location steps are applied to a non-nodeset result - " (cadr op))
    (let ((init-impl (car filter-lst))
          (converters (car steps-res)))
      (list
       (lambda (nodeset position+size var-binding)
         (let ((nset
                (init-impl nodeset position+size var-binding)))
           (let rpt ((nset
                      (cond
                       ((nodeset? nset) nset)
                       (else
                        (sxml:xpointer-runtime-error
                         "expected - nodeset instead of " nset)
                        '())))
                     (fs converters))
             (if (null? fs)
                 nset
                 (rpt ((car fs) nset position+size var-binding)
                      (cdr fs))))))
       (cadr filter-lst)        ; num-ancestors
       (cadddr steps-res)       ; single-level?, =#f in our assumption
       (cadddr filter-lst)      ; position-required?
       ddo:type-nodeset
       (append (list-ref filter-lst 5)  ; deep-predicates
               (list-ref steps-res 5))
       ;; vars2offsets from filter-lst
       (list-ref filter-lst 6))))))

;;! {18} <FilterExpr> ::= (filter-expr (primary-expr  <Expr> )
;;                                     <Predicate>* )
(define (ddo:ast-filter-expr op num-anc single-level? pred-nesting vars2offsets)
  (cond
   ((not (eq? (car op) 'filter-expr))
    (draft:signal-semantic-error "not an FilterExpr - " op))
   ((not (eq? (caadr op) 'primary-expr))
    (draft:signal-semantic-error "not an PrimaryExpr - " (cadr op)))
   ((null? (cddr op))                   ; no Predicates
    (ddo:ast-expr (cadadr op) num-anc single-level? pred-nesting vars2offsets))
   ((and (null? (cdddr op))             ; a single predicate
         (ddo:check-special-predicate (caddr op)))
    => (lambda (special-pred-impl)
         (and-let*
          ((expr-lst (ddo:ast-expr
                      (cadadr op)
                      num-anc ; special predicate doesn't require ancestors
                      single-level? pred-nesting vars2offsets)))
          (list
           (ddo:filter-expr-special-predicate
            (car expr-lst) special-pred-impl)
           (cadr expr-lst)             ; num-ancestors
           (caddr expr-lst)            ; single-level? after this step
           (cadddr expr-lst)           ; position-required?
           ddo:type-nodeset
           (list-ref expr-lst 5)        ; deep-predicates
           ;; vars2offsets
           (list-ref expr-lst 6)))))
   (else                                ; the general case
    (and-let*
     ((preds-res (ddo:ast-predicate-list
                  (cddr op) 0 #t (+ pred-nesting 1) vars2offsets))
      (expr-lst (ddo:ast-expr
                 (cadadr op)
                 (draft:na-max num-anc (cadr preds-res)) ; num-anc
                 single-level? pred-nesting
                 (list-ref preds-res 6) ; vars2offsets from predicates
                 )))
     (if
      (not (or (eq? (list-ref expr-lst 4) ddo:type-nodeset)
               (eq? (list-ref expr-lst 4) ddo:type-any)))
      (draft:signal-semantic-error
       "expression to be filtered evaluates to a non-nodeset - " (cadr op))
      (let ((expr-impl (car expr-lst))
            (pred-impl-lst (car preds-res)))
        (list
         (if
          (list-ref preds-res 3)        ; position required
          (ddo:filter-expr-general expr-impl pred-impl-lst)
          (ddo:filter-expr-non-pos expr-impl pred-impl-lst))
         (cadr expr-lst)               ; num-ancestors
         (caddr expr-lst)              ; single-level? after this step
         (cadddr expr-lst)             ; position-required?
         ddo:type-nodeset
         (append (list-ref expr-lst 5)  ; deep-predicates
                 (list-ref preds-res 5))
         ;; vars2offsets from expr-lst
         (list-ref expr-lst 6))))))))

;;! {19} <VariableReference> ::= (variable-reference  <String> )
(define (ddo:ast-variable-reference
         op num-anc single-level? pred-nesting vars2offsets)
  (let ((name (string->symbol (cadr op))))
    (call-with-values
        (lambda ()
          (cond
           ((assq name (cdr vars2offsets)) ; this variable already in alist
            => (lambda (pair)
                 (values (cdr pair) vars2offsets)))
           (else                        ; this is a new variable
            (values (car vars2offsets)
                    (cons
                     (+ (car vars2offsets) 1)
                     (cons (cons name (car vars2offsets))
                           (cdr vars2offsets)))))))
      (lambda (var-offset new-vars2offsets)
        (list
         (lambda (nodeset position+size var-binding)
           (cond
            ((and (not (null? var-binding))
                  (eq? (caar var-binding) '*var-vector*))
             (vector-ref (cdar var-binding) var-offset))
                                        ; For backward compatibility
            ((assq name var-binding)
             => cdr)
            (else
             (sxml:xpointer-runtime-error "unbound variable - " name)
             '())))
         0
         #t       ; ATTENTION: in is not generally on the single-level
         #f
         ddo:type-any           ; type cannot be statically determined
         '()                    ; deep-predicates
         new-vars2offsets)))))

;;! {20} <Literal> ::= (literal  <String> )
(define (ddo:ast-literal op num-anc single-level? pred-nesting vars2offsets)
  (let ((literal (cadr op)))
    (list
     (lambda (nodeset position+size var-binding) literal)
     0 #t #f ddo:type-string '() vars2offsets)))

;;! {21} <Number> :: (number  <Number> )
(define (ddo:ast-number op num-anc single-level? pred-nesting vars2offsets)
  (let ((number (cadr op)))
    (list
     (lambda (nodeset position+size var-binding) number)
     0 #t #f ddo:type-number '() vars2offsets)))

;;! {22} <FunctionCall> ::= (function-call (function-name  <String> )
;;                                        (argument  <Expr> )* )
(define (ddo:ast-function-call
         op num-anc single-level? pred-nesting vars2offsets)
  (let ((core-alist
         ;; (list fun-name min-num-args max-num-args na4res impl
         ;;       single-level? requires-position? expr-type)
         `((last 0 0 0 ,draft:core-last
                 #t #t ,ddo:type-number)
           (position 0 0 0 ,draft:core-position
                     #t #t ,ddo:type-number)
           (count 1 1 0 ,draft:core-count
                  #t #f ,ddo:type-number)
           (id 1 1 #f ,draft:core-id
               #f #f ,ddo:type-nodeset)
           (local-name 0 1 0 ,draft:core-local-name
                       #t #f ,ddo:type-string)
           (namespace-uri 0 1 0 ,draft:core-namespace-uri
                          #t #f ,ddo:type-string)
           (name 0 1 0 ,draft:core-name
                 #t #f ,ddo:type-string)
           (string 0 1 0 ,draft:core-string
                   #t #f ,ddo:type-string)
           (concat 2 -1 0 ,draft:core-concat
                   #t #f ,ddo:type-string)
           (starts-with 2 2 0 ,draft:core-starts-with
                        #t #f ,ddo:type-boolean)
           (contains 2 2 0 ,draft:core-contains
                     #t #f ,ddo:type-boolean)
           (substring-before 2 2 0 ,draft:core-substring-before
                             #t #f ,ddo:type-boolean)
           (substring-after 2 2 0 ,draft:core-substring-after
                            #t #f ,ddo:type-boolean)
           (substring 2 3 0 ,draft:core-substring
                      #t #f ,ddo:type-boolean)
           (string-length 0 1 0 ,draft:core-string-length
                          #t #f ,ddo:type-number)
           (normalize-space 0 1 0 ,draft:core-normalize-space
                            #t #f ,ddo:type-string)
           (translate 3 3 0 ,draft:core-translate
                      #t #f ,ddo:type-string)
           (boolean 1 1 0 ,draft:core-boolean
                    #t #f ,ddo:type-boolean)
           (not 1 1 0 ,draft:core-not
                #t #f ,ddo:type-boolean)
           (true 0 0 0 ,draft:core-true
                 #t #f ,ddo:type-boolean)
           (false 0 0 0 ,draft:core-false
                  #t #f ,ddo:type-boolean)
           (lang 1 1 #f ,draft:core-lang
                 #t #f ,ddo:type-boolean)
           (number 0 1 0 ,draft:core-number
                   #t #f ,ddo:type-number)
           (sum 1 1 0 ,draft:core-sum
                #t #f ,ddo:type-number)
           (floor 1 1 0 ,draft:core-floor
                  #t #f ,ddo:type-number)
           (ceiling 1 1 0 ,draft:core-ceiling
                    #t #f ,ddo:type-number)
           (round 1 1 0 ,draft:core-round
                  #t #f ,ddo:type-number))))
    (cond
     ((not (eq? (caadr op) 'function-name))
      (draft:signal-semantic-error "not an FunctionName - " (cadr op)))
     ((assq (string->symbol (cadadr op)) core-alist)
      => (lambda (description)               ; Core function found
           (cond
            ((< (length (cddr op)) (cadr description))
             (draft:signal-semantic-error
              "too few arguments for the Core Function call - "
              (cadadr op)))
            ((and (>= (caddr description) 0)
                  (> (length (cddr op)) (caddr description)))
             (draft:signal-semantic-error
              "too many arguments for the Core Function call - "
              (cadadr op)))
            (else                       ; correct number of arguments
             (and-let*
              ((args-impl-lst (ddo:ast-function-arguments
                               (cddr op) ; list of arguments
                               single-level? pred-nesting vars2offsets)))
              (list
                                        ; Producing a function implementation
               (apply (list-ref description 4)
                      num-anc
                      (map car args-impl-lst))
               (apply            ; num-ancestors required for function
                draft:na-max
                (cons
                 (list-ref description 3) ; from function description
                 (map cadr args-impl-lst) ; from arguments
                 ))
               (list-ref description 5)     ; single-level?
               (or (list-ref description 6) ; position-required?
                   (not (null?
                         (ddo:filter cadddr args-impl-lst))))
               (list-ref description 7) ; return type
               (apply append            ; deep-predicates
                      (map
                       (lambda (arg-res) (list-ref arg-res 5))
                       args-impl-lst))
               (if (null? args-impl-lst) ; no arguments
                   vars2offsets
                   (list-ref (car args-impl-lst) 6))))))))
     (else                             ; function definition not found
      (draft:signal-semantic-error
       "function call to an unknown function - " (cadadr op))))))

;;! {22a} ( (argument  <Expr> )* )
;; na-lst - number of ancestors required for each of the arguments
;; Returns:  #f  or
;;  (listof
;;    (list expr-impl num-anc single-level? requires-position? expr-type
;;          deep-predicates vars2offsets))
;; NOTE: In XPath Core Function Library, none of the function arguments
;; is required to save any ancestors in the context
(define (ddo:ast-function-arguments
         op-lst single-level? pred-nesting vars2offsets)
  (let ((arg-res-lst
         (ddo:foldr
          (lambda (op init)
            (cons
             (if
              (not (eq? (car op) 'argument))
              (draft:signal-semantic-error "not an Argument - " op)
              (ddo:ast-expr
               (cadr op) 0 single-level? pred-nesting
               (if (or (null? init)     ; called for the first time
                       (not (car init)))
                   vars2offsets
                   (list-ref (car init) 6) ; vars2offsets from previous pred
                   )))
             init))
          '()
          op-lst)))
    (and
     (not (memv #f arg-res-lst))        ; semantic error detected
     arg-res-lst)))


;;-------------------------------------------------------------------------------
;;!! Highest level API functions
;;
;; procedure ddo:sxpath :: query [ns-binding] [num-ancestors] ->
;;                          -> node-or-nodeset [var-binding] -> nodeset
;; procedure ddo:txpath :: location-path [ns-binding] [num-ancestors] ->
;;                          -> node-or-nodeset [var-binding] -> nodeset
;;
;; Polynomial-time XPath implementation with distinct document order support.
;;
;; The API is identical to the API of a context-based SXPath (here we even use
;; API helpers from "xpath-context.scm"). For convenience, below we repeat
;; comments for the API (borrowed from "xpath-context.scm").
;;
;; query - a query in SXPath native syntax
;; location-path - XPath location path represented as a string
;; ns-binding - declared namespace prefixes (an optional argument)
;;  ns-binding ::= (listof (prefix . uri))
;;  prefix - a symbol
;;  uri - a string
;; num-ancestors - number of ancestors required for resulting nodeset. Can
;;  generally be omitted and is than defaulted to 0, which denotes a
;;  _conventional_  nodeset. If a negative number, this signals that all
;;  ancestors should be remembered in the context.
;;
;; Returns: (lambda (node-or-nodeset . var-binding) ...)
;; var-binding - XPath variable bindings (an optional argument)
;;  var-binding = (listof (var-name . value))
;;  var-name - (a symbol) a name of a variable
;;  value - its value. The value can have the following type: boolean, number,
;;  string, nodeset. NOTE: a node must be represented as a singleton nodeset.
;;
;; The result of applying the latter lambda to an SXML node or nodeset is the
;; result of evaluating the query / location-path for that node / nodeset.

;;! Helper for constructing several highest-level API functions
;; ns+na - can contain 'ns-binding' and/or 'num-ancestors' and/or none of them
(define (ddo:api-helper grammar-parser ast-parser)
  (lambda (xpath-string . ns+na)
    (call-with-values
        (lambda () (draft:arglist->ns+na ns+na))
      (lambda (ns-binding num-anc)
        (and-let*
         ((ast (grammar-parser xpath-string ns-binding))
          (impl-lst (ast-parser ast num-anc
                                #t ; we suppose single-level?=#t for src
                                0  ; predicate nesting is zero
                                '(0)))) ; initial vars2offsets
         (let ((impl-lambda
                (if
                 (and num-anc (zero? num-anc))
                 (let ((impl-car (car impl-lst)))
                   (lambda (node position+size var-binding)
                     (draft:contextset->nodeset
                      (impl-car node position+size var-binding))))
                 (car impl-lst))))
           (lambda (node . var-binding)      ; common implementation
             (impl-lambda
              (as-nodeset node)
              (cons 1 1)
              (ddo:add-vector-to-var-binding
               (list-ref impl-lst 6)    ; vars2offsets
               (reverse             ; deep-predicates: need to reverse
                (list-ref impl-lst 5))
               node
               (if (null? var-binding) var-binding (car var-binding)))))))))))

(define ddo:txpath (ddo:api-helper txp:xpath->ast ddo:ast-location-path))

(define ddo:xpath-expr (ddo:api-helper txp:expr->ast ddo:ast-expr))

(define ddo:sxpath (ddo:api-helper txp:sxpath->ast ddo:ast-expr))
