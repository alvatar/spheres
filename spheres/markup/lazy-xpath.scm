;;!!! This module implements lazy SXPath evaluation over lazy SXML documents
;;
;; This software is in Public Domain.
;; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;;
;; Please send bug reports and comments to:
;;   lizorkin@hotbox.ru    Dmitry Lizorkin
;;
;; In a lazy SXML document, each node may be a promise. If forced, the promise
;; results into an SXML node or a nodeset. For a nodeset, its members are SXML
;; nodes and promises in turn.
;; With every promise forced, a lazy SXML document must conform to SXML
;; Specification. In particular, an attribute node must occur before any child
;; nodes, attribute value must be atomic, etc.
;;
;; SXPath evaluation is lazy in that it results to a nodeset whose last member
;; may be a promise. Such a nodeset with a promise as its last member denotes
;; the first portion of the result. If the promise is forced, it is evaluated
;; into another nodeset, which corresponds to the next portion of the result.
;; SXPath evaluator returns the result in portions when some branch in the
;; document is to be forced in order to obtain the next part of the result.
;; However, a portion that is not the last one, must contain at least one result
;; node. To fulfill this requirement, branches of the document may be forced
;; until at least a result node for a portion is obtained.

;;! Implement 'or' as a function, so that we could 'apply' it
(define (lazy-xpath:or . args)
  (if (null? args) #f (or (car args) (apply lazy-xpath:or (cdr args)))))

;;! Predicate for detecting a promise
;; There is no such a predicate in R5RS, so different Scheme implementations
;; use different names for this functionality
(define lazy-xpath:promise?
  (cond-expand
   (plt promise?)
   (bigloo procedure?) ; ATTENTION: returns #t in more general situations
   (chicken
    ;; Thanks to Zbigniew Szadkowski <zbigniewsz@gmail.com>
    ;; for the insight of this function
    (lambda (p) ((chk:sys-structure?) p 'promise)))
   (gambit
    ##promise?)
   (else
    ;; ATTENTION: just makes the approach applicable for conventional SXML documents
    (lambda (obj) #f))))


;;-------------------------------------------------------------------------------
;;!! Lazy analogues for common list operations

;;! Checks whether the nodeset is empty
;; Note that a promise can evaluate to an empty list, and thus a nodeset
;; consisting of promises only may potentially be empty
(define (lazy-xpath:null? nodeset)
  (cond
   ((null? nodeset) #t)
   ((not (null? (xlink:filter      ; contains at least one non-promise
                 (lambda (node) (not (lazy-xpath:promise? node)))
                 nodeset)))
    #f)
   (else                            ; all nodeset members are promises
    (let iter-promises ((nset nodeset))
      (cond
       ((null? nset) #t)
       ((lazy-xpath:null? (as-nodeset (force (car nset))))
        (iter-promises (cdr nset)))
       (else #f))))))

;;! Like conventional map, but applicable to a lazy nodeset
(define (lazy-xpath:map func nodeset)
  (cond
   ((null? nodeset)                     ; iteration is over
    nodeset)
   ((lazy-xpath:promise? (car nodeset))
    (list
     (delay
       (lazy-xpath:map func
                       (append (as-nodeset (force (car nodeset)))
                               (cdr nodeset))))))
   (else                                ; the first member is a node
    (cons (func (car nodeset))
          (lazy-xpath:map func (cdr nodeset))))))

;;! Lazy analogue for filter
(define (lazy-xpath:filter func nodeset)
  (cond
   ((null? nodeset)                     ; iteration is over
    nodeset)
   ((lazy-xpath:promise? (car nodeset))
    (list
     (delay
       (lazy-xpath:filter func
                          (append (as-nodeset (force (car nodeset)))
                                  (cdr nodeset))))))
   ;; the first member is a node
   ((func (car nodeset))
    (cons (car nodeset)
          (lazy-xpath:filter func (cdr nodeset))))
   (else              ; the first member doesn't satisfy the predicate
    (lazy-xpath:filter func (cdr nodeset)))))

;;! Like conventional car, but for a lazy nodeset
(define (lazy-xpath:car nodeset)
  (cond
   ;; Checking for a safe variant
   ;;((null? nodeset)  ; failed
   ;; #f)
   ((lazy-xpath:promise? (car nodeset))
    (let ((nset-car (force (car nodeset))))
      (lazy-xpath:car
       ((if (nodeset? nset-car) append cons)
        nset-car (cdr nodeset)))))
   (else
    (car nodeset))))

;;! Like conventional cdr
(define (lazy-xpath:cdr nodeset)
  (if
   (lazy-xpath:promise? (car nodeset))
   (let ((nset-car (force (car nodeset))))
     (lazy-xpath:cdr
      ((if (nodeset? nset-car) append cons)
       nset-car (cdr nodeset)))))
  (cdr nodeset))

;;! Like conventional length, but for a lazy nodeset
;; ATTENTION: it has to force all the nodeset members in order to determine
;; the length properly
(define (lazy-xpath:length nodeset)
  (cond
   ((null? nodeset) 0)
   ((lazy-xpath:promise? (car nodeset))
    (let ((nset-car (force (car nodeset))))
      (lazy-xpath:length
       ((if (nodeset? nset-car) append cons)
        nset-car (cdr nodeset)))))
   (else
    (+ 1 (lazy-xpath:length (cdr nodeset))))))

;;! Converts the lazy result into a list, by forcing all the promises one by one
(define (lazy-xpath:result->list nodeset)
  (let iter-nset ((nset nodeset)
                  (res '()))
    (cond
     ((null? nset)                      ; finished scanning
      (reverse res))
     ((lazy-xpath:promise? (car nset))
      (iter-nset (append (as-nodeset (force (car nset))) (cdr nset))
                 res))
     (else                              ; the first member is a node
      (iter-nset (cdr nset)
                 (cons (car nset) res))))))

;;! Converts the lazy node to SXML, by forcing all of its descendants
;; The node itself is not a promise
(define (lazy-xpath:node->sxml node)
  (letrec
      ((force-nodeset
        (lambda (nodeset)
          (cond
           ((null? nodeset) nodeset)
           ((lazy-xpath:promise? (car nodeset))
            (let ((nset-car (force (car nodeset))))
              (force-nodeset
               ((if (nodeset? nset-car) append cons)
                nset-car (cdr nodeset)))))
           (else
            (cons (lazy-xpath:node->sxml (car nodeset))
                  (force-nodeset (cdr nodeset))))))))
    (if
     (or (not (pair? node))
         (null? ((sxml:descendant lazy-xpath:promise?) node)))
     node                           ; will not make a copy of the node
     (cons (car node) (force-nodeset (cdr node))))))

;;! Reaches the root of the root of the contextset
;; Result: singleton nodeset
(define (lazy-xpath:reach-root contextset)
  (letrec
      ((find-root
        (lambda (src prev-result)
          (let loop ((src src)
                     (res '())
                     (prev-result prev-result))
            (cond
             ((null? src)               ; nothing more to do
              (reverse res))
             ((lazy-xpath:promise? (car src))
              (if
               (null? res)              ; need to force this
               (loop (append (as-nodeset (force (car src)))
                             (cdr src))
                     res prev-result)
               (reverse
                (cons (delay (find-root src prev-result))
                      res))))
             (else                    ; (car src) is the ordinary node
              (let ((rt (if (sxml:context? (car src))
                            (draft:list-last
                             (sxml:context->ancestors-u (car src)))
                            (car src))))
                (loop (cdr src)
                      (if
                       (memq rt prev-result) ; already returned
                       res (cons rt res))
                      (cons rt prev-result)))))))))
    (find-root contextset '())))

;;! Analogue for draft:contextset->nodeset for the lazy case
(define (lazy-xpath:contextset->nodeset obj)
  (letrec
      ((iter-nset
        (lambda (nset)
          (cond
           ((null? nset) nset)
           ((lazy-xpath:promise? (car nset))
            (list
             (delay (iter-nset (append (as-nodeset (force (car nset)))
                                       (cdr nset))))))
           (else                        ; (car nset) is a node
            (cons
             (sxml:context->node (car nset))
             (iter-nset (cdr nset))))))))
    (if
     (nodeset? obj)
     (iter-nset obj)
     obj)))

;;! Lazy analogue for draft:recover-contextset
(define (lazy-xpath:recover-contextset nodeset root-node num-anc)
  (cond
   ((null? nodeset)                     ; nothing more to do
    '())
   ((lazy-xpath:promise? (car nodeset))
    (delay (lazy-xpath:recover-contextset
            (append (as-nodeset (force (car nodeset)))
                    (cdr nodeset))
            root-node num-anc)))
   (else                              ; (car nodeset) is a common node
    (cons
     (draft:smart-make-context
      (car nodeset)
      (((sxml:ancestor (lambda (x) #t)) root-node) (car nodeset))
      num-anc)
     (lazy-xpath:recover-contextset (cdr nodeset) root-node num-anc)))))

;;! Makes a context-set from a nodeset supplied, with the num-anc required
;; Members of the nodeset are known to be descendants-or-selves of
;; (map sxml:context->node context-set)
(define (lazy-xpath:find-proper-context nodeset context-set num-anc)
  (let* ((descend (lazy-xpath:descendant-or-self sxml:node? num-anc))
         (possible-ancestors
          (map
           cdr        ; ignore starting '*CONTEXT* for a faster search
           (map-union
            (lambda (node)
              ;; Has to be evaluated in the active manner, since all of the
              ;; candidates generally have to be scanned
              (lazy-xpath:result->list (descend node)))
                                        ;(lazy-xpath:result->list ancestors-set)
            (map-union
             sxml:context->ancestors
             (lazy-xpath:result->list context-set))))))
    (let iter-nset ((nodeset nodeset)
                    (res '()))          ; DL: was: res
      (cond
       ((null? nodeset)                 ; scanning is over
        (reverse res))
       ((lazy-xpath:promise? (car nodeset))
        (if (null? res)        ; result is still null => have to force
            (iter-nset (append (as-nodeset (force (car nodeset)))
                               (cdr nodeset))
                       res)
            (reverse
             (cons
              (delay (iter-nset (append (as-nodeset (force (car nodeset)))
                                        (cdr nodeset))
                                '()))
              res))))
       ((sxml:context? (car nodeset))   ; already a context
        (iter-nset (cdr nodeset)
                   (cons (car nodeset) res)))
       ((assq (car nodeset) possible-ancestors)
        => (lambda (ancestors)
             (iter-nset (cdr nodeset)
                        (cons
                         (draft:make-context
                          (car ancestors) ; = (car nodeset)
                          (cdr ancestors))
                         res))))
       (else                        ; this is a newly constructed node
                                        ; Keep it as is
        (iter-nset (cdr nodeset)
                   (cons (car nodeset) res)))))))


;;-------------------------------------------------------------------------------
;;!! Axes

;;! A helper that tests sibling nodes with respect to the test-pred? and returns
;; them in the lazy manner. Each of the siblings may be a promise.
;; This function is applied by axes: child, attribute, namespace,
;; following-sibling, preceding-sibling
(define (lazy-xpath:output-siblings test-pred? siblings ancestors)
  (letrec
      ((iterate-siblings
        (lambda (src)
          (let loop ((src src) (res '()))
            (cond
             ((null? src)               ; iteration is over
              (reverse res))
             ((lazy-xpath:promise? (car src))
              (reverse  ; otherwise - return the result with a promise
               (cons
                (delay
                  (iterate-siblings
                   (append (as-nodeset (force (car src))) (cdr src))))
                res)))
             (else                      ; the first src is a node
              (loop (cdr src)
                    (if (test-pred? (car src))
                        (cons
                         (if (null? ancestors) ; don't construct context
                             (car src)
                             (draft:make-context (car src) ancestors))
                         res)
                        res))))))))
    (iterate-siblings siblings)))

;;! Returns nodeset membert that are following siblings of the given node
;; Nodeset members may be promises
(define (lazy-xpath:find-foll-siblings node nodeset)
  (cond
   ((null? nodeset)                     ; not found
    '())
   ((lazy-xpath:promise? (car nodeset))
    (lazy-xpath:find-foll-siblings
     node
     (append (as-nodeset (force (car nodeset)))
             (cdr nodeset))))
   ;; (car nodeset) an ordinary node
   ((eq? node (car nodeset))
    (cdr nodeset))
   (else
    (lazy-xpath:find-foll-siblings node (cdr nodeset)))))

;;! Returns nodeset members that are preceding siblings of the given node
;; Nodeset members may be promises
(define (lazy-xpath:find-prec-siblings node nodeset)
  (let loop ((nodeset nodeset)
             (res '()))
    (cond
     ((null? nodeset)                   ; not found
      '())
     ((lazy-xpath:promise? (car nodeset))
      (loop
       (append (as-nodeset (force (car nodeset)))
               (cdr nodeset))
       res))
     ;; the first member in a nodeset an ordinary node
     ((eq? node (car nodeset))
      res)
     (else
      (loop (cdr nodeset)
            (cons (car nodeset) res))))))


;;-------------------------------------------------------------------------------
;;!! Axis functions
;; They have the signature of:
;;  test-pred? [num-ancestors] -> Node -> Nodeset
;; Note that each axis function produces the function to be applied to a single
;; _node_, not to a nodeset

;;! Ancestor axis
;; It should be noted that ancestors of the context node are already forced
(define (lazy-xpath:ancestor test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; not a nodeset
      (if
       (sxml:context? node)
       (let loop ((ancs-to-view (sxml:context->ancestors-u node))
                  (res '()))
         (cond
          ((null? ancs-to-view)      ; processed everyone
           (reverse res))            ; reverse document order required
          ((test-pred? (car ancs-to-view)) ; can add it to result
           (loop
            (cdr ancs-to-view)
            (cons
             (draft:smart-make-context
              (car ancs-to-view) (cdr ancs-to-view) num-anc)
             res)))
          (else           ; current node doesn't satisfy the predicate
           (loop (cdr ancs-to-view) res))))
       '()))))                          ; no ancestors

;;! Ancestor-or-self axis
;; It should be noted that ancestors of the context node are already forced
(define (lazy-xpath:ancestor-or-self test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; not a nodeset
      (cond
       ((sxml:context? node)
        (let loop ((ancs-to-view (sxml:context->content-u node))
                   (res '()))
          (cond
           ((null? ancs-to-view)    ; processed everyone
            ;; reverse document order required
            (reverse res))
           ((test-pred? (car ancs-to-view)) ; can add it to result
            (loop
             (cdr ancs-to-view)
             (cons
              (draft:smart-make-context
               (car ancs-to-view) (cdr ancs-to-view) num-anc)
              res)))
           (else          ; current node doesn't satisfy the predicate
            (loop (cdr ancs-to-view) res)))))
       ;; ordinary SXML node
       ((test-pred? node)               ; satisfies the predicate
        (list node))
       (else
        '())))))

;;! Attribute axis
(define (lazy-xpath:attribute test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (letrec
        ((find-attr-node
          ;; Either returns an attribute node, or #f
          ;; Nodeset members may be promises
          (lambda (nodeset)
            (cond
             ((null? nodeset)           ; failed
              #f)
             ((lazy-xpath:promise? (car nodeset))
              (find-attr-node
               (append (as-nodeset (force (car nodeset)))
                       (cdr nodeset))))
             ;; (car nodeset) an ordinary node
             (((ntype?? '@) (car nodeset))
              (car nodeset))
             (else #f)))))
      (lambda (node)                         ; not a nodeset
        (cond
         ((not (pair? node)) '())     ; no attributes
         ;; (car node) is always a symbol
         ((sxml:context-u? node)        ; a context node
          (let ((attr-node (find-attr-node (sxml:context->node-u node))))
            (if (not attr-node)         ; not found
                '()
                ((lazy-xpath:child test-pred? num-anc)
                 (if (and num-anc (zero? num-anc))
                     attr-node
                     (draft:make-context
                      attr-node (sxml:context->content-u node)))))))
         (else                       ; an ordinary node, and is a pair
          (let ((attr-node (find-attr-node node)))
            (if (not attr-node)         ; not found
                '()
                ((lazy-xpath:child test-pred? num-anc)
                 (if (and num-anc (zero? num-anc))
                     attr-node
                     (draft:make-context attr-node (list node))))))))))))

;;! Child axis
(define (lazy-xpath:child test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; not a nodeset
      (cond
       ((not (pair? node))              ; no children
        '())
       ;; (car node) is always a symbol
       ((sxml:context-u? node)          ; a context node
        (let ((this (sxml:context->node-u node)))
          (if
           (or (not (pair? this))
               (memq (car this) '(*PI* *COMMENT* *ENTITY*)))
           '()                          ; no children
           (lazy-xpath:output-siblings
            test-pred?
            (cdr this)                  ; gives its children
            (draft:list-head (sxml:context->content-u node) num-anc)))))
       ;; an ordinary node, and is a pair
       ((memq (car node) '(*PI* *COMMENT* *ENTITY*))
        '())
       (else
        (lazy-xpath:output-siblings
         test-pred?
         (cdr node)                     ; gives its children
         (if (and num-anc (zero? num-anc))
             '() (list node))))))))

;;! Descendant axis
(define (lazy-xpath:descendant test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (lazy-xpath:child sxml:node? num-anc)))
    (lambda (node)                           ; not a nodeset
      (let rpt ((res '())
                (more (child node)))
        (cond
         ((null? more)                  ; no more candidates
          (reverse res))
         ((lazy-xpath:promise? (car more)) ; need to force it
          (reverse
           (cons
            (delay (rpt '()
                        (append (as-nodeset (force (car more)))
                                (cdr more))))
            res)))
         (else                          ; first in more is a node
          (rpt (if (test-pred? (sxml:context->node (car more)))
                   (cons (car more) res)
                   res)
               (append (child (car more)) (cdr more)))))))))

;;! Descendant-or-self axis
(define (lazy-xpath:descendant-or-self test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (lazy-xpath:child sxml:node? num-anc)))
    (lambda (node)                           ; not a nodeset
      (let rpt ((res '())
                (more (list node)))
        (cond
         ((null? more)                  ; no more candidates
          (reverse res))
         ((lazy-xpath:promise? (car more)) ; need to force it
          (reverse
           (cons
            (delay (rpt '()
                        (append (as-nodeset (force (car more)))
                                (cdr more))))
            res)))
         (else                          ; first in more is a node
          (rpt (if (test-pred? (sxml:context->node (car more)))
                   (cons (car more) res)
                   res)
               (append (child (car more)) (cdr more)))))))))

;;! Following axis
(define (lazy-xpath:following test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (descend (lazy-xpath:descendant-or-self test-pred? num-anc)))
    (lambda (node)                           ; not a nodeset
      (if
       (sxml:context? node)
       (let loop ((curr-node (sxml:context->node-u node))
                  (ancs-to-view (sxml:context->ancestors-u node))
                  (foll-siblings '())
                  (descendants '())
                  (res '()))
         (cond
          ((null? descendants)          ; candidates for result
           (cond
            ((null? foll-siblings) ; no more siblings of the curr-node
             (if
              (null? ancs-to-view)      ; processed everyone
              (reverse res)
              (loop (car ancs-to-view)
                    (cdr ancs-to-view)
                    (lazy-xpath:find-foll-siblings
                     curr-node
                     (cdr ; parent is an element => cdr gives its children
                      (car ancs-to-view)))
                    '()
                    res)))
            ((lazy-xpath:promise? (car foll-siblings))
             (reverse
              (cons
               (delay
                 (loop curr-node ancs-to-view
                       (append (as-nodeset (force (car foll-siblings)))
                               (cdr foll-siblings))
                       '() '()))
               res)))
            (else                      ; (car foll-siblings) is a node
             (loop curr-node ancs-to-view
                   (cdr foll-siblings)
                   (descend           ; descendants are currently null
                    (draft:smart-make-context
                     (car foll-siblings)
                     ancs-to-view num-anc))
                   res))))
          ((lazy-xpath:promise? (car descendants)) ; need to force descendant axis
           (reverse
            (cons
             (delay
               (loop curr-node ancs-to-view foll-siblings
                     (append (as-nodeset (force (car descendants)))
                             (cdr descendants))
                     '()))
             res)))
          (else                   ; the first in descendants is a node
           (loop curr-node ancs-to-view foll-siblings
                 (cdr descendants) (cons (car descendants) res)))))
       ;; no following members
       '()))))

;;! Following-sibling axis
(define (lazy-xpath:following-sibling test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; not a nodeset
      (if
       (and (sxml:context? node)
            (not (null? (sxml:context->ancestors-u node))))
       (lazy-xpath:output-siblings
        test-pred?
        (lazy-xpath:find-foll-siblings
         (sxml:context->node-u node)
         (cdr         ; parent is an element => cdr gives its children
          (car (sxml:context->ancestors-u node))))
        (draft:list-head
         (sxml:context->ancestors-u node) num-anc))
       ;; no parent => no siblings
       '()))))

;;! Namespace axis
;; Since a namespace axis somewhat redundant for SXML, we'll provide a
;; not-very-effective implementation for it
(define (lazy-xpath:namespace test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; not a nodeset
      (lazy-xpath:filter
       (lambda (context)
         (test-pred? (sxml:context->node context)))
       ((lazy-xpath:sxpath '(@@ *NAMESPACES* *) num-anc) node)))))

;;! Parent axis
;; It should be noted that the parent of the context node is already forced
(define (lazy-xpath:parent test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; not a nodeset
      (if
       (and (sxml:context? node)
            (not (null? (sxml:context->ancestors-u node)))
            (test-pred? (car (sxml:context->ancestors-u node))))
       (draft:smart-make-context
        (car (sxml:context->ancestors-u node))
        (cdr (sxml:context->ancestors-u node))
        num-anc)
       '()))))                          ; no parent

;;! Preceding axis
(define (lazy-xpath:preceding test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (descend (lazy-xpath:descendant-or-self test-pred? num-anc)))
    (lambda (node)                           ; not a nodeset
      (if
       (sxml:context? node)
       (let loop ((curr-node (sxml:context->node-u node))
                  (ancs-to-view (sxml:context->ancestors-u node))
                  (prec-siblings '())
                  (descendants '())
                  (res '()))
         (cond
          ((null? descendants)          ; candidates for result
           (cond
            ((null? prec-siblings) ; no more siblings of the curr-node
             (if
              (null? ancs-to-view)      ; processed everyone
              (reverse res)
              (loop (car ancs-to-view)
                    (cdr ancs-to-view)
                    (lazy-xpath:find-prec-siblings
                     curr-node
                     (cdr ; parent is an element => cdr gives its children
                      (car ancs-to-view)))
                    descendants         ; is null
                    res)))
            ((lazy-xpath:promise? (car prec-siblings))
             (reverse
              (cons
               (delay
                 (loop curr-node ancs-to-view
                       (append (as-nodeset (force (car prec-siblings)))
                               (cdr prec-siblings))
                       descendants      ; is null
                       '()))
               res)))
            (else                      ; (car prec-siblings) is a node
             (loop curr-node ancs-to-view
                   (cdr prec-siblings)
                   (reverse
                    (descend          ; descendants are currently null
                     (draft:smart-make-context
                      (car prec-siblings)
                      ancs-to-view num-anc)))
                   res))))
          ((lazy-xpath:promise? (car descendants)) ; need to force descendant axis
           (reverse
            (cons
             (delay
               (loop curr-node ancs-to-view prec-siblings
                     (append (reverse (as-nodeset (force (car descendants))))
                             (cdr descendants))
                     '()))
             res)))
          (else                   ; the first in descendants is a node
           (loop curr-node ancs-to-view prec-siblings
                 (cdr descendants) (cons (car descendants) res)))))
       ;; no preceding members
       '()))))

;;! Preceding-sibling axis
(define (lazy-xpath:preceding-sibling test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; not a nodeset
      (if (and (sxml:context? node)
               (not (null? (sxml:context->ancestors-u node))))
          (draft:siblings->context-set
           ((sxml:filter test-pred?)
            (lazy-xpath:find-prec-siblings
             (sxml:context->node-u node)
             (cdr     ; parent is an element => cdr gives its children
              (car (sxml:context->ancestors-u node)))))
           (draft:list-head
            (sxml:context->ancestors-u node) num-anc))
          '()))))                       ; no parent => no siblings

;;! Self axis
;; Shortens the context if it contains more nodes than specified by num-ancestor
;; In most cases, this work can be considered redundant; however, it eliminates
;; some classes of error that are hard to detect
(define (lazy-xpath:self test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node)                           ; not a nodeset
      (if (sxml:context? node)
          (if (test-pred? (sxml:context->node-u node))
              (list (draft:smart-make-context
                     (sxml:context->node-u node)
                     (sxml:context->ancestors-u node)
                     num-anc))
              '())
          (if (test-pred? node) (list node) '())))))


;;-------------------------------------------------------------------------------
;;!! Making every axis consume a nodeset

;;! Given the axis of the form
;;  Node -> Nodeset
;; produces the axis of the form
;;  Node|Nodeset -> Nodeset
(define (lazy-xpath:axis-consume-nodeset axis)
  (letrec
      ((iterate-nodeset
        ;; candidates - candidate nodes for a result
        (lambda (src candidates)
          (let loop ((src src)
                     (candidates candidates)
                     (res '()))
            (cond
             ((null? candidates) ; consume the following node from src
              (cond
               ((null? src)             ; iteration is over
                (reverse res))
               ((lazy-xpath:promise? (car src))
                (if
                 (null? res) ; result is still empty, need to force src
                 (let ((src-car (as-nodeset (force (car src)))))
                   (cond
                    ((null? src-car)    ; a rare practical situation
                     (loop (cdr src) candidates res))
                    ((lazy-xpath:promise? (car src-car)) ; this shouldn't happen
                     (loop (append src-car (cdr src))
                           candidates
                           res))
                    (else              ; we can finally apply the axis
                     (loop (append (cdr src-car) (cdr src))
                           (axis (car src-car)) ; candidates are null
                           res))))
                 (reverse ; otherwise - return the result with a promise
                  (cons
                   (delay (iterate-nodeset src candidates))
                   res))))
               (else                    ; (car src) is a node
                (loop (cdr src)
                      (axis (car src))  ; candidates are null
                      res))))
             ((lazy-xpath:promise? (car candidates))
                                        ; First candidate is a promise
              (if
               (null? res) ; result is still empty, need to force candidate
               (let ((cand-car (as-nodeset (force (car candidates)))))
                 (cond
                  ((null? cand-car) ; generally, (cdr candidates)=null
                   (loop src (cdr candidates) res))
                  ((lazy-xpath:promise? (car cand-car)) ; this shouldn't happen
                   (loop src
                         (append cand-car (cdr candidates))
                         res))
                  (else                 ; add candidate to result
                   (loop src
                         (append (cdr cand-car) (cdr candidates))
                         (list (car cand-car)) ; res is null
                         ))))
               (reverse ; otherwise - return the result with a promise
                (cons
                 (delay (iterate-nodeset src candidates))
                 res))))
             (else                     ; the first candidate is a node
              (loop src (cdr candidates)
                    (cons (car candidates) res))))))))
    (lambda (nodeset)                        ; node or nodeset
      (cond
       ((null? nodeset)                 ; nothing to do
        '())
       ((and (pair? nodeset) (symbol? (car nodeset))) ; node
        (axis nodeset))
       (else                            ; the general case
        (iterate-nodeset nodeset '()))))))


;;-------------------------------------------------------------------------------
;;!! Lazy analogues for "sxpath-ext.scm"

;;-------------------------------------------------------------------------------
;;!! SXML counterparts to W3C XPath Core Functions Library

;;! The counterpart to XPath 'string' function (section 4.2 XPath Rec.)
;; Converts a given object to a string
(define (lazy-xpath:string object)
  (cond
   ((string? object) object)
   ((nodeset? object) (if (lazy-xpath:null? object)
                          ""
                          (lazy-xpath:string-value (lazy-xpath:car object))))
   ((number? object)
    (if (and (rational? object) (not (integer? object))) ; like 1/2
        (number->string (exact->inexact object))
        (number->string object)))
   ((boolean? object) (if object "true" "false"))
   (else                               ; Unknown type -> empty string.
    "")))

;;! The counterpart to XPath 'boolean' function (section 4.3 XPath Rec.)
;; Converts its argument to a boolean
(define (lazy-xpath:boolean object)
  (cond
   ((boolean? object) object)
   ((number? object) (not (= object 0)))
   ((string? object) (> (string-length object) 0))
   ((nodeset? object) (not (lazy-xpath:null? object)))
   (else                                ; Not specified in XPath Rec.
    #f)))

;;! The counterpart to XPath 'number' function (section 4.4 XPath Rec.)
;; Converts its argument to a number
;; NOTE:
;;  1. The argument is not optional (yet?)
;;  2. string->number conversion is not IEEE 754 round-to-nearest
;;  3. NaN is represented as 0
(define (lazy-xpath:number obj)
  (cond
   ((number? obj) obj)
   ((string? obj)
    (let ((nmb (call-with-input-string obj read)))
      (if (number? nmb)
          nmb
          0)))                          ; NaN
   ((boolean? obj) (if obj 1 0))
   ((nodeset? obj) (lazy-xpath:number (lazy-xpath:string obj)))
   (else 0)))                           ; unknown datatype

;;! Returns a string value for a given node in accordance to
;; XPath Rec. 5.1 - 5.7
;; Undocumented functionality - can be applied for a node that is a promise
(define (lazy-xpath:string-value node)
  (cond
   ((lazy-xpath:promise? node)
    (let ((value (force node)))
      (if (nodeset? value)
          (apply string-append
                 (map lazy-xpath:string-value value))
          (lazy-xpath:string-value value))))
   ((not (pair? node))                  ; a text node?
    (if (string? node)
        node ""))
   ((lazy-xpath:null? (cdr node))       ; null content
    "")
   (else
    (apply
     string-append
     (cons ""
           (map
            lazy-xpath:string-value
            (let ((frst (lazy-xpath:car (cdr node))))
              (if
               (and (pair? frst) (eq? '@ (car frst))) ; attribute node
               (lazy-xpath:cdr (cdr node))
               (cdr node)))))))))


;;-------------------------------------------------------------------------------
;;!! Comparators for XPath objects

;;! A helper for XPath equality operations: = , !=
;;  'bool-op', 'number-op' and 'string-op' are comparison operations for
;; a pair of booleans,  numbers and strings respectively
(define (lazy-xpath:equality-cmp bool-op number-op string-op)
  (lambda (obj1 obj2)
    (cond
     ((and (not (nodeset? obj1)) (not (nodeset? obj2)))
                                        ; neither object is a nodeset
      (cond
       ((boolean? obj1) (bool-op obj1 (sxml:boolean obj2)))
       ((boolean? obj2) (bool-op (sxml:boolean obj1) obj2))
       ((number? obj1) (number-op obj1 (sxml:number obj2)))
       ((number? obj2) (number-op (sxml:number obj1) obj2))
       (else                            ; both objects are strings
        (string-op obj1 obj2))))
     ((and (nodeset? obj1) (nodeset? obj2)) ; both objects are nodesets
      (let first ((str-set1 (lazy-xpath:map lazy-xpath:string-value obj1))
                  (str-set2 (lazy-xpath:map lazy-xpath:string-value obj2)))
        (cond
         ((null? str-set1) #f)
         ((lazy-xpath:promise? (car str-set1)) ; time to get the next portion
          (first (append (as-nodeset (force (car str-set1)))
                         (cdr str-set1))
                 str-set2))
         ((let second ((elem (car str-set1))
                       (set2 str-set2))
            (cond
             ((null? set2) #f)
             ((lazy-xpath:promise? (car set2)) ; time to get the next portion
              (second elem
                      (append (as-nodeset (force (car set2)))
                              (cdr set2))))
             ((string-op elem (car set2)) #t)
             (else (second elem (cdr set2))))) #t)
         (else
          (first (cdr str-set1) str-set2)))))
     (else         ; one of the objects is a nodeset, the other is not
      (call-with-values
          (lambda ()
            (if (nodeset? obj1) (values obj1 obj2) (values obj2 obj1)))
        (lambda (nset elem)
          (cond
           ((boolean? elem) (bool-op elem (lazy-xpath:boolean nset)))
           ((number? elem)
            (let loop ((nset
                        (lazy-xpath:map
                         (lambda (node) (lazy-xpath:number (lazy-xpath:string-value node)))
                         nset)))
              (cond
               ((null? nset) #f)
               ((lazy-xpath:promise? (car nset)) ; time to get the next portion
                (loop (append (as-nodeset (force (car nset)))
                              (cdr nset))))
               ((number-op elem (car nset)) #t)
               (else (loop (cdr nset))))))
           ((string? elem)
            (let loop ((nset (lazy-xpath:map lazy-xpath:string-value nset)))
              (cond
               ((null? nset) #f)
               ((lazy-xpath:promise? (car nset)) ; time to get the next portion
                (loop (append (as-nodeset (force (car nset)))
                              (cdr nset))))
               ((string-op elem (car nset)) #t)
               (else (loop (cdr nset))))))
           (else                        ; unknown datatype
            (display (string-append "Unknown datatype: " (object->string elem) "\n"))
            #f))))))))

(define lazy-xpath:equal? (lazy-xpath:equality-cmp eq? = string=?))

(define lazy-xpath:not-equal?
  (lazy-xpath:equality-cmp
   (lambda (bool1 bool2) (not (eq? bool1 bool2)))
   (lambda (num1 num2) (not (= num1 num2)))
   (lambda (str1 str2) (not (string=? str1 str2)))))

;;! Relational operation ( < , > , <= , >= ) for two XPath objects
;;  op is comparison procedure: < , > , <= or >=
(define (lazy-xpath:relational-cmp op)
  (lambda (obj1 obj2)
    (cond
     ((not (or (nodeset? obj1) (nodeset? obj2))) ; neither obj is a nodeset
      (op (lazy-xpath:number obj1) (lazy-xpath:number obj2)))
     ((boolean? obj1)       ; 'obj1' is a boolean, 'obj2' is a nodeset
      (op (lazy-xpath:number obj1) (lazy-xpath:number (lazy-xpath:boolean obj2))))
     ((boolean? obj2)       ; 'obj1' is a nodeset, 'obj2' is a boolean
      (op (lazy-xpath:number (lazy-xpath:boolean obj1)) (lazy-xpath:number obj2)))
     ((or (null? obj1) (null? obj2)) ; one of the objects is an empty nodeset
      #f)
     (else                          ; at least one object is a nodeset
      (op
       (cond
        ((nodeset? obj1)             ; 'obj1' is a (non-empty) nodeset
         (let ((nset1 (lazy-xpath:map
                       (lambda (node) (lazy-xpath:number (lazy-xpath:string-value node)))
                       obj1)))
           (let first ((num1 (car nset1))
                       (nset1 (cdr nset1)))
             (cond
              ((null? nset1) num1)
              ((lazy-xpath:promise? (car nset1)) ; time to obtain the next portion
               (first num1
                      (apply (as-nodeset (force (car nset1)))
                             (cdr nset1))))
              ((op num1 (car nset1)) (first num1 (cdr nset1)))
              (else (first (car nset1) (cdr nset1)))))))
        ((string? obj1) (sxml:number obj1))
        (else                           ; 'obj1' is a number
         obj1))
       (cond
        ((nodeset? obj2)             ; 'obj2' is a (non-empty) nodeset
         (let ((nset2 (lazy-xpath:map
                       (lambda (node) (lazy-xpath:number (lazy-xpath:string-value node)))
                       obj2)))
           (let second ((num2 (car nset2))
                        (nset2 (cdr nset2)))
             (cond
              ((null? nset2) num2)
              ((lazy-xpath:promise? (car nset2)) ; time to obtain the next portion
               (second num2
                       (apply (as-nodeset (force (car nset2)))
                              (cdr nset2))))
              ((op num2 (car nset2)) (second (car nset2) (cdr nset2)))
              (else (second num2 (cdr nset2)))))))
        ((string? obj2) (sxml:number obj2))
        (else                           ; 'obj2' is a number
         obj2)))))))


;;-------------------------------------------------------------------------------
;;!! XPath Core Function Library

;;-------------------------------------------------------------------------------
;; 4.1 Node Set Functions

;;! last()
(define (lazy-xpath:core-last num-anc)
  (lambda (nodeset position+size var-binding)
    (cdr position+size)))

;;! position()
(define (lazy-xpath:core-position num-anc)
  (lambda (nodeset position+size var-binding)
    (car position+size)))

;;! count(node-set)
(define (lazy-xpath:core-count num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (let ((res (arg-func nodeset position+size var-binding)))
      (cond
       ((nodeset? res) (lazy-xpath:length res))
       (else
        (sxml:xpointer-runtime-error
         "count() function - an argument is not a nodeset")
        0)))))

;;! id(object)
(define (lazy-xpath:core-id num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (let* ((root-node (list (lazy-xpath:car
                             (lazy-xpath:reach-root nodeset))))
           (id-nset ((sxml:child (ntype?? 'id-index))
                     ((sxml:child (ntype?? '@@)) root-node))))
      (if
       (null? id-nset)          ; no id-index
       '()                      ; ID function returns an empty nodeset
       (let ((res ((sxml:id (cdar id-nset)) ; implemented in "sxpath-ext.scm"
                   (lazy-xpath:result->list
                    (lazy-xpath:contextset->nodeset
                     (arg-func nodeset position+size var-binding))))))
         (if (and num-anc (zero? num-anc)) ; no ancestors required
             res
             (lazy-xpath:recover-contextset res root-node num-anc)))))))

;;! local-name(node-set?)
(define (lazy-xpath:core-local-name num-anc . arg-func) ; optional argument
  (if (null? arg-func)                  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((nodeset (lazy-xpath:contextset->nodeset nodeset)))
          (cond
           ((lazy-xpath:null? nodeset) "")
           ((not (pair? (lazy-xpath:car nodeset))) "") ; no name
           (else
            (let ((name (symbol->string (car (lazy-xpath:car nodeset)))))
              (cond
               ((string-index-right name #\:)
                => (lambda (pos)
                     (substring name (+ pos 1) (string-length name))))
               (else                    ; a NCName
                name)))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (let ((obj
                 (lazy-xpath:contextset->nodeset
                  (func nodeset position+size var-binding))))
            (cond
             ((null? obj) "")           ; an empty nodeset
             ((not (nodeset? obj))
              (sxml:xpointer-runtime-error
               "NAME function - an argument is not a nodeset")
              "")
             ((not (pair? (lazy-xpath:car obj))) "") ; no name
             (else
              (let ((name (symbol->string (car (lazy-xpath:car obj)))))
                (cond
                 ((string-index-right name #\:)
                  => (lambda (pos)
                       (substring
                        name (+ pos 1) (string-length name))))
                 (else                  ; a NCName
                  name))))))))))

;;! namespace-uri(node-set?)
(define (lazy-xpath:core-namespace-uri num-anc . arg-func) ; optional argument
  (if (null? arg-func)                  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((nodeset (lazy-xpath:contextset->nodeset nodeset)))
          (cond
           ((lazy-xpath:null? nodeset) "")
           ((not (pair? (lazy-xpath:car nodeset))) "") ; no name
           (else
            (let ((name (symbol->string (car (lazy-xpath:car nodeset)))))
              (cond
               ((string-index-right name #\:)
                => (lambda (pos)
                     (substring name 0 pos)))
               (else                    ; a NCName
                "")))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (let ((obj
                 (lazy-xpath:contextset->nodeset
                  (func nodeset position+size var-binding))))
            (cond
             ((lazy-xpath:null? obj) "") ; an empty nodeset
             ((not (nodeset? obj))
              (sxml:xpointer-runtime-error
               "NAME function - an argument is not a nodeset")
              "")
             ((not (pair? (lazy-xpath:car obj))) "") ; no name
             (else
              (let ((name (symbol->string (car (lazy-xpath:car obj)))))
                (cond
                 ((string-index-right name #\:)
                  => (lambda (pos)
                       (substring name 0 pos)))
                 (else ""))))))))))

;;! name(node-set?)
(define (lazy-xpath:core-name num-anc . arg-func) ; optional argument
  (if (null? arg-func)                  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((nodeset (lazy-xpath:contextset->nodeset nodeset)))
          (cond
           ((lazy-xpath:null? nodeset) "")
           ((not (pair? (lazy-xpath:car nodeset))) "") ; no name
           (else
            (symbol->string (car (lazy-xpath:car nodeset)))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (let ((obj
                 (lazy-xpath:contextset->nodeset
                  (func nodeset position+size var-binding))))
            (cond
             ((lazy-xpath:null? obj) "") ; an empty nodeset
             ((not (nodeset? obj))
              (sxml:xpointer-runtime-error
               "NAME function - an argument is not a nodeset")
              "")
             ((not (pair? (lazy-xpath:car obj))) "") ; no name
             (else
              (symbol->string (car (lazy-xpath:car obj))))))))))


;;-------------------------------------------------------------------------------
;;! 4.2 String Functions

;;! string(object?)
(define (lazy-xpath:core-string num-anc . arg-func) ; optional argument
  (if (null? arg-func)                  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (lazy-xpath:string
         (lazy-xpath:contextset->nodeset nodeset)))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (lazy-xpath:string
           (lazy-xpath:contextset->nodeset
            (func nodeset position+size var-binding)))))))

;;! concat(string, string, string*)
(define (lazy-xpath:core-concat num-anc . arg-func-lst)
  (lambda (nodeset position+size var-binding)
    (apply
     string-append
     (map
      (lambda (f)
        (lazy-xpath:string
         (lazy-xpath:contextset->nodeset
          (f nodeset position+size var-binding))))
      arg-func-lst))))

;;! starts-with(string, string)
(define (lazy-xpath:core-starts-with num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let ((str1 (lazy-xpath:string
                 (lazy-xpath:contextset->nodeset
                  (arg-func1 nodeset position+size var-binding))))
          (str2 (lazy-xpath:string
                 (lazy-xpath:contextset->nodeset
                  (arg-func2 nodeset position+size var-binding)))))
      (string-prefix? str2 str1))))

;;! contains(string, string)
(define (lazy-xpath:core-contains num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let ((str1 (lazy-xpath:string
                 (lazy-xpath:contextset->nodeset
                  (arg-func1 nodeset position+size var-binding))))
          (str2 (lazy-xpath:string
                 (lazy-xpath:contextset->nodeset
                  (arg-func2 nodeset position+size var-binding)))))
      ;; must return a boolean
      (if (string-contains str1 str2) #t #f))))

;;! substring-before(string, string)
(define (lazy-xpath:core-substring-before num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let* ((str1 (lazy-xpath:string
                  (lazy-xpath:contextset->nodeset
                   (arg-func1 nodeset position+size var-binding))))
           (str2 (lazy-xpath:string
                  (lazy-xpath:contextset->nodeset
                   (arg-func2 nodeset position+size var-binding))))
           (pos (string-contains str1 str2)))
      (if (not pos)                     ; STR1 doesn't contain STR2
          ""
          (substring str1 0 pos)))))

;;! substring-after(string, string)
(define (lazy-xpath:core-substring-after num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let* ((str1 (lazy-xpath:string
                  (lazy-xpath:contextset->nodeset
                   (arg-func1 nodeset position+size var-binding))))
           (str2 (lazy-xpath:string
                  (lazy-xpath:contextset->nodeset
                   (arg-func2 nodeset position+size var-binding))))
           (pos (string-contains str1 str2)))
      (if
       (not pos)                        ; STR1 doesn't contain STR2
       ""
       (substring
        str1 (+ pos (string-length str2)) (string-length str1))))))

;;! substring(string, number, number?)
(define (lazy-xpath:core-substring num-anc arg-func1 arg-func2 . arg-func3)
  (if (null? arg-func3)                 ; no third argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((str (lazy-xpath:string
                    (lazy-xpath:contextset->nodeset
                     (arg-func1 nodeset position+size var-binding))))
              (num1 (lazy-xpath:number
                     (lazy-xpath:contextset->nodeset
                      (arg-func2 nodeset position+size var-binding)))))
          (let ((len (string-length str))
                (start (- (inexact->exact (round num1)) 1)))
            (if (> start len)
                ""
                (substring str (if (< start 0) 0 start) len)))))
      (let ((arg-func3 (car arg-func3)))
        (lambda (nodeset position+size var-binding)
          (let ((str (lazy-xpath:string
                      (lazy-xpath:contextset->nodeset
                       (arg-func1 nodeset position+size var-binding))))
                (num1 (lazy-xpath:number
                       (lazy-xpath:contextset->nodeset
                        (arg-func2 nodeset position+size var-binding))))
                (num2 (lazy-xpath:number
                       (lazy-xpath:contextset->nodeset
                        (arg-func3 nodeset position+size var-binding)))))
            (let* ((len (string-length str))
                   (start (- (inexact->exact (round num1)) 1))
                   (fin (+ start (inexact->exact (round num2)))))
              (if (or (> start len) (< fin 0) (< fin start))
                  ""
                  (substring str
                             (if (< start 0) 0 start)
                             (if (> fin len) len fin)))))))))

;;! string-length(string?)
(define (lazy-xpath:core-string-length num-anc . arg-func) ; optional argument
  (if (null? arg-func)                  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (string-length
         (lazy-xpath:string (lazy-xpath:contextset->nodeset nodeset))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (string-length
           (lazy-xpath:string
            (lazy-xpath:contextset->nodeset
             (func nodeset position+size var-binding))))))))

;;! normalize-space(string?)
(define (lazy-xpath:core-normalize-space num-anc . arg-func)
  ;; internal copy of string-split
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
  (if (null? arg-func)                  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let rpt ((src (xlink:string-split
                        (lazy-xpath:string (lazy-xpath:contextset->nodeset nodeset))
                        sxml:whitespace))
                  (res '()))
          (cond
           ((null? src)
            (apply string-append (reverse res)))
           ((= (string-length (car src)) 0) ; empty string
            (rpt (cdr src) res))
           ((null? res)
            (rpt (cdr src) (cons (car src) res)))
           (else
            (rpt (cdr src) (cons (car src) (cons " " res)))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (let rpt ((src (xlink:string-split
                          (lazy-xpath:string
                           (lazy-xpath:contextset->nodeset
                            (func nodeset position+size var-binding)))
                          sxml:whitespace))
                    (res '()))
            (cond
             ((null? src)
              (apply string-append (reverse res)))
             ((= (string-length (car src)) 0) ; empty string
              (rpt (cdr src) res))
             ((null? res)
              (rpt (cdr src) (cons (car src) res)))
             (else
              (rpt (cdr src) (cons (car src) (cons " " res))))))))))

;;! translate(string, string, string)
(define (lazy-xpath:core-translate num-anc arg-func1 arg-func2 arg-func3)
  (lambda (nodeset position+size var-binding)
    (let ((str1 (lazy-xpath:string
                 (lazy-xpath:contextset->nodeset
                  (arg-func1 nodeset position+size var-binding))))
          (str2 (lazy-xpath:string
                 (lazy-xpath:contextset->nodeset
                  (arg-func2 nodeset position+size var-binding))))
          (str3 (lazy-xpath:string
                 (lazy-xpath:contextset->nodeset
                  (arg-func3 nodeset position+size var-binding)))))
      (let ((alist
             (let while ((lst2 (string->list str2))
                         (lst3 (string->list str3))
                         (alist '()))
               (cond
                ((null? lst2) (reverse alist))
                ((null? lst3)
                 (append
                  (reverse alist)
                  (map
                   (lambda (ch) (cons ch #f))
                   lst2)))
                (else
                 (while
                  (cdr lst2)
                  (cdr lst3)
                  (cons (cons (car lst2) (car lst3)) alist)))))))
        (let rpt ((lst1 (string->list str1))
                  (res '()))
          (cond
           ((null? lst1) (list->string (reverse res)))
           ((assoc (car lst1) alist)
            => (lambda (pair)
                 (if (cdr pair)
                     (rpt (cdr lst1) (cons (cdr pair) res))
                     (rpt (cdr lst1) res))))
           (else
            (rpt (cdr lst1) (cons (car lst1) res)))))))))


;;-------------------------------------------------------------------------------
;;!! 4.3 Boolean Functions

;;! boolean(object)
(define (lazy-xpath:core-boolean num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (lazy-xpath:boolean
     (arg-func nodeset position+size var-binding))))

;;! not(boolean)
(define (lazy-xpath:core-not num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (not (lazy-xpath:boolean
          (arg-func nodeset position+size var-binding)))))

;;! true()
(define (lazy-xpath:core-true num-anc)
  (lambda (nodeset position+size var-binding) #t))

;;! false()
(define (lazy-xpath:core-false num-anc)
  (lambda (nodeset position+size var-binding) #f))

;;! lang(string)
(define (lazy-xpath:core-lang num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (let ((arg (lazy-xpath:string
                (lazy-xpath:contextset->nodeset
                 (arg-func nodeset position+size var-binding))))
          (lng
           ((lazy-xpath:child (ntype?? '*text*))
            ((lazy-xpath:attribute (ntype?? 'xml:lang))
             ((lazy-xpath:ancestor-or-self (lambda (x) #t))
              (lazy-xpath:car nodeset)  ; context-node = (car nodeset)
              )))))
      (and (not (null? lng))
           (or (string-ci=? arg (lazy-xpath:car lng))
               (string-prefix-ci? (string-append arg "-") (lazy-xpath:car lng)))))))


;;-------------------------------------------------------------------------------
;;!! 4.4 Number Functions

;;! number(object?)
(define (lazy-xpath:core-number num-anc . arg-func) ; optional argument
  (if (null? arg-func)                  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (lazy-xpath:number (lazy-xpath:contextset->nodeset nodeset)))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (lazy-xpath:number
           (lazy-xpath:contextset->nodeset
            (func nodeset position+size var-binding)))))))

;;! sum(node-set)
(define (lazy-xpath:core-sum num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (let ((res (arg-func nodeset position+size var-binding)))
      (cond
       ((nodeset? res)
        (apply +
               (map
                (lambda (node)
                  (lazy-xpath:number
                   (lazy-xpath:string-value (sxml:context->node node))))
                (lazy-xpath:result->list res))))
       (else
        (sxml:xpointer-runtime-error
         "SUM function - an argument is not a nodeset")
        0)))))

;;! floor(number)
(define (lazy-xpath:core-floor num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (inexact->exact
     (floor (lazy-xpath:number
             (lazy-xpath:contextset->nodeset
              (arg-func nodeset position+size var-binding)))))))

;;! ceiling(number)
(define (lazy-xpath:core-ceiling num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (inexact->exact
     (ceiling (lazy-xpath:number
               (lazy-xpath:contextset->nodeset
                (arg-func nodeset position+size var-binding)))))))

;;! round(number)
(define (lazy-xpath:core-round num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (inexact->exact
     (round (lazy-xpath:number
             (lazy-xpath:contextset->nodeset
              (arg-func nodeset position+size var-binding)))))))


;;-------------------------------------------------------------------------------
;;!! XPath AST processing
;; AST is considered to be properly formed

;;! {5} <AxisSpecifier> ::= (axis-specifier  <AxisName> )
;;  {6} <AxisName> ::= (ancestor)
;;                     | (ancestor-or-self)
;;                     | (attribute)
;;                     | (child)
;;                     | (descendant)
;;                     | (descendant-or-self)
;;                     | (following)
;;                     | (following-sibling)
;;                     | (namespace)
;;                     | (parent)
;;                     | (preceding)
;;                     | (preceding-sibling)
;;                     | (self)
;;                     | (arc)  ; the following 3 are added by SXLink
;;                     | (traverse)
;;                     | (traverse-arc)
;; Returns:  (cons lambda num-ancestors)
(define (lazy-xpath:ast-axis-specifier op num-anc)
  (if
   (not (eq? (car op) 'axis-specifier))
   (draft:signal-semantic-error "not an AxisSpecifier - " op)
   (case (caadr op)                     ; AxisName
     ((ancestor)
      (cons lazy-xpath:ancestor #f))
     ((ancestor-or-self)
      (cons lazy-xpath:ancestor-or-self #f))
     ((attribute)
      (cons lazy-xpath:attribute (draft:na-minus-nneg num-anc 1)))
     ((child)
      (cons lazy-xpath:child (draft:na-minus-nneg num-anc 1)))
     ((descendant)
      (cons lazy-xpath:descendant (draft:na-minus-nneg num-anc 1)))
     ((descendant-or-self)
      (cons lazy-xpath:descendant-or-self num-anc))
     ((following)
      (cons lazy-xpath:following #f))
     ((following-sibling)
      (cons lazy-xpath:following-sibling (draft:na-max num-anc 1)))
     ((namespace)
      (cons lazy-xpath:namespace (draft:na-minus-nneg num-anc 1)))
     ((parent)
      (cons lazy-xpath:parent (draft:na+ num-anc 1)))
     ((preceding)
      (cons lazy-xpath:preceding #f))
     ((preceding-sibling)
      (cons lazy-xpath:preceding-sibling (draft:na-max num-anc 1)))
     ((self)
      (cons lazy-xpath:self num-anc))
     (else
      (draft:signal-semantic-error "unknown AxisName - " op)))))


;;-------------------------------------------------------------------------------
;;!! In this section, each function accepts 2 arguments
;;  op - S-expression which represents the operation
;;  num-anc - how many ancestors are required in the context after that
;;            operation
;; and returns either #f, which signals of a semantic error, or
;;  (list (lambda (nodeset position+size var-binding) ...)
;;        num-anc-it-requires
;;        requires-size? )
;;  position+size - the same to what was called 'context' in TXPath-1
;;  requires-size? - context size in required for evaluating the operation

;;! {1} <LocationPath> ::= <RelativeLocationPath>
;;                        | <AbsoluteLocationPath>
(define (lazy-xpath:ast-location-path op num-anc)
  (case (car op)
    ((absolute-location-path)
     (lazy-xpath:ast-absolute-location-path op num-anc))
    ((relative-location-path)
     (lazy-xpath:ast-relative-location-path op num-anc))
    (else
     (draft:signal-semantic-error "improper LocationPath - " op))))

;;! {2} <AbsoluteLocationPath> ::= (absolute-location-path  <Step>* )
(define (lazy-xpath:ast-absolute-location-path op num-anc)
  (cond
   ((not (eq? (car op) 'absolute-location-path))
    (draft:signal-semantic-error "not an AbsoluteLocationPath - " op))
   ((null? (cdr op))                    ; no Steps
    (list
     (lambda (nodeset position+size var-binding)
       (lazy-xpath:reach-root nodeset))
     #f                                 ; num-ancestors
     #f                                 ; requires-size?
     ))
   (else
    (and-let*
     ((steps-res (lazy-xpath:ast-step-list (cdr op) num-anc)))
     (list
      (if
       (null? (cdar steps-res))         ; only a single step
       (let ((step-impl (caar steps-res)))
         (lambda (nodeset position+size var-binding)
           (step-impl
            (lazy-xpath:reach-root nodeset) position+size var-binding)))
       (let ((converters (car steps-res)))
         (lambda (nodeset position+size var-binding)
           (let rpt ((nset (lazy-xpath:reach-root nodeset))
                     (fs converters))
             (if (null? fs)
                 nset
                 (rpt ((car fs) nset position+size var-binding)
                      (cdr fs)))))))
      ;; num-ancestors
      #f
      ;; requires-size?
      #f)))))

;;! {3} <RelativeLocationPath> ::= (relative-location-path  <Step>+ )
(define (lazy-xpath:ast-relative-location-path op num-anc)
  (if
   (not (eq? (car op) 'relative-location-path))
   (draft:signal-semantic-error "not a RelativeLocationPath - " op)
   (and-let*
    ((steps-res (lazy-xpath:ast-step-list (cdr op) num-anc)))
    (list
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
     (cadr steps-res)                   ; num-ancestors
     ;; requires-size?
     #f))))

;;! {4} <Step> ::= (step  <AxisSpecifier> <NodeTest> <Predicate>* )
;;                | (range-to  (expr <Expr>)  <Predicate>* )
(define (lazy-xpath:ast-step op num-anc)
  (cond
   ((eq? (car op) 'range-to)
    (draft:signal-semantic-error "range-to function not implemented"))
   ((eq? (car op) 'filter-expr)
    (lazy-xpath:ast-filter-expr op num-anc))
   ((eq? (car op) 'lambda-step)         ; created by sxpath
    (let ((proc (cadr op)))
      (list
       (if
        (and num-anc (zero? num-anc))   ; no ancestors required
        (lambda (nodeset position+size var-binding)
          (proc (lazy-xpath:contextset->nodeset (as-nodeset nodeset))
                var-binding))
        (lambda (nodeset position+size var-binding)
          (lazy-xpath:find-proper-context
           (proc (lazy-xpath:contextset->nodeset (as-nodeset nodeset))
                 var-binding)
           (as-nodeset nodeset)
           num-anc)))
       num-anc                          ; num-ancestors
       #f                               ; requires-last?
       )))
   ((eq? (car op) 'step)
    (if
     (null? (cdddr op))                 ; no Predicates
     (and-let*
      ((axis-lst (lazy-xpath:ast-axis-specifier (cadr op) num-anc))
       (ntest (draft:ast-node-test (caddr op))))
      (let ((axis
             (lazy-xpath:axis-consume-nodeset
              ((car axis-lst) ntest num-anc))))
        (list
         (lambda (nodeset position+size var-binding)
           (axis nodeset))
         (cdr axis-lst)                 ; num-ancestors
         #f                             ; requires-size?
         )))
     (and-let*
      ((preds-res (lazy-xpath:ast-predicate-list (cdddr op) 0))
       (axis-lst (lazy-xpath:ast-axis-specifier
                  (cadr op) (draft:na-max num-anc (cadr preds-res))))
       (ntest (draft:ast-node-test (caddr op))))
      (let ((axis ((car axis-lst)
                   ntest (draft:na-max num-anc (cadr preds-res))))
            (pred-impl-lst (car preds-res)))
        (list
         (lambda (nodeset position+size var-binding)
           (let iter-src ((src nodeset)
                          (candidates '())
                          (res '()))
             (cond
              ((null? candidates) ; consume the following node from src
               (cond
                ((null? src)            ; iteration is over
                 (reverse res))
                ((lazy-xpath:promise? (car src))
                 (if
                  (null? res) ; result is still empty, need to force src
                  (iter-src (append (as-nodeset (force (car src)))
                                    (cdr src))
                            candidates
                            res)
                  (reverse ; otherwise - return the result with a promise
                   (cons
                    (delay (iter-src src candidates '()))
                    res))))
                (else                   ; (car src) is a node
                 (iter-src
                  (cdr src)
                  (let iter-preds ((nset (axis (car src)))
                                   (preds pred-impl-lst))
                    (if
                     (null? preds)
                     nset
                     (iter-preds
                      ((car preds) nset position+size var-binding)
                      (cdr preds))))
                  res))))
              ((lazy-xpath:promise? (car candidates))
                                        ; First candidate is a promise
               (if
                (null? res) ; result is still empty, need to force candidate
                (iter-src src
                          (append (as-nodeset (force (car candidates)))
                                  (cdr candidates))
                          res)
                (reverse ; otherwise - return the result with a promise
                 (cons
                  (delay (iter-src src candidates '()))
                  res))))
              (else                    ; the first candidate is a node
               (iter-src src (cdr candidates)
                         (cons (car candidates) res))))))
         (cdr axis-lst)                 ; num-ancestors
         #f                             ; requires-last?
         )))))
   (else
    (draft:signal-semantic-error "not a Step - " op))))

;;! {4a} ( <Step>+ )
;; Returns (list (listof step-impl) num-anc) or #f
;; NOTE: requires-size? is not needed here, since it is always #f
(define (lazy-xpath:ast-step-list step-lst num-anc)
  (let loop ((steps-to-view (reverse step-lst))
             (res-lst '())
             (num-anc num-anc))
    (if (null? steps-to-view)           ; everyone processed
        (list res-lst num-anc)
        (and-let*
         ((step-res (lazy-xpath:ast-step (car steps-to-view) num-anc)))
         (loop
          (cdr steps-to-view)
          (cons (car step-res) res-lst)
          (cadr step-res))))))

;;! {8} <Predicate> ::= (predicate  <Expr> )
;; NOTE: num-anc is dummy here, since it is always 0 for Predicates
(define (lazy-xpath:ast-predicate op num-anc)
  (if
   (not (eq? (car op) 'predicate))
   (draft:signal-semantic-error "not an Predicate - " op)
   (and-let*
    ((expr-res (lazy-xpath:ast-expr (cadr op) 0)))
    (let ((pred (car expr-res)))
      (list
       (if
        (caddr expr-res)                ; requires-last?
        (lambda (nodeset position+size var-binding)
          (if
           (null? nodeset)              ; already empty
           nodeset                      ; nothing to filter
           (let ((size (lazy-xpath:length nodeset)))
             (let loop ((nset nodeset)
                        (res '())
                        (pos 1))
               (cond
                ((null? nset)
                 (reverse res))
                ((lazy-xpath:promise? (car nset))
                                        ; This promise was already forced when evaluating lazy-xpath:length
                 (loop (append (as-nodeset (force (car nset)))
                               (cdr nset))
                       res pos))
                (else                   ; (car nset) is a node
                 (let ((value (pred (list (car nset))
                                    (cons pos size)
                                    var-binding)))
                   (loop (cdr nset)
                         (if (if (number? value)
                                 (= value pos)
                                 (lazy-xpath:boolean value))
                             (cons (car nset) res)
                             res)
                         (+ pos 1)))))))))
        (lambda (nodeset position+size var-binding)
          (if
           (null? nodeset)              ; already empty
           nodeset                      ; nothing to filter
           (let loop ((nset nodeset)
                      (res '())
                      (pos 1))
             (cond
              ((null? nset)
               (reverse res))
              ((lazy-xpath:promise? (car nset))
               (reverse
                (cons
                 (delay (loop
                         (append (as-nodeset (force (car nset)))
                                 (cdr nset))
                         '()            ; turns res to empty
                         pos))
                 res)))
              (else                     ; (car nset) is a node
               (let ((value (pred (list (car nset))
                                  (cons pos 1) ; context size is dummy
                                  var-binding)))
                 (loop (cdr nset)
                       (if (if (number? value)
                               (= value pos)
                               (lazy-xpath:boolean value))
                           (cons (car nset) res)
                           res)
                       (+ pos 1)))))))))
       ;; num-ancestors
       (cadr expr-res)
       ;; requires-last?
       (caddr expr-res))))))

;;! {8a} ( <Predicate>+ )
;; Returns (list (listof pred-impl) num-anc) or #f
;; NOTE: num-anc is dummy here, since it is always 0 for Predicates
(define (lazy-xpath:ast-predicate-list op-lst num-anc)
  (let ((pred-res-lst
         (map
          (lambda (op) (lazy-xpath:ast-predicate op 0))
          op-lst)))
    (if
     (member #f pred-res-lst)           ; error detected
     #f
     (list
      (map car pred-res-lst)
      (apply draft:na-max (map cadr pred-res-lst))))))

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
(define (lazy-xpath:ast-expr op num-anc)
  (case (car op)
    ((or)
     (lazy-xpath:ast-or-expr op num-anc))
    ((and)
     (lazy-xpath:ast-and-expr op num-anc))
    ((= !=)
     (lazy-xpath:ast-equality-expr op num-anc))
    ((< > <= >=)
     (lazy-xpath:ast-relational-expr op num-anc))
    ((+ -)
     (lazy-xpath:ast-additive-expr op num-anc))
    ((* div mod)
     (lazy-xpath:ast-multiplicative-expr op num-anc))
    ((union-expr)
     (lazy-xpath:ast-union-expr op num-anc))
    ((path-expr)
     (lazy-xpath:ast-path-expr op num-anc))
    ((filter-expr)
     (lazy-xpath:ast-filter-expr op num-anc))
    ((variable-reference)
     (lazy-xpath:ast-variable-reference op num-anc))
    ((literal)
     (lazy-xpath:ast-literal op num-anc))
    ((number)
     (lazy-xpath:ast-number op num-anc))
    ((function-call)
     (lazy-xpath:ast-function-call op num-anc))
    ((absolute-location-path)
     (lazy-xpath:ast-absolute-location-path op num-anc))
    ((relative-location-path)
     (lazy-xpath:ast-relative-location-path op num-anc))
    (else
     (draft:signal-semantic-error "unknown Expr - " op))))

;;! {10} <OrExpr> ::= (or <Expr> <Expr>+ )
;; NOTE: num-anc is dummy here, since it is always 0 for OrExpr
(define (lazy-xpath:ast-or-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (lazy-xpath:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)           ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
       (list
        (lambda (nodeset position+size var-binding)
          (let rpt ((fs expr-impls))
            (cond
             ((null? fs) #f)
             ((lazy-xpath:boolean ((car fs) nodeset position+size var-binding)) #t)
             (else (rpt (cdr fs))))))
        ;; num-ancestors
        (apply draft:na-max (map cadr expr-res-lst))
        ;; requires-last?
        (apply lazy-xpath:or (map caddr expr-res-lst)))))))

;;! {11} <AndExpr> ::= (and <Expr> <Expr>+ )
;; NOTE: num-anc is dummy here, since it is always 0 for AndExpr
(define (lazy-xpath:ast-and-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (lazy-xpath:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)           ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
       (list
        (lambda (nodeset position+size var-binding)
          (let rpt ((fs expr-impls))
            (cond
             ((null? fs) #t)
             ((not
               (lazy-xpath:boolean ((car fs) nodeset position+size var-binding)))
              #f)
             (else (rpt (cdr fs))))))
        ;; num-ancestors
        (apply draft:na-max (map cadr expr-res-lst))
        ;; requires-last?
        (apply lazy-xpath:or (map caddr expr-res-lst)))))))

;;! {12} <EqualityExpr> ::= (=  <Expr> <Expr> )
;;                         | (!=  <Expr> <Expr> )
;; NOTE: num-anc is dummy here, since it is always 0 for EqualityExpr
(define (lazy-xpath:ast-equality-expr op num-anc)
  (and-let*
   ((left-lst (lazy-xpath:ast-expr (cadr op) 0))
    (right-lst (lazy-xpath:ast-expr (caddr op) 0)))
   (let ((cmp-op (cadr (assq (car op) `((= ,lazy-xpath:equal?)
                                        (!= ,lazy-xpath:not-equal?)))))
         (left (car left-lst))
         (right (car right-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (cmp-op
         (lazy-xpath:contextset->nodeset
          (left nodeset position+size var-binding))
         (lazy-xpath:contextset->nodeset
          (right nodeset position+size var-binding))))
      ;; num-ancestors
      (draft:na-max (cadr left-lst) (cadr right-lst))
      ;; requires-last?
      (or (caddr left-lst) (caddr right-lst))))))

;;! {13} <RelationalExpr> ::= (<  <Expr> <Expr> )
;;                            | (>  <Expr> <Expr> )
;;                            | (<=  <Expr> <Expr> )
;;                            | (>=  <Expr> <Expr> )
;; NOTE: num-anc is dummy here, since it is always 0 for RelationalExpr
(define (lazy-xpath:ast-relational-expr op num-anc)
  (and-let*
   ((left-lst (lazy-xpath:ast-expr (cadr op) 0))
    (right-lst (lazy-xpath:ast-expr (caddr op) 0)))
   (let ((cmp-op
          (lazy-xpath:relational-cmp
           (cadr (assq (car op) `((< ,<) (> ,>) (<= ,<=) (>= ,>=))))))
         (left (car left-lst))
         (right (car right-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (cmp-op
         (lazy-xpath:contextset->nodeset
          (left nodeset position+size var-binding))
         (lazy-xpath:contextset->nodeset
          (right nodeset position+size var-binding))))
      ;; num-ancestors
      (draft:na-max (cadr left-lst) (cadr right-lst))
      ;; requires-last?
      (or (caddr left-lst) (caddr right-lst))))))

;;! {14} <AdditiveExpr> ::= (+  <Expr> <Expr> )
;;                          | (-  <Expr> <Expr>? )
;; NOTE: num-anc is dummy here, since it is always 0 for AdditiveExpr
(define (lazy-xpath:ast-additive-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (lazy-xpath:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)           ; error detected
     #f
     (let ((add-op (cadr (assq (car op) `((+ ,+) (- ,-)))))
           (expr-impls (map car expr-res-lst)))
       (list
        (lambda (nodeset position+size var-binding)
          (apply
           add-op
           (map
            (lambda (expr)
              (lazy-xpath:number
               (lazy-xpath:contextset->nodeset
                (expr nodeset position+size var-binding))))
            expr-impls)))
        ;; num-ancestors
        (apply draft:na-max (map cadr expr-res-lst))
        ;; requires-last?
        (apply lazy-xpath:or (map caddr expr-res-lst)))))))

;;! {15} <MultiplicativeExpr> ::= (*  <Expr> <Expr> )
;;                                | (div  <Expr> <Expr> )
;;                                | (mod  <Expr> <Expr> )
;; NOTE: num-anc is dummy here, since it is always 0 for MultiplicativeExpr
(define (lazy-xpath:ast-multiplicative-expr op num-anc)
  (and-let*
   ((left-lst (lazy-xpath:ast-expr (cadr op) 0))
    (right-lst (lazy-xpath:ast-expr (caddr op) 0)))
   (let ((mul-op
          (cadr (assq (car op) `((* ,*) (div ,/) (mod ,remainder)))))
         (left (car left-lst))
         (right (car right-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (mul-op
         (lazy-xpath:number
          (lazy-xpath:contextset->nodeset
           (left nodeset position+size var-binding)))
         (lazy-xpath:number
          (lazy-xpath:contextset->nodeset
           (right nodeset position+size var-binding)))))
      ;; num-ancestors
      (draft:na-max (cadr left-lst) (cadr right-lst))
      ;; requires-last?
      (or (caddr left-lst) (caddr right-lst))))))

;;! {16} <UnionExpr> ::= (union-expr  <Expr> <Expr>+ )
(define (lazy-xpath:ast-union-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (lazy-xpath:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)           ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
       (list
        (lambda (nodeset position+size var-binding)
          (let iter-operands ((fs expr-impls)
                              (candidates '())
                              (res '()))
            (cond
             ((null? candidates)
              (if
               (null? fs)             ; no more operands to be unioned
               (reverse res)
               (iter-operands
                (cdr fs)
                (let ((nset ((car fs) nodeset position+size var-binding)))
                  (cond
                   ((not (nodeset? nset))
                    (sxml:xpointer-runtime-error
                     "expected - nodeset instead of " nset)
                    '())
                   (else nset)))
                res)))
             ((lazy-xpath:promise? (car candidates))
              (if
               (null? res) ; res is still null, need to force candidate
               (iter-operands
                fs
                (append (as-nodeset (force (car candidates)))
                        (cdr candidates))
                res)
               (reverse
                (cons
                 (delay (iter-operands
                         fs
                         (append (as-nodeset (force (car candidates)))
                                 (cdr candidates))
                         '()))
                 res))))
             (else                      ; first candidate is a node
              (iter-operands
               fs (cdr candidates) (cons (car candidates) res))))))
        (apply draft:na-max (map cadr expr-res-lst))
        ;; requires-last?
        (apply lazy-xpath:or (map caddr expr-res-lst)))))))

;;! {17} <PathExpr> ::= (path-expr  <FilterExpr> <Step>+ )
(define (lazy-xpath:ast-path-expr op num-anc)
  (and-let*
   ((steps-res (lazy-xpath:ast-step-list (cddr op) num-anc))
    (filter-lst (lazy-xpath:ast-filter-expr (cadr op) (cadr steps-res))))
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
      ;; num-ancestors
      (cadr filter-lst)
      ;; requires-last?
      (caddr filter-lst)))))

;;! {18} <FilterExpr> ::= (filter-expr (primary-expr  <Expr> )
;;                                    <Predicate>* )
(define (lazy-xpath:ast-filter-expr op num-anc)
  (cond
   ((not (eq? (car op) 'filter-expr))
    (draft:signal-semantic-error "not an FilterExpr - " op))
   ((not (eq? (caadr op) 'primary-expr))
    (draft:signal-semantic-error "not an PrimaryExpr - " (cadr op)))
   ((null? (cddr op))                   ; no Predicates
    (lazy-xpath:ast-expr (cadadr op) num-anc))
   (else                                ; there are predicates
    (and-let*
     ((preds-res (lazy-xpath:ast-predicate-list (cddr op) 0))
      (expr-lst (lazy-xpath:ast-expr
                 (cadadr op) (draft:na-max num-anc (cadr preds-res)))))
     (let ((expr-impl (car expr-lst))
           (pred-impl-lst (car preds-res)))
       (list
        (lambda (nodeset position+size var-binding)
          (let ((prim-res (expr-impl nodeset position+size var-binding)))
            (let iter-preds ((nset
                              (if
                               (nodeset? prim-res)
                               prim-res
                               (begin
                                 (sxml:xpointer-runtime-error
                                  "expected - nodeset instead of " prim-res)
                                 '())))
                             (preds pred-impl-lst))
              (if
               (null? preds)
               nset
               (iter-preds
                ((car preds) nset position+size var-binding)
                (cdr preds))))))
        ;; num-ancestors
        (cadr expr-lst)
        ;; requires-last?
        #f))))))

;;! {19} <VariableReference> ::= (variable-reference  <String> )
(define (lazy-xpath:ast-variable-reference op num-anc)
  (let ((name (string->symbol (cadr op))))
    (list
     (lambda (nodeset position+size var-binding)
       (cond
        ((assoc name var-binding)
         => cdr)
        (else
         (sxml:xpointer-runtime-error "unbound variable - " name)
         '())))
     ;; num-ancestors
     0
     ;; requires-last?
     #f)))

;;! {20} <Literal> ::= (literal  <String> )
(define (lazy-xpath:ast-literal op num-anc)
  (let ((literal (cadr op)))
    (list
     (lambda (nodeset position+size var-binding) literal)
     0 #f)))

;;! {21} <Number> :: (number  <Number> )
(define (lazy-xpath:ast-number op num-anc)
  (let ((number (cadr op)))
    (list
     (lambda (nodeset position+size var-binding) number)
     0 #f)))

;;! {22} <FunctionCall> ::= (function-call (function-name  <String> )
;;                                         (argument  <Expr> )* )
(define (lazy-xpath:ast-function-call op num-anc)
  (let ((core-alist
         ;; (list fun-name min-num-args max-num-args na4res impl requires-last?)
         `((last 0 0 0 ,lazy-xpath:core-last #t)
           (position 0 0 0 ,lazy-xpath:core-position #f)
           (count 1 1 0 ,lazy-xpath:core-count #f)
           (id 1 1 #f ,lazy-xpath:core-id #f)
           (local-name 0 1 0 ,lazy-xpath:core-local-name #f)
           (namespace-uri 0 1 0 ,lazy-xpath:core-namespace-uri #f)
           (name 0 1 0 ,lazy-xpath:core-name #f)
           (string 0 1 0 ,lazy-xpath:core-string #f)
           (concat 2 -1 0 ,lazy-xpath:core-concat #f)
           (starts-with 2 2 0 ,lazy-xpath:core-starts-with #f)
           (contains 2 2 0 ,lazy-xpath:core-contains #f)
           (substring-before 2 2 0 ,lazy-xpath:core-substring-before #f)
           (substring-after 2 2 0 ,lazy-xpath:core-substring-after #f)
           (substring 2 3 0 ,lazy-xpath:core-substring #f)
           (string-length 0 1 0 ,lazy-xpath:core-string-length #f)
           (normalize-space 0 1 0 ,lazy-xpath:core-normalize-space #f)
           (translate 3 3 0 ,lazy-xpath:core-translate #f)
           (boolean 1 1 0 ,lazy-xpath:core-boolean #f)
           (not 1 1 0 ,lazy-xpath:core-not #f)
           (true 0 0 0 ,lazy-xpath:core-true #f)
           (false 0 0 0 ,lazy-xpath:core-false #f)
           (lang 1 1 #f ,lazy-xpath:core-lang #f)
           (number 0 1 0 ,lazy-xpath:core-number #f)
           (sum 1 1 0 ,lazy-xpath:core-sum #f)
           (floor 1 1 0 ,lazy-xpath:core-floor #f)
           (ceiling 1 1 0 ,lazy-xpath:core-ceiling #f)
           (round 1 1 0 ,lazy-xpath:core-round #f))))
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
              ((args-impl (lazy-xpath:ast-function-arguments (cddr op))))
              (list
               ;; Producing a function implementation
               (apply (list-ref description 4) num-anc args-impl)
               (list-ref description 3)
               ;; requires-last?
               (list-ref description 5)))))))
     ;; function definition not found
     (else
      (draft:signal-semantic-error
       "function call to an unknown function - " (cadadr op))))))

;;! {22a} ( (argument  <Expr> )* )
;; Returns: (listof expr-impl) or #f
(define (lazy-xpath:ast-function-arguments op-lst)
  (let ((arg-res-lst
         (map
          (lambda (op)
            (if
             (not (eq? (car op) 'argument))
             (draft:signal-semantic-error "not an Argument - " op)
             (lazy-xpath:ast-expr (cadr op) 0)))
          op-lst)))
    (if
     (member #f arg-res-lst)            ; semantic error detected
     #f
     (map car arg-res-lst))))


;;-------------------------------------------------------------------------------
;;!! Highest level API functions
;; The API is identical to the API of a context-based SXPath (here we even use
;; API helpers from "xpath-context.scm"). For convenience, below we repeat
;; comments for the API (borrowed from "xpath-context.scm")
;;
;; xpath-string - an XPath location path (a string)
;; ns+na - can contain 'ns-binding' and/or 'num-ancestors' and/or none of them
;; ns-binding - declared namespace prefixes (an optional argument)
;;  ns-binding ::= (listof (prefix . uri))
;;  prefix - a symbol
;;  uri - a string
;; num-ancestors - number of ancestors required for resulting nodeset. Can
;;  generally be omitted and is than defaulted to 0, which denotes a _usual_
;;  nodeset. If a negative number, this signals that all ancestors should be
;;  remembered in the context
;;
;; Returns: (lambda (nodeset position+size var-binding) ...)
;; position+size - the same to what was called 'context' in TXPath-1
;; var-binding - XPath variable bindings (an optional argument)
;;  var-binding = (listof (var-name . value))
;;  var-name - (a symbol) a name of a variable
;;  value - its value. The value can have the following type: boolean, number,
;;  string, nodeset. NOTE: a node must be represented as a singleton nodeset

;;! Helper for constructing several highest-level API functions
(define (lazy-xpath:api-helper grammar-parser ast-parser)
  (lambda (xpath-string . ns+na)
    (call-with-values
        (lambda () (draft:arglist->ns+na ns+na))
      (lambda (ns-binding num-anc)
        (and-let*
         ((ast (grammar-parser xpath-string ns-binding))
          (impl-lst (ast-parser ast num-anc)))
         (let ((query-impl (car impl-lst)))
           (lambda (node . var-binding)
             (let ((query-res
                    (query-impl
                     (as-nodeset node) (cons 1 1)
                     (if (null? var-binding) var-binding (car var-binding)))))
               (if (and num-anc (zero? num-anc) (nodeset? query-res))
                   (lazy-xpath:map sxml:context->node query-res)
                   query-res)))))))))

(define lazy-xpath:txpath
  (lazy-xpath:api-helper txp:xpath->ast lazy-xpath:ast-location-path))

(define lazy-xpath:xpath-expr
  (lazy-xpath:api-helper txp:expr->ast lazy-xpath:ast-expr))

;; Support for native sxpath syntax
(define lazy-xpath:sxpath
  (lazy-xpath:api-helper txp:sxpath->ast lazy-xpath:ast-expr))
