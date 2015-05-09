;;!!! Suffix tree, a data structure for representing sets of lists efficiently,
;; provided there is an ordering relation on the elements of lists.
;;
;; .author Ivan Raikov, 2011
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright 2011 Ivan Raikov and the Okinawa Institute of Science and
;; Technology.
;;
;; A suffix tree is a tree with arcs labeled by elements from the
;; element type of the lists and with branches ordered on the basis of
;; their arc labels; moreover, only one branch per distinct label
;; value is allowed per node.  Ends of lists are designated by an
;; "EOL" marker; a value may be associated with the EOL symbol.

(define (list-of pred) (lambda (x) (every pred x)))

;; Original code, from Chicken Scheme
;; EOL/BRN predicates not implemented
;; (define-datatype branch
;;   branch?
;;   (EOL (v identity))
;;   (BRN (label identity)
;;        (branches (list-of branch?))))
(define-type branch
  type-exhibitor: branch
  predicate: branch?
  type
  v/label
  branches)
(define (EOL v)
  (make-branch 'EOL v #f))
(define (BRN label branches)
  (make-branch 'BRN label branches))


(define-record-type suffix-tree
  (make-suffix-tree1 leq key->list branches)
  suffix-tree?
  (leq suffix-tree-compfn)
  (key->list suffix-tree-keyfn)
  (branches suffix-tree-branches))

(define (suffix-tree-branch-label b)
  (match b (($ branch 'BRN l bs) l)
	 (else (error 'suffix-tree-branch-label "invalid branch" b))))

(define (suffix-tree-branch-children b)
  (match b (($ branch 'BRN l bs) bs)
	 (else (error 'suffix-tree-branch-children "invalid branch" b))))

(define (suffix-tree-branch-eol b)
  (match b (($ branch 'BRN l (($ branch 'EOL v))) v)
	 (else #f)))

(define (suffix-tree-equal? t1 t2)
  (let ((aeq (suffix-tree-compfn t1))
	(tr1 (suffix-tree-branches t1))
	(beq (suffix-tree-compfn t2))
	(tr2 (suffix-tree-branches t2)))
    (let recur ((tr1 tr1) (tr2 tr2))
      (match (list tr1 tr2)
             ((() ())   #t)
             (((($ branch 'EOL b1) . tr1) (($ branch 'EOL b2) . tr2))
              (and (beq b1 b2) (recur tr1 tr2)))
             (((($ branch 'BRN a1 tr11) . tr1) (($ branch 'BRN a2 tr21) . tr2))
              (and (aeq a1 a2) (recur tr11 tr21) (recur tr1 tr2)))
             (else #f)))))

(define (make-suffix-tree leq key->list . rest)
  (make-suffix-tree1 leq key->list '()))

(define (update-branches branches tree)
  (make-suffix-tree1 (suffix-tree-compfn tree)
		     (suffix-tree-keyfn tree)
		     branches))

;;! Inserts list into tr and associates bval with the EOL indicator for the list
(define (suffix-tree-insert key bval tr)
  (let ((lst ((suffix-tree-keyfn tr) key)))
    (if (null? lst)
	(error 'suffix-tree-insert "empty input list"))
    (let ((leq (suffix-tree-compfn tr)))
      (let ((branches
	     (let recur ((lst lst)
			 (bval bval)
			 (tr (suffix-tree-branches tr)))
	       (match (list lst bval tr)
		      ((() b ())
		       (list (EOL b)))
		      (((a . t) b ())
		       (list (BRN a (recur t b '()))))
		      ((() b (($ branch 'EOL _) . _))
		       (error 'insert "element already in tree" ))
		      (((and a (_ . _)) b (($ branch 'EOL b1) . tr))
		       (cons (EOL b1) (recur a b tr)))
		      ((() b tr)
		       (cons (EOL b) tr))
		      (((and al (a . t)) b (and tr (($ branch 'BRN a1 tr1) . tr2)))
		       (if (leq  a a1)
			   (if (leq a1 a)
			       (cons (BRN a1 (recur t b tr1)) tr2)
			       (cons (BRN a  (recur t b '())) tr))
			   (cons (BRN a1 tr1) (recur al b tr2))))))))
	(update-branches branches tr)))))

;;! Returns the value associated with lst in tr
(define* (suffix-tree-lookup k t (partial #f))
  (let ((leq (suffix-tree-compfn t)))
    (let recur ((lst ((suffix-tree-keyfn t) k))
                (tr (suffix-tree-branches t)))
      (match (list lst tr)
             ((_ ())  (error 'suffix-tree-lookup "not found" k))
             ((() (($ branch 'EOL b) . tr))
              b)
             (((and al (_ . _)) (($ branch 'EOL _) . tr))
              (recur al tr))
             ((() tr)
              (if (not partial)
                  (error 'suffix-tree-lookup "not found" k)
                  (partial (update-branches tr t))
                  ))
             (((and al (a . t)) (($ branch 'BRN a1 tr1) . tr2))
              (if (leq a a1)
                  (if (leq a1 a)
                      (recur t tr1)
                      (error 'suffix-tree-lookup "not found" k))
                  (recur al tr2)))))))

;;! Removes lst from tr.  Any branches having a null subsuffix-tree
;; associated with them are deleted.
(define (suffix-tree-remove k tr)
  (let ((leq (suffix-tree-compfn tr)))
    (let ((branches
	   (let recur ((k ((suffix-tree-keyfn tr) k))
		       (tr (suffix-tree-branches tr)))
             (match (list k tr)
                    ((() (($ branch 'EOL _) . tr1))
                     tr1)
                    (((and al (_ . _)) (($ branch 'EOL b) . tr1))
                     (cons (EOL b) (recur al tr1)))
                    ((() tr1)  tr1)
                    (((and al (a . t)) (and tr (($ branch 'BRN a1 tr1) . tr2)))
                     (if (leq a a1)
                         (if (leq a1 a)
                             (let ((tr3  (recur t tr1)))
                               (if (null? tr3) tr2 (cons (BRN a1 tr3) tr2)))
                             tr)
                         (cons (BRN a1 tr1) (recur al tr2))))))))
      (update-branches branches tr))))

;;! Merges tr1 and tr2.  If there is a list that appears in both
;; suffix-trees, an exception is raised.
(define (suffix-tree-merge tr1 tr2)
  (let ((leq (suffix-tree-compfn tr1)))
    (let ((branches
	   (let recur ((tr1 (suffix-tree-branches tr1))
		       (tr2 (suffix-tree-branches tr2)))
	     (match (list tr1 tr2)
                    ((()  tr2)  tr2)
                    ((tr1 ())   tr1)
                    (((($ branch 'EOL b1) . _) (($ branch 'EOL _) . _))
                     (error 'suffix-tree-merge "already in suffix-tree" tr1 tr2))
                    (((($ branch 'EOL b1) . tr11) tr2)
                     (cons (EOL b1) (recur tr11 tr2)))
                    ((tr1 (($ branch 'EOL b2) . tr21))
                     (cons (EOL b2) (recur tr1 tr21)))
                    (((and tr1 (($ branch 'BRN a1 tr11) . tr12))
                      (and tr2 (($ branch 'BRN a2 tr21) . tr22)))
                     (if (leq a1 a2)
                         (if (leq a2 a1)
                             (cons (BRN a1 (recur tr11 tr21)) (recur  tr12 tr22))
                             (cons (BRN a1 tr11) (recur  tr12 tr2)))
                         (cons (BRN a2 tr21) (recur tr1 tr22))))))))
      (update-branches branches tr1))))

;;! Splits tr into three suffix-trees on the basis of a.  The first suffix-tree
;; consists of branches headed by actions less than a (plus any EOL
;; symbol), the second contains the branch (if any) associated with a,
;; and the third consists of branches headed by actions greater than a.
(define (suffix-tree-partition a tr)
  (let ((leq (suffix-tree-compfn tr)))
    (let recur ((a a) (tr (suffix-tree-branches tr)))
      (match (list tr a)
             ((() a)  (list '() '() '()))
             (((($ branch 'EOL b) . tr1) a)
              (match-let (((tr1 tr2 tr3)  (recur a tr1)))
                         (list (cons (EOL b) tr1) tr2 tr3)))
             (((and tr (($ branch 'BRN a1 tr1) . tr2)) a)
              (if (leq a a1)
                  (if (leq a1 a)
                      (list '() (list (BRN a tr1)) tr2)
                      (list '() '() tr))
                  (match-let (((tr1 tr2 tr3)  (recur a tr2)))
                             (list (cons (BRN a1 tr1) tr1) tr2 tr3))))))))

(define (suffix-tree-lookup/partial k tr)
  (suffix-tree-lookup k tr (lambda (x) x)))
