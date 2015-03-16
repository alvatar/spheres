;;!!! Zipper data structue
;; .author Oleg Kiselyov, 2004-2006
;;
;; Zipper is a very handy data structure that lets us replace an item
;; deep in a complex data structure, e.g., a tree or a term, without any
;; mutation. The resulting data structure will share as much of its
;; components with the old structure as possible [see addendum]. The old
;; data structure is still available (which can be handy if we wish to
;; 'undo' the operation later on). Zipper is essentially an `updateable'
;; and yet pure functional cursor into a data structure.
;;
;; Useful references:
;;         http://www.nist.gov/dads/HTML/zipper.html
;;         http://citeseer.ist.psu.edu/hinze01web.html
;;
;; Zipper is a _delimited continuation_ reified as a data
;; structure. Somehow that idea is not commonly discussed in the zipper
;; literature. Because Scheme has first-class and delimited
;; continuations, we can derive and use zipper far more easily.
;;
;; Given below is a derivation of zipper and an example of its use:
;; swapping out of two branches of a two trees. The latter is a typical
;; cross-over operation in genetic programming.
;;
;; As pointed out earlier, we don't need counting to select a random node
;; from a tree. After we selected the node, we can zip down to that node
;; in the tree using the eq? test. In the following however,
;; we skip the random selection for simplicity and thus we shall be
;; selecting nodes by their index in the depth-first order.
;;
;;
;; To derive zipper, we first write the familiar depth-first traversal
;; routine:
;;
;; deterministic, left-to-right map
;; (define (map* f l)
;;   (if (null? l) l
;;     (cons (f (car l)) (map* f (cdr l)))))
;;
;; (define (depth-first handle tree)
;;   (cond
;;     ((null? tree) tree)
;;     ((handle tree) => (lambda (new-tree) new-tree))
;;     ; the node was not handled -- descend
;;     ((not (pair? tree)) tree) ; an atom
;;     (else
;;       (cons (car tree) 			; node name
;; 	(map* (lambda (kid) (depth-first handle kid)) (cdr tree))))))
;;
;; The function handle, the first-argument of depth-first, receives a
;; node and should yield either a node or #f. In the first case, the
;; return node replaces the existing node in the result tree. If the
;; handle returned #f, it has declined to handle that node, so we keep
;; that node and descend into it, if possible.
;;
;; To see how this works, we define two sample trees and print out their
;; nodes:
;;
;; (define tree1 '(a (b) (c (d 1 2)) e))
;; (define tree2 '(z (u) (v (w 10 12)) y))
;;
;; (depth-first (lambda (node) (display node) (newline) #f) tree1)
;; ==> prints
;;   (a (b) (c (d 1 2)) e)
;;   (b)
;;   (c (d 1 2))
;;   (d 1 2)
;;   1
;;   2
;;   e
;; ==> yields
;; '(a (b) (c (d 1 2)) e)
;;
;;
;; (define-record-type zzipper
;;   (zipper curr-node k)
;;   zipper?
;;   (curr-node z-curr-node)
;;   (k z-k))
;;
;; It contains two fields: the current node of a tree, and the
;; continuation. The continuation should receive a node or #f. In the
;; former case, the received node will replace the existing node. In the
;; latter case, we keep the existing node and continue the traversal. The
;; continuation returns either a new zipper, or a tree (if the traversal
;; is finished). One can see that zipper is in a sense an 'inverse' of the
;; function handle.
;;
;; (define (zip-tree tree)
;;   (reset (depth-first (lambda (tree) (shift f (zipper tree f))) tree)))
;;
;; As promised, zipper is indeed a manifestation of a delimited
;; continuation.
;;
;; We should point out that both the zipper record and the constructor
;; function zip-tree are _generic_. They by themselves depend neither on
;; the representation of the tree nor on the traversal strategy. All the
;; information about the tree data structure and its traversal is
;; encapsulated in one single function depth-first. We can switch from
;; depth-first to breadth-first strategy or from a nested list to a
;; nested vector realization of trees just by changing
;; depth-first. Neither zipper, nor zip-tree, nor any code that uses
;; zipper (see below) will require any modifications. This property of
;; our zipper is in a marked contrast with Gerard Huet's derivation of
;; zipper. In Gerard Huet's formulation, zipper does depend on the
;; concrete realization of the data type: zipper is derived (pun
;; intended) from the data type. Different data types (and different
;; realizations of an abstract data type) will have different
;; corresponding zipper structures. In our formulation, zipper is a
;; _generic_ derivation (pun intended) on the traversal function. Zipper
;; is a derivative of the traversal function -- mechanical derivative at
;; that. So, shift/reset can be considered traversal function derivative
;; operators.
;;
;;
;; We can now print out the tree in a different way:
;;
;; (define (print-tree tree)
;;   (do ((cursor (zip-tree tree) ((z-k cursor) #f)))
;;       ((not (zipper? cursor)))
;;     (display (z-curr-node cursor))
;;     (newline)))
;;
;; we use zipper, which is a cursor, to examine all of the tree, node by
;; node. In a sense, we have inverted the operation of depth-first.
;;
;; (print-tree tree1)
;; prints as before
;;
;; (print-tree tree2)
;;   (z (u) (v (w 10 12)) y)
;;   (u)
;;   (v (w 10 12))
;;   (w 10 12)
;;   10
;;   12
;;   y
;;
;; We introduce a few helpful functions
;;
;; (define (zip-all-the-way-up zipper)
;;   (if (zipper? zipper) (zip-all-the-way-up ((z-k zipper) (z-curr-node zipper)))
;;     zipper))
;;
;; (define (locate-nth-node n tree)
;;   (do ((i 0 (+ 1 i)) (cursor (zip-tree tree) ((z-k cursor) #f)))
;;     ((and (= i n)
;;        (if (zipper? cursor) #t
;; 	 (error "too few nodes"))) cursor)
;;     ))
;;
;;
;; And we are ready for some action:
;;
;; replace the 3-d node of tree1 with 'xxx
;; (let ((desired-node (locate-nth-node 3 tree1)))
;;   (display "Replacing the node: ")
;;   (display (z-curr-node desired-node))
;;   (newline)
;;   (zip-all-the-way-up ((z-k desired-node) 'xxx)))
;;
;; ==> prints
;;     Replacing the node: (d 1 2)
;; ==> yieds
;;     '(a (b) (c xxx) e)
;;
;; It did replace it, didn't it?
;;
;; ; cross-over of the 3d node of tree1 and 1st node of tree2
;; (let* ((desired-node1 (locate-nth-node 3 tree1))
;;        (_ (begin
;; 	    (display "Cross-over the node1: ")
;; 	    (display (z-curr-node desired-node1))
;; 	    (newline)))
;;        (desired-node2 (locate-nth-node 1 tree2))
;;        (_ (begin
;; 	    (display "Cross-over the node2: ")
;; 	    (display (z-curr-node desired-node2))
;; 	    (newline)))
;;        (new-tree1
;; 	 (zip-all-the-way-up ((z-k desired-node1)
;; 			      (z-curr-node desired-node2))))
;;        (new-tree2
;; 	 (zip-all-the-way-up ((z-k desired-node2)
;; 			      (z-curr-node desired-node1))))
;; 	)
;;   (display "new tree1: ") (display new-tree1) (newline)
;;   (display "new tree2: ") (display new-tree2) (newline)
;; )
;;
;; ==> prints
;;   Cross-over the node1: (d 1 2)
;;   Cross-over the node2: (u)
;;   new tree1: (a (b) (c (u)) e)
;;   new tree2: (z (d 1 2) (v (w 10 12)) y)
;;
;; Well, it seems to work...
;;
;; If we swap the 3d node of tree1 and the 5th node of tree2, we get
;;   Cross-over the node1: (d 1 2)
;;   Cross-over the node2: 12
;;   new tree1: (a (b) (c 12) e)
;;   new tree2: (z (u) (v (w 10 (d 1 2))) y)
;;
;; To conclude, delimited continuations are quite useful. They can be
;; emulated in any R5RS Scheme system; yet it is better for performance
;; if they are supported natively. Scheme48 does support delimited
;; continuations natively (Martin Gasbichler and Michael Sperber,
;; ICFP 2002). If your favorite Scheme system does not offer them by
;; default, please complain to the implementors. It doesn't matter which
;; particular delimited continuation operator (shift, control,
;; shift0, splitter, cupto, etc) is supported -- all of them are equally
;; expressible:
;; 	Chung-chieh Shan, Scheme2004 workshop
;; 	http://www.eecs.harvard.edu/~ccshan/recur/
;;
;; Addendum, June 7, 2006 [inspired by a question from Andrew Wilcox]
;; To be more precise, the zipper preserves sharing as much as the
;; underlying enumerator does. The following is the maximal sharing
;; preserving enumerator. Those two functions should replace the ones in
;; the article.
;;
;; deterministic, left-to-right map
;; It preserves sharing as much as possible: that is, if given the pair
;; l == (cons h t), (and (eq? h (f h)) (eq? t (map* f t))) holds, then
;; (eq? (map* f l) l) holds as well.
;; (define (map* f l)
;;   (if (null? l) l
;;     (let ((h (car l)) (t (cdr l)))
;;       (let ((h1 (f h)) (t1 (map* f t)))
;;         (if (and (eq? h1 h) (eq? t1 t)) l
;;             (cons h1 t1))))))
;;
;; (define (depth-first handle tree)
;;   (cond
;;     ((null? tree) tree)
;;     ((handle tree) => (lambda (new-tree) new-tree))
;;     ; the node was not handled -- descend
;;     ((not (pair? tree)) tree) ; an atom
;;     (else
;;       (let ((kids1
;;              (map* (lambda (kid) (depth-first handle kid)) (cdr tree))))
;;         (if (eq? kids1 (cdr tree)) tree
;;             (cons (car tree) ; node name
;;                 kids1))))))
;;
;; To test that the new depth-first indeed preserves sharing, we evaluate
;;
;; (eq? tree1
;;      (depth-first (lambda (node) (display node) (newline) #f) tree1))
;;
;; which, after printing all nodes in depth-first order, gives the result
;; #t. The tree returned by depth-first in this case is indeed the
;; original tree as it is.
;;
;; The zipper code needs no changes, and it works as it was, with the
;; same results. To test the sharing preservation, we first produce a
;; tree by replacing the 6th node (which is y) in tree2:
;;
;; (define tree2*
;;   (let ((desired-node (locate-nth-node 6 tree2)))
;;   (display "Replacing the node: ")
;;   (display (z-curr-node desired-node))
;;   (newline)
;;   (zip-all-the-way-up ((z-k desired-node) 'newy))))
;;
;; here's the result: (z (u) (v (w 10 12)) newy)
;;
;; Now, we write a function that takes two trees, traverses them in
;; lockstep and prints out the nodes and if they are shared:
;;
;; (define (tree-compare-sharing t1 t2)
;;   (do ((cursor1 (zip-tree t1) ((z-k cursor1) #f))
;;        (cursor2 (zip-tree t2) ((z-k cursor2) #f)))
;;       ((cond
;;         ((and (zipper? cursor1) (zipper? cursor2)) #f)
;; 	((zipper? cursor1) (display "t2 finished early") #t)
;; 	((zipper? cursor2) (display "t1 finished early") #t)
;; 	(else #t)))
;;       (let ((n1 (z-curr-node cursor1)) (n2 (z-curr-node cursor2)))
;;         (cond
;; 	 ((eq? n1 n2) (display "shared node: ") (display n1))
;; 	 (else (display "t1 node: ") (display n1) (newline)
;; 	       (display "t2 node: ") (display n2)))
;; 	(newline))))
;;
;; (tree-compare-sharing tree2 tree2*)
;; ===>
;; t1 node: (z (u) (v (w 10 12)) y)
;; t2 node: (z (u) (v (w 10 12)) newy)
;; shared node: (u)
;; shared node: (v (w 10 12))
;; shared node: (w 10 12)
;; shared node: 10
;; shared node: 12
;; t1 node: y
;; t2 node: newy
;;

(define-library (spheres/structure zipper)
  (export zipper
          zipper?
          zipper-current-node
          zipper-k)

  (define-type <zipper>
    constructor: zipper
    predicate: zipper?
    (current-node zipper-current-node)
    (k zipper-k)))
