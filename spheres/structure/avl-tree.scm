;;!!! AVL Tree
;; .author Hans Oesterholt, 2005
;; .author Alvaro Castro-Castilla, 2015
;;
;; This module provides a thread safe (TODO) implementation of AVL Trees.
;; AVL Trees are binary trees that have the nice property that they
;; are kept balanced. Actually, unlike normal binary trees, all insert,
;; delete and find operations on AVL Trees are guarantied  O(log n).
;; See L<http://wikipedia.org/wiki/AVL_tree|http://wikipedia.org/wiki/AVL_tree>
;; for more information on AVL Trees.
;;
;; This implementation has been derived from a C++ implementation of
;; William A. McKee (google: avl tree algorithm McKee will get you
;; to his homepage).
;;
;; It starts out with an avl algorithm implementation on the tree nodes,
;; after which the algorithm on the nodes is wrapped as a whole avl tree.
;; Refer to the
;; L<avl tree documentation|#avl_tree_interface> for the
;; provided AVL Tree functions.
;;
;; Although the basic operations on avl trees are thread safe,
;; if multiple avl trees are combined into one operation, e.g.
;; an avl-map function, thread safety will not be guaranteed for
;; all trees.
;;
;; Thread safety for one tree is provided using
;; a recursion enabled critical section.

(declare (fixnum))

;;! AVL tree node datastructure.
(define-type avl-node
  (data data data!)
  (left left left!)
  (right right right!)
  (height height height!))

;;-------------------------------------------------------------------------------
;;!! AVL Node Implementation
;;
;; The AVL Tree Node implementation is not hard to understand.
;; AVL Trees are being balanced by keeping a balance factor in
;; each node. A node with balance factor -1, 0 or 1 is considered
;; balanced. All other (integer) values will make the node unbalanced.
;; In this implementation, the balance factor is the 'height' factor.

;;! Compute height, computes this factor for a node.
(define (compute-height node)
  (let ((h 0))
    (if (not (eq? (left node) 'nil))
	(if (> (height (left node)) h)
	    (set! h (height (left node)))))
    (if (not (eq? (right node) 'nil))
	(if (> (height (right node)) h)
	    (set! h (height (right node)))))
    (height! node (+ h 1))))

;;! (insert-node is-less? node ndata) is used to insert a new node
;; in the tree. It will use the 'is-less?' function to determine
;; the right order in the tree. Insert-node will traverse the tree
;; and insert a new node at the point in the tree that puts the
;; ndata argument in the right order of the tree. After inserting
;; the new node, the tree is rebalanced.
(define (insert-node is-less? node ndata)
  (if (eq? node 'nil)
      (make-avl-node ndata)
      (begin
	(if (is-less? ndata (data node))
	    (left! node (insert-node is-less? (left node) ndata))
	    (right! node (insert-node is-less? (right node) ndata)))
	(balance node))))

;;! (find-node is-equal? is-less? node fdata) finds fdata in the
;; tree. It will use the 'is-less?' and the 'is-equal?' functions
;; to determine how to traverse the tree and to determine the data
;; of a node equals the given fdata.
(define (find-node is-equal? is-less? node fdata)
  (if (eq? node 'nil)
      'nil
      (if (is-equal? fdata (data node))
	  node
	  (if (is-less? fdata (data node))
	      (find-node is-equal? is-less? (left node) fdata)
	      (find-node is-equal? is-less? (right node) fdata)))))

;;! (remove-node is-equal? is-less? node rdata) recursively locates the node to
;; be removed in the avl tree, removes the node (using move-down-righthand-side)
;; and rebalances the tree as needed all the way back up the recursion.
(define (remove-node is-equal? is-less? node rdata decreaser)
  (if (eq? node 'nil)
      'nil
      (if (is-equal? rdata (data node))
	  (begin
	    (decreaser)
	    (move-down-righthand-side (left node) (right node)))
	  (begin
	    (if (is-less? rdata (data node))
		(left! node (remove-node is-equal? is-less? (left node) rdata decreaser))
		(right! node (remove-node is-equal? is-less? (right node) rdata decreaser)))
	    (balance node)))))

(define (move-down-righthand-side node rhs)
  (if (eq? node 'nil)
      rhs
      (begin
	(right! node (move-down-righthand-side (right node) rhs))
	(balance node))))

;;! (node-for-each level node function) works in ascending order through the whole
;; tree and calls function with the data of each node and the level of the node
;; in the tree. Nothing is done with the result of the function. node-for-each
;; is all about side effects.
(define (node-for-each level node function)
  (if (eq? node 'nil)
      'nil
      (begin
	(node-for-each (+ level 1) (left node) function)
	(function (data node) level)
	(node-for-each (+ level 1) (right node) function))))

;;! (node-map level newroot node function) works in ascending order through the
;; whole avl tree and calls function with the data of each node and the level
;; of the node int the tree. The result of the function is inserted into
;; newroot.
(define (node-map level newroot node function)
  (if (eq? node 'nil)
      'nil
      (begin
	(node-map (+ level 1) newroot (left node) function)
	(avl-insert! newroot (function (data node) level))
	(node-map (+ level 1) newroot (right node) function))))

;;! (node-filter level newroot node function) works in ascending order through the
;; whole avl tree and calls function with the data and the level of each node.
;; The function is expected to be a boolean function. If function returns #t,
;; the current node is inserted into newroot, otherwise not.
(define (node-filter level newroot node function)
  (if (eq? node 'nil)
      'nil
      (begin
	(node-filter (+ level 1) newroot (left node) function)
	(if (function (data node) level)
	    (avl-insert! newroot (data node)))
	(node-filter (+ level 1) newroot (right node) function))))

;;! (balance node) rebalances a subtree, by rotating nodes. It does this
;; only, if the difference-in-height between the left hand side and
;; the right hand side of a node is E<lt> -1 or E<gt> 1.
(define (balance node)
  (define (exchange-left node parent)
    (right! parent (left node))
    (left! node (balance parent))
    (balance node))
  (define (exchange-right node parent)
    (left! parent (right node))
    (right! node (balance parent))
    (balance node))
  (define (rotate-left node)
    (exchange-left (right node) node))
  (define (rotate-right node)
    (exchange-right (left node) node))
  (define (difference-in-height node)
    (let ((lh (if (eq? (left node) 'nil) 0 (height (left node))))
	  (rh (if (eq? (right node) 'nil) 0 (height (right node)))))
      (- lh rh)))
  (let ((d (difference-in-height node)))
    (if (or (< d -1) (> d 1))
	(if (< d 0)
	    (begin
	      (if (> (difference-in-height (right node)) 0)
		  (right! node (rotate-right (right node))))
	      (rotate-left node))
	    (begin
	      (if (< (difference-in-height (left node)) 0)
		  (left! node (rotate-left (left node))))
	      (rotate-right node)))
	(begin
	  (compute-height node)
	  node))))

;;! (node-min node) returns the left most node in the avl tree (which will
;; hold the minimum data). This function is used by C<avl-min>.
(define (node-min node)
  (if (eq? (left node) 'nil)
      node
      (node-min (left node))))

;;! (node-max node) returns the right most node in the avl tree (which will
;; hold the maximum data). This function is used by C<avl-max>.
(define (node-max node)
  (if (eq? (right node) 'nil)
      node
      (node-max (right node))))

;;-------------------------------------------------------------------------------

;;! AVL Tree data structure
;; Implement thread safety using a monitor section (TODO)
(define-type avl
  (is-equal is-equal is-equal!)
  (is-less is-less is-less!)
  (root root root!)
  (nodes nodes nodes!)
  (sem sem sem!)
  (me me me!))

;; Internal semaphore implementation based on mutexes
;; (from Gambit documentation)
(define (make-semaphore n)
  (vector n (make-mutex) (make-condition-variable)))
(define (semaphore-wait! sema)
  (mutex-lock! (vector-ref sema 1))
  (let ((n (vector-ref sema 0)))
    (if (> n 0)
        (begin
          (vector-set! sema 0 (- n 1))
          (mutex-unlock! (vector-ref sema 1)))
        (begin
          (mutex-unlock! (vector-ref sema 1) (vector-ref sema 2))
          (semaphore-wait! sema)))))
(define (semaphore-signal! sema)
  (mutex-lock! (vector-ref sema 1))
  (let ((n (+ (vector-ref sema 0) 1)))
    (vector-set! sema 0 n)
    (if (> n 0)
        (condition-variable-broadcast! (vector-ref sema 2)))
    (mutex-unlock! (vector-ref sema 1))))

(define-syntax avl%protect
  (syntax-rules ()
    ((_ %avl body)
     (let ((sem-set
            ;; Conditional semaphore locking,
            ;; to provide recursive avl%protection
	    (if (not (eq? (me %avl) (current-thread)))
		(begin
		  (semaphore-wait! (sem %avl))
		  (me! %avl (current-thread))
		  #t)
		#f)))
       (let ((result body))
	 (if sem-set
	     (begin
	       (me! %avl 'me-done)
	       (semaphore-signal! (sem %avl))))
	 result)))))

;;-------------------------------------------------------------------------------
;;!! Avl Tree Interface

;;! Given a function 'is-equal?' that determines if two objects in an
;; avl tree are equal, and a function 'is-less' that determines wheter
;; one object is 'less than' an other object, a new avl-tree
;; is created with the 'avl' function.
(define (avl is-equal? is-less?)
  (make-avl (vector 'avl 'nil is-equal? is-less? 0 (make-semaphore 1) 'me)))

;;! The 'avl-from-avl' function creates a new avl-tree from a given
;; avl tree, which 'donates' the 'is-equal?' and 'is-less?' functions.
(define (avl-from-avl troot)
  (avl (is-equal troot) (is-less troot)))

;;! Inserts obj in avl by calling insert-node. Returns avl.
;; This is not a functional approach, as the avl tree
;; is updated in place.
(define (avl-insert! avl obj)
  (avl%protect avl
               (begin
                 (root! avl (insert-node (is-less avl) (root avl) obj))
                 (nodes! avl (+ (nodes avl) 1))
                 avl)))

;;! Removes obj from avl (if obj exists in avl). Returns avl.
;; This is not a functional approach, as the avl tree
;; is updated in place.
(define (avl-remove! avl obj)
  (avl%protect avl
               (root! avl (remove-node (is-equal avl)
                                       (is-less avl)
                                       (root avl)
                                       obj
                                       (lambda () (nodes! avl (- (nodes avl) 1)))))))

;;! avl-find looks up obj in avl by calling find-node. If no node containing
;; obj is found, not-founc-func, which must be a function with no arguments, is called.
;; Either (data node) is returned, or the result of not-found-func.
(define (avl-find avl obj not-found-func)
  (avl%protect avl
               (let ((r (find-node (is-equal avl) (is-less avl) (root avl) obj)))
                 (if (eq? r 'nil)
                     (not-found-func)
                     (data r)))))

;;! Checks wheter obj exists in avl. Returns #t, if so, returns #f, otherwise.
;; Be carefull, when using this function on an avl tree in a threaded environment. The result
;; of this function may not be valid anymore because an other thread may have
;; inserted or removed an object from the tree.
(define (avl-exists? avl obj)
  (let ((found #t))
    (avl-find avl obj (lambda () (set! found #f) #f))
    found))

;;! Constructs a new avl tree from avl mapping function on the data and the
;; level in the tree of each node (in ascending order). (i.e., ascending order
;; in terms of the is-less? function). Function is a function that takes
;; two arguments: data and level, e.g.: (avl-map avl (lambda (obj level) (+ obj level))).
;; If is-equal-and-is-less is the empty list, the new avl tree will be
;; constructed from avl. Otherwise, a new avl tree will be constructed,
;; with is-equal? as (car is-equal-and-is-less) and is-less? as (cadr is-equal-and-is-less).
(define (avl-map troot function . iseq-isless)
  (avl%protect troot
               (let ((newroot (avl (if (null? iseq-isless)
                                       (is-equal troot)
                                       (car iseq-isless))
                                   (if (null? iseq-isless)
                                       (is-less troot)
                                       (cadr iseq-isless)))))
                 (node-map 0 newroot (root troot)  function)
                 newroot)))

;;! Calls function for each node of avl. Function is a function that takes
;; the data and the level of a node as arguments (see avl-map).
(define (avl-for-each avl function)
  (avl%protect avl
               (begin
                 (node-for-each 0 (root avl) function)
                 avl)))

;;! Constructs a new avl tree from C<avl>, inserting all nodes of C<avl> for which
;; function, which is a function that takes the data and level of each node
;; as arguments, returns #t. The result of C<function> must be of type boolean.
;;
;; Example: (avl-filter avl (lambda (obj level) (E<gt> obj 0))) filters out all
;; nodes for which data has a value E >= 0.
(define (avl-filter troot filter-function-boolean)
  (avl%protect troot
               (let ((newroot (avl (is-equal troot) (is-less troot))))
                 (node-filter 0 newroot (root troot) filter-function-boolean)
                 newroot)))

;;! Returns the number of nodes in avl.
(define (avl-nodes avl)
  (nodes avl))

;;! Returns #t, if the avl tree has no nodes; #f otherwise.
(define (avl-empty? avl)
  (= (nodes avl) 0))

;;! Will return the minimum data in the avl tree C<avl>, or
;; avl-no-nodes, if (avl-nodes avl) equals 0.
(define (avl-min troot)
  (avl%protect troot
               (if (= (avl-nodes troot) 0)
                   'avl-no-nodes
                   (data (node-min (root troot))))))

;;! Will return the maximum data in the avl tree avl, or
;; 'avl-no-nodes, if (avl-nodes avl) equals 0.
(define (avl-max troot)
  (avl%protect troot
               (if (= (avl-nodes troot) 0)
                   'avl-no-nodes
                   (data (node-max (root troot))))))

;;! This function will call a function with the tree in a critical section,
;; i.e., make the function call atomic.
;;
;; Example of use:
;;
;;           (avl-atomic avl (lambda (avl)
;;		                (if (avl-empty? avl)
;;                                   #f
;;		                    (begin
;;			              (avl-remove! avl (avl-min avl))
;;			              #t))))
;;
(define (avl-atomic avl func)
  (avl%protect avl
               (func avl)))
