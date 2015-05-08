;;!!! Treaps in Scheme
;; .author Oleg Kiselyov
;;
;; An implementation of an ordered dictionary data structure, based
;; on randomized search trees (treaps) by Seidel and Aragon:
;;
;; 	R. Seidel and C. R. Aragon. Randomized Search Trees.
;; 	Algorithmica, 16(4/5):464-497, 1996.
;;
;; This code defines a treap object that implements an ordered dictionary
;; mapping of keys to values. The object responds to a variety of query and
;; update messages, including efficient methods for finding the minimum and
;; maximum keys and their associated values as well as traversing of a
;; treap in an ascending or descending order of keys. Looking up an arbitrary
;; or the min/max keys, and deleting the min/max keys require no more
;; key comparisons than the depth of the treap, which is O(log n) where
;; n is the total number of keys in the treap. Arbitrary key deletion and
;; insertions run in O(log n) _amortized_ time.
;;
;; This code is inspired by a Stefan Nilsson's article "Treaps in Java"
;; (Dr.Dobb's Journal,  July 1997, p.40-44) and by the Java implementation
;; of treaps described in the article. Yet this Scheme code has been
;; developed completely from scratch, using the description of the algorithm
;; given in the article, and insight gained from examining the Java source
;; code. As a matter of fact, treap insertion and deletion algorithms
;; implemented in this code differ from the ones described in the article
;; and implemented in the Java code; this Scheme implementation uses fewer
;; assignments and comparisons (see below for details). Some insight as
;; to a generic tree interface gleaned from wttree.scm, "Weight balanced
;; trees" by Stephen Adams (a part of The Scheme Library, slib2b1).
;;
;; A treap is a regular binary search tree, with one extension. The extension
;; is that every node in a tree, beside a key, a value, and references to
;; its left and right children, has an additional constant field, a priority.
;; The value of this field is set (to a random integer number) when the node
;; is constructed, and is not changed afterwards. At any given moment,
;; the priority of every non-leaf node never exceeds the priorities of its
;; children. When a new node is inserted, we check that this invariant holds;
;; otherwise, we perform a right or left rotation that swaps a parent and
;; its kid, keeping the ordering of keys intact; the changes may need to be
;; propagated recursively up. The priority property, and the fact they are
;; chosen at random, makes a treap look like a binary search tree built by
;; a random sequence of insertions. As the article shows, this makes a treap
;; a balanced tree: the expected length of an average search path is roughly
;; 1.4log2(n)-1, and the expected length of the longest search path is about
;; 4.3log2(n). See the Stefan Nilsson's article for more details.
;;
;; The treap object is created by a make-treap function, the only user-visible
;; function defined in this code:
;;	procedure:  make-treap KEY-COMPARE-PROC
;; where KEY-COMPARE-PROC is a user-supplied function
;;	KEY-COMPARE-PROC key1 key2
;; that takes two keys and returns a negative, positive, or zero number
;; depending on how the first key compares to the second.
;;
;; The treap object responds to the following messages (methods):
;;	'get
;;		returns a procedure LAMBDA KEY . DEFAULT-CLAUSE
;;		which searches the treap for an association with a given
;;		KEY, and returns a (key . value) pair of the found association.
;;		If an association with the KEY cannot be located in the treap,
;;		the PROC returns the result of evaluating the DEFAULT-CLAUSE.
;;		If the default clause is omitted, an error is signalled.
;;		The KEY must be comparable to the keys in the treap
;;		by a key-compare predicate (which has been specified
;;		when the treap was created)
;;
;;	'get-min
;;		returns a (key . value) pair for an association in the
;;		treap with the smallest key. If the treap is empty, an error
;;		is signalled.
;;	'delete-min!
;;		removes the min key and the corresponding association
;;		from the treap. Returns a (key . value) pair of the
;;		removed association.
;;		If the treap is empty, an error is signalled.
;;	'get-max
;;		returns a (key . value) pair for an association in the
;;		treap with the largest key. If the treap is empty, an error
;;		is signalled.
;;	'delete-max!
;;		removes the max key and the corresponding association
;;		from the treap. Returns a (key . value) pair of the
;;		removed association.
;;		If the treap is empty, an error is signalled.
;;
;;	empty?
;;		returns #t if the treap is empty
;;
;;	size
;;		returns the size (the number of associations) in the treap
;;
;;	depth
;;		returns the depth of the tree. It requires the complete
;;		traversal of the tree, so use sparingly
;;
;;	clear!
;;		removes all associations from the treap (thus making it empty)
;;
;;	'put!
;;		returns a procedure LAMBDA KEY VALUE
;;		which, given a KEY and a VALUE, adds the corresponding
;;		association to the treap. If an association with the same
;;		KEY already exists, its value is replaced with the VALUE
;;		(and the old (key . value) association is returned). Otherwise,
;;		the return value is #f.
;;
;;	'delete!
;;		returns a procedure LAMBDA KEY . DEFAULT-CLAUSE
;;		which searches the treap for an association with a given KEY,
;;		deletes it, and returns a (key . value) pair of the found
;;		and deleted association.
;;		If an association with the KEY cannot be located in the treap,
;;		the PROC returns the result of evaluating the DEFAULT-CLAUSE.
;;		If the default clause is omitted, an error is signalled.
;;
;;	for-each-ascending
;;		returns a procedure LAMBDA PROC that will apply the given
;;		procedure PROC to each (key . value) association of the treap,
;;		from the one with the smallest key all the way to the one with
;;		the max key, in an ascending order of keys.
;;		The treap must not be empty.
;;
;;	for-each-descending
;;		returns a procedure LAMBDA PROC that will apply the given
;;		procedure PROC to each (key . value) association of the treap,
;;		in the descending order of keys.
;;		The treap must not be empty.
;;
;;	debugprint
;;		prints out all the nodes in the treap, for debug purposes
;;
;;	;;alist->
;;
;;
;; Notes on the algorithm
;; As the DDJ paper shows, insertion of a node into a treap is a simple
;; recursive algorithm, Example 1 of the paper. This algorithm is implemented
;; in the accompanying source [Java] code as
;; <BLOCKQUOTE>
;;   private Tree insert(Tree node, Tree tree) {
;;      if (tree == null) return node;
;;      int comp = node.key.compareTo(tree.key);
;;      if (comp < 0) {
;;         tree.left = insert(node, tree.left);
;;         if (tree.prio > tree.left.prio)
;;            tree = tree.rotateRight();
;;      } else if (comp > 0) {
;;         tree.right = insert(node, tree.right);
;;         if (tree.prio > tree.right.prio)
;;            tree = tree.rotateLeft();
;;      } else {
;;         keyFound = true;
;;         prevValue = tree.value;
;;         tree.value = node.value;
;;      }
;;      return tree;
;;   }
;; </BLOCKQUOTE>
;;
;; This algorithm, however, is not as efficient as it could be. Suppose we
;; try to insert a node which turns out to already exist in the tree,
;; at a depth D. The algorithm above would descend into this node in the
;; winding phase of the algorithm, replace the node's value, and, in the
;; unwinding phase of the recursion, would perform D assignments of the kind
;;	tree.left = insert(node, tree.left);
;; and D comparisons of nodes' priorities. None of these priority checks and
;; assignments are actually necessary: as we haven't added any new node,
;; the tree structure hasn't changed.
;;
;; Therefore, the present Scheme code implements a different insertion
;; algorithm, which avoids doing unnecessary operations. The idea is simple:
;; if we insert a new node into some particular branch of the treap and verify
;; that this branch conforms to the treap priority invariant, we are certain
;; the invariant holds throughout the entire treap, and no further checks
;; (up the tree to the root) are necessary. In more detail:
;;	- Starting from the root, we recursively descend until we find
;;	  a node with a given key, or a place a new node can be inserted.
;;	- We insert the node and check to see if its priority is less than
;;	  that of its parent. If this is the case, we left- or right-rotate
;;	  the tree to make the old parent a child of the new node, and the
;;	  new node a new root of this particular branch. We return this new
;;	  parent as an indication that further checks up the tree are
;;	  necessary. If the new node conforms to the parent's priority, we
;;	  insert it and return #f
;;	- On the way up, we check the priorities again and rotate the tree
;;	  to restore the priority invariant at the current level if needed.
;;	- If no changes are made at the current level, we return a flag #f
;;	  meaning that no further changes or checks are necessary at the
;;	  higher levels.
;; Thus, if a new node was originally inserted at a level D in the tree (level
;; 0 being the root) but then migrated up by L levels (because of its priority),
;; the original insertion algorithm would perform (D-1) assignments,
;; (D-1) priority checks, plus L rotations (at a cost of 2 assignments in the
;; treap each). Our algorithm does only (L+1) node priority checks and
;; max(2(L-1)+2,1) assignments.
;; Note if priorities are really (uniformly) random, L is uniformly distributed
;; over [0,D], so the average cost of our algorithm is
;; 	D/2 +1 checks and D assignments
;; compared to
;;	D-1 checks and 2D-1 assignments
;; for the original algorithm described in the DDJ paper.
;;
;; The similar gripe applies to the Java implementation of a node deletion
;; algorithm:
;; <BLOCKQUOTE>
;;   private Tree delete(Ordered key, Tree t) {
;;      if (t == null) return null;
;;      int comp = key.compareTo(t.key);
;;      if (comp < 0)
;;         t.left = delete(key, t.left);
;;      else if (comp > 0)
;;         t.right = delete(key, t.right);
;;      else {
;;         keyFound = true;
;;         prevValue = t.value;
;;         t = t.deleteRoot();
;;      }
;;      return t;
;;   }
;; </BLOCKQUOTE>
;;
;; The algorithm as implemented looks fully-recursive. Furthermore, deletion
;; of a node at a level D in the treap involves at least D assignments, most
;; of them being unnecessary. Indeed, if a node being deleted is a leaf, only
;; one change to the tree is needed to detach the node. Deleting a node
;; obviously requires a left- or a right-kid field of the node's parent be
;; modified (cleared). This change, however does NOT need to be propagated up:
;; deleting of a node does not violate neither ordering nor priority invariants
;; of the treap; all changes are confined to a branch rooted at the
;; parent of the deleted node.
;; This Scheme code implements node deletion algorithm in the optimal way,
;; performing only those assignments which are absolutely necessary, and
;; replacing full recursions with tail recursions (which are simply iterations).
;; Our implementation is also simpler and clearer, making use of a helper
;; procedure join! to join two treap branches (while keeping both treap
;; invariants intact). The deletion algorithm can then be expressed as
;; replacing a node with a join of its two kids; compare this explanation
;; to the one given in the DDJ paper!
;;
;; $Id: treap.scm,v 1.3 2004/07/08 21:00:24 oleg Exp $

(declare                            ; Compilation optimization options
 (block)
 (standard-bindings)
 (fixnum make-treap))

;; Introduce a 'put! funtion to act as insert in STL?
;; std::map::insert inserts association with a new key in a map.
;; It does not modify the map if the key is already in the map.
;; The insert function returns a pair. The second element is a boolean:
;; #t if the actual insertion took place.
;; The first element is an iterator that points to the key-value pair
;; in the map that has the given key, regardless of whether that pair
;; has just been inserted or it was already there.

;;(define wt-tree/split< #f)
;;(define wt-tree/split> #f)
;;(define wt-tree/union #f)
;;(define wt-tree/intersection #f)
;;(define wt-tree/difference #f)
;;(define wt-tree/subset? #f)
;;(define wt-tree/set-equal? #f)
;;(define wt-tree/fold #f)


(define (make-treap key-compare)
  ;; Treaps package needs a random number generator to
  ;; generate values of nodes' priorities.
  ;; Here is the most primitive linear congruential generator,
  ;; which is equivalent to the "standard" UNIX rand()
  ;; It returns an integral random number uniformly distributed
  ;; within [0,2^15-1]
  ;; All bad words that have been said about rand() equally apply
  ;; here. Still, for this present application, less than perfect
  ;; spectral properties of this generator aren't too important.
  (define random
    (let ((state 5))
      (lambda ()
        (set! state (modulo (+ (* state 1103515245) 12345) #x7fff))
        state)))

  ;; a node of a tree, a vector of
  ;;   slot 0 - key, anything that key-compare could be applied to
  ;;   slot 1 - value, any object associated with the key
  ;;   slot 2 - left-kid, #f if absent
  ;;   slot 3 - right-kid
  ;;   slot 4 - prio, a priority of the node (a FIXNUM random number)

  (define (new-leaf key value)
    (vector key value #f #f (random)))

  (define-macro (node:key node)   `(vector-ref ,node 0))
  (define-macro (node:key-value node)  `(cons (vector-ref ,node 0)
      					      (vector-ref ,node 1)))
  (define-macro (node:value-set! node value)   `(vector-set! ,node 1 ,value))

  (define-macro (node:left-kid node)   `(vector-ref ,node 2))
  (define-macro (node:right-kid node)  `(vector-ref ,node 3))
  (define-macro (node:left-kid-set! node new-kid)
    `(vector-set! ,node 2 ,new-kid))
  (define-macro (node:right-kid-set! node new-kid)
    `(vector-set! ,node 3 ,new-kid))

  (define-macro (node:priority node) `(vector-ref ,node 4))
  (define-macro (node:unsubordination? parent kid)
    `(##fixnum.> (node:priority ,parent) (node:priority ,kid)))

  (define-macro (node:dispatch-on-key node key on-less on-equal on-greater)
    (let ((result (gensym)))
      `(let ((,result (key-compare ,key (vector-ref ,node 0))))
         (cond
          ((zero? ,result) ,on-equal)
          ((positive? ,result) ,on-greater)
          (else ,on-less)))))

  (define (node:debugprint node)
    (println " " (node:key-value node) ", kids "
             (cons (not (not (node:left-kid node)))
                   (not (not (node:right-kid node))))
             ", prio " (node:priority node) "\n"))


  (let ((root #f) (size 0))
    ;; Looking up assocaitions in a treap: just like in any search tree
    ;; Given  a key, return the corresponding (key . value) association
    ;; in the treap, or #f if the treap does not contain an association
    ;; with that key
    ;; This procedure takes as many comparisons (evaluations of the
    ;; key-compare procedure) as the depth of the found node
    (define (locate-assoc key)
      (let loop ((node root))
        (and node
             (node:dispatch-on-key node key
                                   (loop (node:left-kid node))
                                   (node:key-value node)
                                   (loop (node:right-kid node))))))

    (define-macro (locate-extremum-node branch-selector)
      `(if (not root) (error "empty tree")
           (let loop ((node root) (parent #f))
             (if node (loop (,branch-selector node) node)
                 (node:key-value parent)))))

                                        ; in-order traversal of the treap
    (define-macro (for-each-inorder primary-branch-selector
				    secondary-branch-selector)
      `(lambda (proc)
         (if (not root) (error "empty tree")
             (let loop ((node root))
               (when node
                     (loop (,primary-branch-selector node))
                     (proc (node:key-value node))
                     (loop (,secondary-branch-selector node)))))))

    (define (get-depth)
      (let loop ((node root) (level 0))
        (if (not node) level
            (max (loop (node:left-kid node) (++ level))
                 (loop (node:right-kid node) (++ level))))))

    ;; debug printing of all nodes of the tree in-order
    ;; in an ascending order of keys
    (define (debugprint)
      (println "\nThe treap contains " size " nodes\n")
      (let loop ((node root) (level 0))
        (when node
              (loop (node:left-kid node) (++ level))
              (string-append "  level " level)
              (node:debugprint node)
              (loop (node:right-kid node) (++ level))))
      (newline)
      (newline))

    ;; Adding a new association to the treap (or replacing the old one
    ;; if existed). Return the (key . value) pair of an old (existed
    ;; and replaced association), or #f if a new association was really
    ;; added
    (define (insert! key value)
      (letrec ((new-node (new-leaf key value))
               (old-key-value #f)
               ;; If the left branch of parent is empty, insert the
               ;; new node there, check priorities
               ;; Otherwise, descend recursively
               ;; If the parent got inverted due to a right rotation,
               ;; return the new parent of the branch; otherwise,
               ;; return #f (indicating no further checks are necessary)
               (insert-into-left-branch
                (lambda (key parent)
                  (let ((old-left-kid (node:left-kid parent)))
                    ;; Found a place to insert the 'new-node': as the left
                    ;; leaf of the parent
                    (if (not old-left-kid)
                        (cond
                         ((node:unsubordination? parent new-node)
                          ;; Right rotation over the new-leaf
                          (node:right-kid-set! new-node parent)
                          new-node)	; becomes a new parent
                         (else
                          (node:left-kid-set! parent new-node)
                          #f))
                        ;; Insert the new-leaf into a branch rooted
                        ;; on old-left-kid
                        (let ((new-left-kid
                               (node:dispatch-on-key old-left-kid key
                                                     (insert-into-left-branch key old-left-kid)
                                                     (update-existing-node old-left-kid)
                                                     (insert-into-right-branch key old-left-kid))))
                          (and new-left-kid
                               ;; That branch got a new root
                               (cond
                                ((node:unsubordination? parent new-left-kid)
                                 ;; Right rotation over the new-left-kid
                                 (node:left-kid-set! parent
                                                     (node:right-kid new-left-kid))
                                 (node:right-kid-set! new-left-kid parent)
                                 new-left-kid) ; becomes a new parent
                                (else
                                 (node:left-kid-set! parent new-left-kid)
                                 #f))))))))
               ;; If the right branch of parent is empty, insert the
               ;; new node there, check priorities
               ;; Otherwise, descend recursively
               ;; If the parent got inverted due to a left rotation,
               ;; return the new parent of the branch; otherwise,
               ;; return #f (indicating no further checks are necessary)
               (insert-into-right-branch
                (lambda (key parent)
                  (let ((old-right-kid (node:right-kid parent)))
                    ;; Found a place to insert the 'new-node': as the right
                    ;; leaf of the parent
                    (if (not old-right-kid)
                        (cond
                         ((node:unsubordination? parent new-node)
                          ;; Left rotation over the new-leaf
                          (node:left-kid-set! new-node parent)
                          new-node)	; becomes a new parent
                         (else
                          (node:right-kid-set! parent new-node)
                          #f))
                        ;; Insert the new-leaf into a branch rooted
                        ;; on old-right-kid
                        (let ((new-right-kid
                               (node:dispatch-on-key old-right-kid key
                                                     (insert-into-left-branch key old-right-kid)
                                                     (update-existing-node old-right-kid)
                                                     (insert-into-right-branch key old-right-kid))))
                          (and new-right-kid
                               ;; That branch got a new root
                               (cond
                                ((node:unsubordination? parent new-right-kid)
                                 ;; Left rotation over the new-right-kid
                                 (node:right-kid-set! parent
                                                      (node:left-kid new-right-kid))
                                 (node:left-kid-set! new-right-kid parent)
                                 new-right-kid)	; becomes a new parent
                                (else
                                 (node:right-kid-set! parent new-right-kid)
                                 #f))))))))
               (update-existing-node
                (lambda (node)
                  (set! old-key-value (node:key-value node))
                  (node:value-set! node value)
                  #f)))			; end of letrec
        ;; insert's body
        (cond
         ;; insert into an empty tree
         ((not root) (set! root new-node))
         (else
          (let ((new-root
                 (node:dispatch-on-key root key
                                       (insert-into-left-branch key root)
                                       (update-existing-node root)
                                       (insert-into-right-branch key root))))
            (if new-root
                (set! root new-root)))))
        (if (not old-key-value)
            (++! size))        ; if the insertion has really occurred
        old-key-value))
    ;; Deleting existing associations from the treap
    (define-macro (delete-extremum-node! branch-selector
                                         branch-setter the-other-branch-selector)
      `(cond
        ((not root) (error "empty tree"))
        ((not (,branch-selector root))	; root is the extreme node
         (let ((result (node:key-value root)))
           (set! root (,the-other-branch-selector root))
           (--! size)
           result))
        (else
         (let loop ((node (,branch-selector root)) (parent root))
           (let ((kid (,branch-selector node)))
             (if kid (loop kid node)
                 (let ((result (node:key-value node)))
                   (,branch-setter parent (,the-other-branch-selector node))
                   (--! size)
                   result)))))))
    ;; Given two treap branches (both of which could be empty)
    ;; which satisfy both the order invariant and the priority invariant
    ;; (all keys of all the nodes in the right branch are strictly bigger
    ;; than the keys of left branch nodes), join them
    ;; while keeping the sorted and priority orders intact
    (define (join! left-branch right-branch)
      (cond
       ((not left-branch) right-branch) ; left-branch was empty
       ((not right-branch) left-branch) ; right-branch was empty
       ((node:unsubordination? left-branch right-branch)
                                        ; the root of the right-branch should be the new root
        (node:left-kid-set! right-branch
                            (join! left-branch (node:left-kid right-branch)))
        right-branch)
       (else
                                        ; the root of the left-branch should be the new root
        (node:right-kid-set! left-branch
                             (join! (node:right-kid left-branch) right-branch))
        left-branch)))
    ;; Find an association with a given KEY, and delete it.
    ;; Return the (key . value) pair of the deleted association, or
    ;; #f if it couldn't be found
    (define (delete! key)
      (define (delete-node! node parent from-left?)
        (let ((old-assoc (node:key-value node))
              (new-kid (join! (node:left-kid node) (node:right-kid node))))
          (--! size)
          (if parent
              (if from-left?
                  (node:left-kid-set! parent new-kid)
                  (node:right-kid-set! parent new-kid))
              ;; Deleting of the root node
              (set! root new-kid))
          old-assoc))
      (let loop ((node root) (parent #f) (from-left? #t))
        (and node
             (node:dispatch-on-key node key
                                   (loop (node:left-kid node) node #t)
                                   (delete-node! node parent from-left?)
                                   (loop (node:right-kid node) node #f)))))

    (define (apply-default-clause key default-clause)
      (cond
       ((null? default-clause)
        (error "key " key " was not found in the treap "))
       ((pair? (cdr default-clause))
        (error "default argument must be a single clause"))
       ((procedure? (car default-clause)) ((car default-clause)))
       (else (car default-clause))))

    ;; Dispatcher
    (lambda (selector)
      (case selector
        ((get)
         (lambda (key . default-clause)
           (or (locate-assoc key) (apply-default-clause key default-clause))))

        ((delete!)
         (lambda (key . default-clause)
           (or (delete! key) (apply-default-clause key default-clause))))

        ((get-min) (locate-extremum-node node:left-kid))
        ((get-max) (locate-extremum-node node:right-kid))
        ((delete-min!)
         (delete-extremum-node! node:left-kid node:left-kid-set!
                                node:right-kid))
        ((delete-max!)
         (delete-extremum-node! node:right-kid node:right-kid-set!
                                node:left-kid))
        ((empty?) (not root))
        ((size)   size)
        ((depth)   (get-depth))
        ((clear!)  (set! root #f) (set! size 0))
        ((put!) insert!)
        ((for-each-ascending) (for-each-inorder node:left-kid node:right-kid))
        ((for-each-descending) (for-each-inorder node:right-kid node:left-kid))
        ((debugprint) (debugprint))
        (else
         (error "Unknown message " selector " sent to a treap"))))))
