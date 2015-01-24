;;!!! Random sampling of trees
;; .author Oleg Kiselyov
;; .author Alvaro Castro-Castilla, 2015. See LICENSE


;;! Select a uniformly distributed random node from a tree in one pass,
;; without an a priori knowledge on the number of nodes in the tree.
;; We consider '() to be the absence of a child. Thus
;; '(a . ()) is a tree with one, left child.
;; '(() . a) is a tree with one right child.
;; We do not count leaves as nodes. Only internal nodes (aka, pairs)
;; count.
;;
;; The algorithm is an instance of a Reservoir sampling:
;; Select at random N records from a sequence -- without a
;; priori knowledge of the total number of records in a sequence
;; (provided that this number is greater than N). The method guarantees
;; that each record is selected with a probability of N/M, where M is the
;; total number of the records in the sequence.
;;
;; See
;; Reservoir Sampling
;; by Paul F. Hultquist and William R. Mahoney
;; Dr. Dobbs J., January 2001, p. 189.
;; The "Algorithm Alley" column.
;; The algorithm was originally developed by Alan Waterman.
;;
;;   procedure random-node TREE -> [COUNT NODE]
;; Traverse the TREE _once_ and return COUNT, the total number of nodes
;; in the tree, and NODE -- one node of the tree selected with
;; the probability of 1/COUNT. TREE must be a pair.

(define (random-node tree)
  ;; We assume that a procedure (random i j) returns a random integer k,
  ;; i <= k <= j, that is uniformly distributed within the interval [i,j]
  ;; (endpoints included!)
  (define (random a b) (+ a (random-integer (+ 1 (- b a)))))
  (let select ((node  (car tree)) (p 1) (random-sample tree)
	       (todo (list (cdr tree))))
    (if (not (pair? node))              ; Leaves don't count
        (if (null? todo) (values p random-sample)
            (select (car todo) p random-sample (cdr todo)))
        (let*
            ((p (+ 1 p))
             (k (random 1 p))
             (random-sample (if (= k 1) node random-sample)))
          (if (pair? node)
              (select (car node) p random-sample (cons (cdr node) todo))
              (if (null? todo) (values p random-sample)
                  (select (car todo) p random-sample (cdr todo))))))))

;; Same as random-node, but count leaves too.
(define (random-node/leaves tree)
  ;; We assume that a procedure (random i j) returns a random integer k,
  ;; i <= k <= j, that is uniformly distributed within the interval [i,j]
  ;; (endpoints included!)
  (define (random a b) (+ a (random-integer (+ 1 (- b a)))))
  (let select ((node  (car tree)) (p 1) (random-sample tree)
	       (todo (list (cdr tree))))
    (if (null? node)
        (if (null? todo) (values p random-sample)
            (select (car todo) p random-sample (cdr todo)))
        (let*
            ((p (+ 1 p))
             (k (random 1 p))
             (random-sample (if (= k 1) node random-sample)))
          (if (pair? node)
              (select (car node) p random-sample (cons (cdr node) todo))
              (if (null? todo) (values p random-sample)
                  (select (car todo) p random-sample (cdr todo))))))))



;; Article posting headers:
;;   Date: Tue, 15 Apr 2003 22:17:15 -0700
;;   Newsgroups: comp.lang.scheme
;;   Subject: Re: random node in tree
;;   References: <mYZma.4116$8T6.318252@news20.bellglobal.com>
;;   Message-ID: <7eb8ac3e.0304152117.3124c038@posting.google.com>
;; $Id: random-tree-node.scm,v 1.1 2003/04/17 02:13:43 oleg Exp oleg $

;; Proof of the algorithm.
;; Claim:
;; At each invocation of 
;; 	(select node p reservoir todo)
;; p>=1 is the number of previously traversed nodes (up to but not including
;;   'node')
;; 'node' is either '() or the the (p+1)-th node of the tree
;; reservoir is the node randomly selected from the traversed
;;     with the probability 1/p
;; todo is the stack of right brothers of 'node'
;;
;; Proof by induction:
;; Base case: initial invocation.
;; 'node' is the left child of the root or '(), todo a singleton list
;; that contains its right brother, p = 1 and reservoir is the traversed
;; node (which is the root).
;; Claim holds.
;;
;; Induction hypothesis: Claim holds after q nodes are traversed, 
;; as we enter (select node q reservoir todo)
;; If 'node' does not count (it's '() or a leaf, if we don't count leaves)
;; we skip it and continue the pre-order traversal. 
;; If 'node' is the node that counts, we set q' to q+1, k to be a
;; number uniformly distributed 1 <= k <= q'. The number k has the probability
;; 1/q' = 1/(q+1) of being 1.
;; We set reservoir to be 'node' with the probability 1/(q+1),
;; we maintain the current value of the reservoir with the probability
;; 1 - 1/(q+1) = q/(q+1). Thus reservoir is one of the q previously
;; traversed nodes selected with the probability 1/q * q/(q+1) = 1/(q+1).
;; If node has children, we recursively enter select, with
;; the first argument being the left child of 'node' or nil, 
;; the second argument (q+1) -- the number of nodes traversed,--
;; reservoir is the node selected uniformly at random from the traversed,
;; todo is the (grown) stack of the right brothers of the first argument.
;; The claim holds.
;; If 'node' is not a pair but 'todo' is not empty, we re-enter
;; select but the invariant holds. If we don't count leaves as nodes,
;; the latter alternative does not apply.
;;
;; It follows from the claim that when 'select' exits,
;; it returns the total number of nodes in the tree and one node
;; uniformly randomly selected from the them.


;; Tests
;; In the following we define (random i j) to always return its left
;; boundary.
;; (define (random i j) i)
;; With such a definition of "random", (random-node tree) will return
;; the last traversed node.

;; (define (test-random-node tree)
;;   (display "Tree: ") (display tree) (newline)
;;   (call-with-values 
;;     (lambda () (random-node tree))
;;     (lambda (count rand-node)
;;       (display "total count: ") (display count)
;;       (display " selected node: ") (display rand-node)
;;       (newline))))

;; (test-random-node '(a))
;; (test-random-node '(() . (b c)))
;; (test-random-node '(() . (b . c)))
;; (test-random-node '(* (+ 3 4) 5))

;; Results of the test runs
;; Tree: (a)
;; total count: 1 selected node: (a)
;; Tree: (() b c)
;; total count: 3 selected node: (c)
;; Tree: (() b . c)
;; total count: 2 selected node: (b . c)
;; Tree: (* (+ 3 4) 5)
;; total count: 6 selected node: (5)

