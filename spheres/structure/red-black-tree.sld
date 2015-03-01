;;!!! Red-black tree
;; .author Marc Feeley, 2000-2007
;; .author Massachusetts Institute of Technology, 1993-2000
;; .license lgpl/v2.1
;;
;; File: "rbtree.scm", Time-stamp: <2007-04-05 18:53:11 feeley>
;;
;; Copyright (c) 2000-2007 by Marc Feeley, All Rights Reserved.
;; Copyright (c) 1993-2000 Massachusetts Institute of Technology.
;;
;; This red-black tree implementation is inspired by the code
;; in the MIT-Scheme runtime (file "rbtree.scm").  That code is
;; based on the algorithms presented in the book "Introduction to
;; Algorithms" by Cormen, Leiserson, and Rivest.
;;
;; The main differences with the MIT-Scheme code are:
;;
;;   1) Nil pointers are replaced by a special sentinel that is also
;;      the cell that contains a pointer (in the "left child" field)
;;      to the root of the red-black tree.  The "right child" field of
;;      the sentinel is never accessed.  The sentinel is black.
;;
;;   2) The color field contains #f when the node is red and a
;;      reference to the sentinel when the node is black.  It is thus
;;      possible to find the sentinel from any node in constant time
;;      (if the node is black extract the color field, otherwise
;;      extract the color field of the parent, which must be black).
;;
;;   3) One field of the sentinel always points to the leftmost node of
;;      the red-black tree.  This allows constant time access to the
;;      "minimum" node, which is a frequent operation of priority queues.
;;
;;   4) Several cases are handled specially (see the code for details).
;;
;;   5) Macros are used to generate code specialized for each case of
;;      symmetrical operations (e.g. left and right rotation).
;;
;;   6) Nodes are assumed to be preallocated.  Inserting and deleting a
;;      node from a red-black tree does not perform any heap
;;      allocation.  Moreover, all algorithms consume a constant amount
;;      of stack space.


(define-library (spheres/structure red-black-tree)
  (export define-rbtree)

  (define-macro (define-rbtree
                  rbtree-init!
                  node->rbtree
                  insert!
                  remove!
                  reposition!
                  empty?
                  singleton?
                  before?
                  color
                  color-set!
                  parent
                  parent-set!
                  left
                  left-set!
                  right
                  right-set!
                  leftmost
                  leftmost-set!
                  rightmost
                  rightmost-set!)

    (define (black rbtree)
      rbtree)

    (define (black? rbtree)
      `(lambda (node)
         (,color node)))

    (define (blacken! rbtree)
      `(lambda (node)
         (,color-set! node ,(black rbtree))))

    (define (red)
      #f)

    (define (red?)
      `(lambda (node)
         (not (,color node))))

    (define (reden!)
      `(lambda (node)
         (,color-set! node ,(red))))

    (define (copy-color!)
      `(lambda (node1 node2)
         (,color-set! node1 (,color node2))))

    (define (exchange-color!)
      `(lambda (node1 node2)
         (let ((color-node1 (,color node1)))
           (,color-set! node1 (,color node2))
           (,color-set! node2 color-node1))))

    (define (update-parent!)
      `(lambda (parent-node old-node new-node)
         (if (eq? old-node (,left parent-node))
             (,left-set! parent-node new-node)
             (,right-set! parent-node new-node))))

    (define (rotate! side1 side1-set! side2 side2-set!)
      `(lambda (node)
         (let ((side2-node (,side2 node)))
           (let ((side1-side2-node (,side1 side2-node)))
             (,side2-set! node side1-side2-node)
             (,parent-set! side1-side2-node node))
           (let ((parent-node (,parent node)))
             (,side1-set! side2-node node)
             (,parent-set! node side2-node)
             (,parent-set! side2-node parent-node)
             (,(update-parent!) parent-node node side2-node)))))

    (define (rotate-left!)
      (rotate! left left-set! right right-set!))

    (define (rotate-right!)
      (rotate! right right-set! left left-set!))

    (define (neighbor side other-side)
      `(lambda (node rbtree)
         (let ((side-node (,side node)))
           (if (eq? side-node rbtree)
               (let ((parent-node (,parent node)))
                 (if (or (eq? parent-node rbtree)
                         (eq? node (,side parent-node)))
                     rbtree
                     parent-node))
               (let loop ((x side-node))
                 (let ((other-side-x (,other-side x)))
                   (if (eq? other-side-x rbtree)
                       x
                       (loop other-side-x))))))))

    (define (insert-below! x)
      `(let ((x ,x))
         (if (,before? node x)
             (insert-left! (,left x) x)
             (insert-right! (,right x) x))))

    (define (insert-body side1 rotate-side1! side2 rotate-side2!)
      `(let ((side2-parent-parent-x (,side2 parent-parent-x)))
         (if (,(red?) side2-parent-parent-x)
             (begin
               (,(blacken! 'rbtree) parent-x)
               (,(blacken! 'rbtree) side2-parent-parent-x)
               (,(reden!) parent-parent-x)
               (loop parent-parent-x))
             (let ((y
                    (if (eq? x (,side2 parent-x))
                        (begin
                          (,rotate-side1! parent-x)
                          (,parent parent-x))
                        (,parent x))))
               (,(blacken! 'rbtree) y)
               (let ((parent-y (,parent y)))
                 (,(reden!) parent-y)
                 (,rotate-side2! parent-y))))))

    (define (remove-body side1 rotate-side1! side2 rotate-side2!)
      `(let ((x
              (let ((side2-parent-node (,side2 parent-node)))
                (if (,(red?) side2-parent-node)
                    (begin
                      (,(blacken! 'rbtree) side2-parent-node)
                      (,(reden!) parent-node)
                      (,rotate-side1! parent-node)
                      (,side2 parent-node))
                    side2-parent-node))))

         (define (common-case y)
           (,(copy-color!) y parent-node)
           (,(blacken! 'rbtree) parent-node)
           (,(blacken! 'rbtree) (,side2 y))
           (,rotate-side1! parent-node)
           (,(blacken! 'rbtree) (,left rbtree)))

         (if (,(red?) (,side2 x))
             (common-case x)
             (let ((side1-x (,side1 x)))
               (if (,(black? 'rbtree) side1-x)
                   (begin
                     (,(reden!) x)
                     (fixup! (,parent parent-node) parent-node))
                   (begin
                     (,(blacken! 'rbtree) side1-x)
                     (,(reden!) x)
                     (,rotate-side2! x)
                     (common-case (,side2 parent-node))))))))

    `(begin

       (define (,rbtree-init! rbtree)
         ,@(if leftmost
               `((,leftmost-set! rbtree rbtree))
               `())
         (,(blacken! 'rbtree) rbtree)
         (,parent-set! rbtree rbtree)
         (,left-set! rbtree rbtree)
         rbtree)

       (define (,node->rbtree node)
         (or (,color node)
             (,color (,parent node))))

       (define (,insert! rbtree node)

         (define (fixup!)

           (let loop ((x node))
             (let ((parent-x (,parent x)))
               (if (,(red?) parent-x)
                   (let ((parent-parent-x (,parent parent-x)))
                     (if (eq? parent-x (,left parent-parent-x))
                         ,(insert-body left
                                       (rotate-left!)
                                       right
                                       (rotate-right!))
                         ,(insert-body right
                                       (rotate-right!)
                                       left
                                       (rotate-left!)))))))

           (,(blacken! 'rbtree) (,left rbtree))
           #f)

         (define (insert-left! left-x x)
           (if (eq? left-x rbtree)
               (begin
                 (,left-set! x node)
                 (,parent-set! node x)

                 ;; check if leftmost must be updated

                 ,@(if leftmost
                       `((if (eq? x (,leftmost rbtree))
                             (,leftmost-set! rbtree node)))
                       `())

                 (fixup!))
               ,(insert-below! 'left-x)))

         (define (insert-right! right-x x)
           (if (eq? right-x rbtree)
               (begin
                 (,right-set! x node)
                 (,parent-set! node x)

                 ;; check if rightmost must be updated

                 ,@(if rightmost
                       `((if (eq? x (,rightmost rbtree))
                             (,rightmost-set! rbtree node)))
                       `())

                 (fixup!))
               ,(insert-below! 'right-x)))

         (,(reden!) node)
         (,left-set! node rbtree)
         (,right-set! node rbtree)

         (insert-left! (,left rbtree) rbtree)

         (,parent-set! rbtree rbtree)

         node)

       (define (,remove! node)
         (let ((rbtree (,node->rbtree node)))

           (define (fixup! parent-node node)
             (cond ((or (eq? parent-node rbtree) (,(red?) node))
                    (,(blacken! 'rbtree) node))
                   ((eq? node (,left parent-node))
                    ,(remove-body left
                                  (rotate-left!)
                                  right
                                  (rotate-right!)))
                   (else
                    ,(remove-body right
                                  (rotate-right!)
                                  left
                                  (rotate-left!)))))

           (let ((parent-node (,parent node))
                 (left-node (,left node))
                 (right-node (,right node)))

             (,parent-set! node #f) ;; to avoid leaks
             (,left-set! node #f)
             (,right-set! node #f)

             (cond ((eq? left-node rbtree)

                    ;; check if leftmost must be updated

                    ,@(if leftmost
                          `((if (eq? node (,leftmost rbtree))
                                (,leftmost-set!
                                 rbtree
                                 (if (eq? right-node rbtree)
                                     parent-node
                                     right-node))))
                          `())

                    (,parent-set! right-node parent-node)
                    (,(update-parent!) parent-node node right-node)

                    (if (,(black? 'rbtree) node)
                        (begin
                          (,(reden!) node) ;; to avoid leaks
                          (fixup! parent-node right-node))))

                   ((eq? right-node rbtree)

                    ;; check if rightmost must be updated

                    ,@(if rightmost
                          `((if (eq? node (,rightmost rbtree))
                                (,rightmost-set!
                                 rbtree
                                 left-node)))
                          `())

                    (,parent-set! left-node parent-node)
                    (,(update-parent!) parent-node node left-node)

                    ;; At this point we know that the node is black.
                    ;; This is because the right child is nil and the
                    ;; left child is red (if the left child was black
                    ;; the tree would not be balanced)

                    (,(reden!) node) ;; to avoid leaks
                    (fixup! parent-node left-node))

                   (else
                    (let loop ((x right-node) (parent-x node))
                      (let ((left-x (,left x)))
                        (if (eq? left-x rbtree)
                            (begin
                              (,(exchange-color!) x node)
                              (,parent-set! left-node x)
                              (,left-set! x left-node)
                              (,parent-set! x parent-node)
                              (,(update-parent!) parent-node node x)
                              (if (eq? x right-node)
                                  (if (,(black? 'rbtree) node)
                                      (begin
                                        (,(reden!) node) ;; to avoid leaks
                                        (fixup! x (,right x))))
                                  (let ((right-x (,right x)))
                                    (,parent-set! right-x parent-x)
                                    (,left-set! parent-x right-x)
                                    (,parent-set! right-node x)
                                    (,right-set! x right-node)
                                    (if (,(black? 'rbtree) node)
                                        (begin
                                          (,(reden!) node) ;; to avoid leaks
                                          (fixup! parent-x right-x))))))
                            (loop left-x x)))))))

           (,parent-set! rbtree rbtree)))

       (define (,reposition! node)
         (let* ((rbtree
                 (,node->rbtree node))
                (predecessor-node
                 (,(neighbor left right) node rbtree))
                (successor-node
                 (,(neighbor right left) node rbtree)))
           (if (or (and (not (eq? predecessor-node rbtree))
                        (,before? node predecessor-node))
                   (and (not (eq? successor-node rbtree))
                        (,before? successor-node node)))
               (begin
                 (,remove! node)
                 (,insert! rbtree node)))))

       (define (,empty? rbtree)
         (eq? rbtree (,left rbtree)))

       (define (,singleton? rbtree)
         (let ((root (,left rbtree)))
           (and (not (eq? root rbtree))
                (eq? (,left root) rbtree)
                (eq? (,right root) rbtree)))))))

