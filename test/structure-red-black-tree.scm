(%load-library '(spheres/util test) silent: #t)

(%load-library '(spheres/structure red-black-tree))

(test-begin "Red-Black Tree")


(define (make-my-rbtree color parent left leftmost)
  (vector color parent left leftmost))

(define (make-my-node color parent left right value)
  (vector color parent left right value))

(define (my-color x)            (vector-ref x 0))
(define (my-color-set! x y)     (vector-set! x 0 y))
(define (my-parent x)           (vector-ref x 1))
(define (my-parent-set! x y)    (vector-set! x 1 y))
(define (my-left x)             (vector-ref x 2))
(define (my-left-set! x y)      (vector-set! x 2 y))
(define (my-right x)            (vector-ref x 3))
(define (my-right-set! x y)     (vector-set! x 3 y))
(define (my-leftmost x)         (vector-ref x 3))
(define (my-leftmost-set! x y)  (vector-set! x 3 y))
(define (my-value x)            (vector-ref x 4))
(define (my-value-set! x y)     (vector-set! x 4 y))

(define (my-before? node1 node2) ;; ordering function
  (< (my-value node1) (my-value node2)))

(define-rbtree
  my-rbtree-init!  ;; defined by define-rbtree
  my-node->rbtree  ;; defined by define-rbtree
  my-insert!       ;; defined by define-rbtree
  my-remove!       ;; defined by define-rbtree
  my-reposition!   ;; defined by define-rbtree
  my-empty?        ;; defined by define-rbtree
  my-singleton?    ;; defined by define-rbtree
  my-before?
  my-color
  my-color-set!
  my-parent
  my-parent-set!
  my-left
  my-left-set!
  my-right
  my-right-set!
  my-leftmost
  my-leftmost-set!
  #f
  #f)

(define (my-rbtree-create)
  (my-rbtree-init! (make-my-rbtree #f #f #f #f)))

(define (my-node-create value)
  (make-my-node #f #f #f #f value))

(define t (my-rbtree-create)) ;; start with an empty tree
(define n1 (my-node-create 1))
(define n2 (my-node-create 2))
(define n3 (my-node-create 3))

(test-assert "Empty (0 elem)" (my-empty? t))

(test-assert "Not singleton (0 elem)" (not (my-singleton? t)))

(my-insert! t n2)

(test-assert "Not empty (1 elem)" (not (my-empty? t)))
(test-assert "Singleton (1 elem)" (my-singleton? t))
(test-equal n2 (my-leftmost t))

(my-insert! t n1)

(test-assert "Not empty (2 elem)" (not (my-empty? t)))
(test-assert "Not singeleton (2 elem)" (not (my-singleton? t)))
(test-equal "Leftmost (2 elem)" n1 (my-leftmost t))

(my-insert! t n3)

(test-assert "Not empty (3 elem)" (not (my-empty? t)))
(test-assert "Not singleton (3 elem)" (not (my-singleton? t)))
(test-equal "Leftmost (3 elem)" n1 (my-leftmost t))

(my-remove! n1)

(test-assert "Not empty (removed elem)" (not (my-empty? t)))
(test-assert "Not singleton (removed elem)" (not (my-singleton? t)))
(test-equal "Leftmost (removed elem)" n2 (my-leftmost t))

(my-remove! n2)

(test-assert "Not empty (removed elem 2)" (not (my-empty? t)))
(test-assert "Singleton (removed elem 2)" (my-singleton? t))
(test-equal "Leftmost (removed elem 2)" n3 (my-leftmost t))

(my-remove! n3)
(test-assert "Empty (removed elem 3)" (my-empty? t))
(test-assert "Not singleton (removed elem 3)" (not (my-singleton? t)))

;; implement a sorting function with a priority queue:

(define (my-sort lst)
  (let ((t (my-rbtree-create)))
    (for-each (lambda (value) (my-insert! t (my-node-create value)))
              lst)
    (let loop ((result '()))
      (if (my-empty? t)
          result
          (let* ((smallest (my-leftmost t))
                 (value (my-value smallest)))
            (my-remove! smallest)
            (loop (cons value result)))))))

(test-equal "Sort implemented with Red-Black Tree"
            (my-sort '(5 12 8 1 9 10 3 2 7 6 4 11))
            '(12 11 10 9 8 7 6 5 4 3 2 1))

(test-end)
