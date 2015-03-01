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

(expect* (equal? #t (my-empty? t)))
(expect* (equal? #f (my-singleton? t)))

(my-insert! t n2)

(expect* (equal? #f (my-empty? t)))
(expect* (equal? #t (my-singleton? t)))
(expect* (equal? n2 (my-leftmost t)))

(my-insert! t n1)

(expect* (equal? #f (my-empty? t)))
(expect* (equal? #f (my-singleton? t)))
(expect* (equal? n1 (my-leftmost t)))

(my-insert! t n3)

(expect* (equal? #f (my-empty? t)))
(expect* (equal? #f (my-singleton? t)))
(expect* (equal? n1 (my-leftmost t)))

(my-remove! n1)

(expect* (equal? #f (my-empty? t)))
(expect* (equal? #f (my-singleton? t)))
(expect* (equal? n2 (my-leftmost t)))

(my-remove! n2)

(expect* (equal? #f (my-empty? t)))
(expect* (equal? #t (my-singleton? t)))
(expect* (equal? n3 (my-leftmost t)))

(my-remove! n3)

(expect* (equal? #t (my-empty? t)))
(expect* (equal? #f (my-singleton? t)))

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

(expect* (equal? '(12 11 10 9 8 7 6 5 4 3 2 1)
                 (my-sort '(5 12 8 1 9 10 3 2 7 6 4 11))))

