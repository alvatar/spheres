;;; skiplists/run.scm

(require-library skiplists)
(import skiplists)


;;; (run xpr0 xpr1 ...)
;;; -------------------
(define (run . xprs)
  (let loop ((xprs xprs))
    (if (null? xprs)
      (print "All tests passed!")
      (if (car xprs)
        (loop (cdr xprs))
        (error 'run "#### Some test failed! ####")))))

;;; build a list of length n of random numbers between 0 and n
(define (random-list n)
  (let loop ((acc '()) (k n))
    (if (zero? k)
      acc
      (loop (cons (random n) acc) (- k 1)))))

;;; build the list of cardinals less than n
(define (enum n)
  (let loop ((acc '()) (k (- n 1)))
    (if (negative? k)
      acc
      (loop (cons k acc) (- k 1)))))

(define lst (random-list 60))
(define dlst (map (lambda (x) (list x (car (random-list 5)))) lst))

(define ord (make-skiplist-with-gap-from-list (enum 60) 5 3 -))
(define skp (make-skiplist-from-list lst 5 -)) ; without dups
(define skp* (make-skiplist-from-list lst 5 - dups)) ; with dups
(define skp12 (make-skiplist-from-list dlst 5
                                      (lambda (x y) (- (car x) (car y)))
                                      (lambda (x y) (- (cadr x) (cadr y)))))
(define skp21 (apply skip-reorder skp12 (reverse (skip-orders skp12))))
(define flt (skip-filter skp* even?))

(run
  (= (skip-count skp*) (length lst))
  (apply <= (skip-list skp*))
  (<= (skip-count flt) (length lst))
  (not (memq #f (map even? (skip-list flt))))
  (<= (skip-count skp) (length lst))
  (apply < (skip-list skp))
  (= (skip-count skp12) (length lst) (length dlst))
  (= (skip-count skp21) (length lst) (length dlst))
  (apply <= (map car (skip-list skp12)))
  (apply <= (map cadr (skip-list skp21)))
  (equal? (skip-list skp*)
          (skip-list (skip-restructure skp* 7 3)))
  (equal? (skip-list skp12)
          (skip-list (skip-restructure skp12 7 3)))
  (equal? (skip-list ord) (enum 60))
  (begin (skip-insert! skp* -1 -1 -1)
         ;(skip-insert! skp* -1)
         ;(skip-insert! skp* -1)
    (= (skip-count skp*) (+ 3 (length lst))))
  (memv -1 (skip-list skp*))
  (begin (skip-remove-all! skp* -1)
         (= (skip-count skp*) (length lst)))
  (not (memv -1 (skip-list skp*)))
  (let ((len (skip-count skp)))
    (skip-insert! skp -1)
    (skip-insert! skp -1)
    (skip-insert! skp -1)
    (skip-remove! skp -1)
    (= len (skip-count skp)))
  (begin
    (apply skip-insert! skp (enum 60))
    (equal? (skip-list skp) (enum 60)))
  )



;;;; Testing, Diagnostic, and General Utilities

;;; This is no excuse for a proper test suite.

(define (test-skip-list count)
  (let ((skip-list (make-skip-list skip-list-type:string)))
    (do ((i 0 (+ i 1)))
        ((>= i count))
      (let ((s (number->string i)))
        (skip-list/insert! skip-list s i)
        (if (not (= i (skip-list/lookup skip-list s -1)))
            (error "Intermediate lossage in insertion:" i))))
    (do ((i 0 (+ i 2)))
        ((>= i count))
      (let ((s (number->string i)))
        (skip-list/delete! skip-list s)
        (if (skip-list/lookup skip-list s #f)
            (error "Intermediate lossage in deletion:" i))))
    (do ((i 0 (+ i 1)))
        ((>= i count))
      (let ((s (number->string i)))
        (if (if (even? i)
                (skip-list/lookup skip-list s #f)
                (not (= i (skip-list/lookup skip-list s -1))))
            (error "Post-lossage:" i)))))
  (let ((alist
         (do ((i 0 (+ i 1))
              (alist '() (cons (cons i i) alist)))
             ((>= i count) (reverse alist)))))
    (let ((skip-list (alist->skip-list alist skip-list-type:exact-integer)))
      (if (not (equal? alist (skip-list->alist skip-list)))
          (error "Alist lossage:" skip-list)))
    (do ((i 0 (+ i 1)))
        ((>= i count))
      (let ((skip-list (alist->skip-list alist skip-list-type:exact-integer)))
        (receive (lesser greater) (skip-list/split<! skip-list i)
          (if (not (= count (+ (skip-list/count lesser)
                               (skip-list/count greater))))
              (error "Count mismatch:"
                     count
                     `(+ ,(skip-list/count lesser)
                         ,(skip-list/count greater))))
          (let ((skip-list*
                 (skip-list/union-merge! lesser greater
                   (lambda (key lesser-datum greater-datum)
                     (error "Merge:" key lesser-datum greater-datum)))))
            (if (not (equal? alist (skip-list->alist skip-list*)))
                (error "Split/union lossage:" skip-list*))))))))

(define (skip-list/horizontals skip-list)
  (let loop ((level (bottom-level)) (horizontals '()))
    (if (= level (skip-list.level skip-list))
        horizontals
        (loop
         (+ level 1)
         (cons (let loop ((node (skip-list.header skip-list)) (alist '()))
                 (let ((node (node-forward node level)))
                   (if (node? node)
                       (loop node
                             (cons (cons (node-key node) (node-datum node))
                                   alist))
                       (reverse alist))))
               horizontals)))))

(define (flip-coin)
  (zero? (random-integer 2)))
