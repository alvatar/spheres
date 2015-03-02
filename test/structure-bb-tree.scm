(%load-library '(spheres/util test) silent: #t)

(%load-library '(spheres/structure bb-tree) compile: #t)

(test-begin "Balanced Binary Tree")

;; Some testing stuff
(define (make-count from to)
  (if (> from to)
      empty-bb-tree
      (bb-tree-add (make-count (+ from 1) to)
                   from
                   <)))

(define (list->bb-tree list <?)
  (let loop ((list list) (accum empty-bb-tree))
    (if (null? list)
        accum
        (loop (cdr list)
              (bb-tree-add accum
                           (car list)
                           <?)))))

(define (pt bb-tree)
  (let loop ((bb-tree bb-tree))
    (if (eq? empty-bb-tree bb-tree)
        empty-bb-tree
        (let ((elm (bb-tree-element bb-tree))
              (l (bb-tree-left-subbb-tree bb-tree))
              (r (bb-tree-right-subbb-tree bb-tree)))
          (if (and (eq? l empty-bb-tree)
                   (eq? r empty-bb-tree))
              elm
              (list (loop l)
                    elm
                    (loop r)))))))

(define (tp list)
  (if (pair? list)
      (%%make-bb-tree-nonbalancing (cadr list)
                                   (tp (car list))
                                   (tp (caddr list)))
      (if (eq? list empty-bb-tree)
          empty-bb-tree
          (%%make-bb-tree-nonbalancing list empty-bb-tree empty-bb-tree))))

(define (rm bb-tree)
  (list->bb-tree
   (bb-tree->list bb-tree)
   <))

(bb-tree->list (make-count 11 20))
(bb-tree->list (make-count 0 9))
(bb-tree->list
 (%%bb-tree-join 10
                 (make-count 0 10)
                 (make-count 11 12)
                 <))
(bb-tree->list (bb-tree-split> (make-count 1 20) 10 <))
(bb-tree->list (bb-tree-split< (make-count 1 20) 10 <))

(bb-tree->list
 (bb-tree-union (bb-tree-split> (make-count 1 20) 10 <)
                (bb-tree-split< (make-count 1 20) 10 <)
                <))

(bb-tree->list
 (bb-tree-difference (make-count 1 10)
                     (make-count 4 8)
                     <))

(bb-tree->list
 (bb-tree-intersection (make-count 1 5)
                       (make-count 4 8)
                       <))

(bb-tree->list
 (bb-tree-union (make-count 1 8)
                (make-count 12 20)
                <))

(test-end)
