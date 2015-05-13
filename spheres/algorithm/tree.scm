;;!!! K-ary tree algorithms
;; .author Alvaro Castro-Castilla 2010

;;! Node
(define (make-node data children-list)
  (cons data children-list))

(define (node-data node)
  (car node))

(define (node-children tree)
  (cdr tree))

;;! Leaf
(define (make-leaf x) x)

(define (leaf-data l) (car l))

;;! Node predicate
(define (node? obj)
  (and (list? obj)
       (> (length obj) 1)))

;;! Leaf predicate
(define (leaf? obj) (not (node? obj)))

;;! Build a tree taking only up the that depth
(define (take-levels tree level)
  (let recur ((node tree)
              (level level))
    (cond
     ((null? node) '())
     ((leaf? node) node)
     ((zero? level) (make-leaf (node-data node)))
     (else (make-node
            (node-data node)
            (map recur
                 (node-children node)
                 (circular-list (- level 1))))))))

;;! Build a list with a given tree level. Takes an option to deal with shallow
;; leaves
(define* (extract-level tree level (shallow-leaves #f))
  (let/cc
   abort
   (let ((leaf-process (case shallow-leaves
                         ;; Aborts if reaches a leaf shallower than target level
                         ((strict)
                          (lambda (leaf) (abort #f)))
                         ;; If they are leaves, add them even if not in target level
                         ((accept)
                          (lambda (leaf) (list leaf)))
                         ;; Doesn't consider leaves that are not in target level
                         ((remove #f)
                          (lambda (leaf) #f)))))
     ((letrec
          ((recur-down (lambda (node level)
                         (cond
                          ((null? node) '())
                          ((zero? level) (if (leaf? node)
                                             node
                                             (node-data node)))
                          ;; leaf but not in target level
                          ((leaf? node)
                           (leaf-process node))
                          (else
                           (recur-right (node-children node) level)))))
           (recur-right (lambda (nodes level)
                          (if (null? nodes)
                              '()
                              (aif valid-head (recur-down (car nodes) (- level 1))
                                   ;; cons if we're about to reach target level, append otherwise
                                   ((if (<= level 1) cons append)
                                    valid-head
                                    (recur-right (cdr nodes) level))
                                   (recur-right (cdr nodes) level))))))
        recur-down) tree level))))

;;! Build a k-ary with a given tree level, preserving its depth. Takes an option
;; to deal with shallow leaves
(define* (skim-level tree level (shallow-leaves #f))
  (let/cc
   abort
   (let ((leaf-process (case shallow-leaves
                         ;; Aborts if reaches a leaf shallower than target level
                         ((strict)
                          (lambda (leaf) (abort #f)))
                         ;; If they are leaves, add them even if not in target level
                         ((accept)
                          (lambda (leaf) leaf))
                         ;; Doesn't consider leaves that are not in target level
                         ((remove #f)
                          (lambda (leaf) #f)))))
     (let recur ((node tree)
                 (level level))
       (cond
        ((null? node) '())
        ((zero? level) (if (leaf? node)
                           node
                           (node-data node)))
        ((leaf? node) (leaf-process node))
        (else (make-node
               #f
               (map recur
                    (node-children node)
                    (circular-list (- level 1))))))))))

;;! Calculate the depth of the deepest leaf
(define (depth tree)
  ((letrec
       ((recur-down (lambda (node level)
                      (cond
                       ((null? node) '())
                       ((leaf? node) level)
                       (else
                        (recur-right (node-children node) level)))))
        (recur-right (lambda (nodes level)
                       (if (null? nodes)
                           level
                           (max (recur-down (car nodes) (++ level))
                                (recur-right (cdr nodes) level))))))
     recur-down) tree 0))
