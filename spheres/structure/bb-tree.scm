;;!!! A functional balanced tree data structure, with a rather low level
;; interface. It is intended to be used as a base to implement data
;; structures like maps, sets and priority queues. It can obviously
;; also be used to implement sorting, removal of duplicate elements in
;; lists and things like that. The implementation is based on the
;; algorithms described in
;; http://groups.csail.mit.edu/mac/users/adams/BB/
;;
;; .author Per Eckerdal, 2010
;; .author Alvaro Castro-Castilla, 2015

;; A function whose name begins with %% is unsafe; it doesn't check
;; its arguments. It might or might not segfault, but other functions
;; might do it later on since the data structure can become bogus
;; unless you give it proper arguments. They are not exported, so
;; those issues are taken care of internally.
;;
;; Copyright (c) 2010 Per Eckerdal
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


(cond-expand
 (gambit (declare (fixnum)))
 (else (void)))


(define *bb-tree-weight* 5)

(define (check-bb-tree . params)
  (if (let loop ((lst params))
        (if (null? lst)
            #f
            (or (not (bb-tree? (car lst)))
                (loop (cdr lst)))))
      (error "Invalid parameters" params)))

(define-type bb-tree
  constructor: make-bb-tree/internal
  predicate: bb-tree/internal?
  (element read-only: unprintable:)
  (count read-only: unprintable:)
  (left-subbb-tree read-only: unprintable:)
  (right-subbb-tree read-only: unprintable:))

(define (bb-tree? x)
  (or (eq? x empty-bb-tree)
      (bb-tree/internal? x)))

(define empty-bb-tree (list 'empty-bb-tree))

(define-macro (%%bb-tree-element t)
  `(##vector-ref ,t 1))

(define-macro (%%bb-tree-count t)
  `(##vector-ref ,t 2))

(define-macro (%%bb-tree-left-subbb-tree t)
  `(##vector-ref ,t 3))

(define-macro (%%bb-tree-right-subbb-tree t)
  `(##vector-ref ,t 4))

(define-macro (%%bb-tree-size t)
  (let ((gs (gensym)))
    `(let ((,gs ,t))
       (if (eq? ,gs empty-bb-tree)
           0
           (%%bb-tree-count ,gs)))))

(define-macro (%%< a b)
  `(##< ,a ,b))

(define-macro (%%> a b)
  `(##< ,b ,a))

;; The constructors for the bb-tree datatype are in different layers of
;; abstraction:
;;
;; make-bb-tree/internal is the most low-level bb-tree constructor
;;
;; make-bb-tree-nonbalancing keeps track of the count field
;;
;; make-bb-tree is used when the original bb-tree was in balance and one of
;; l or r have changed size by at most one element, as in insertion or
;; deletion of a single element.
;;
;; %%bb-tree-join is used for joining an element and two bb-trees of
;; arbitrary sizes, where the every element of the left bb-tree is < the
;; element and every element of the right bb-tree is > the element.

(define (%%make-bb-tree-nonbalancing elm left right)
  (make-bb-tree/internal elm
                         (+ 1
                            (%%bb-tree-size left)
                            (%%bb-tree-size right))
                         left
                         right))

(define (%%bb-tree-rotate-single-left parent-elm left right)
  (%%make-bb-tree-nonbalancing
   (%%bb-tree-element right)
   (%%make-bb-tree-nonbalancing parent-elm
                                left
                                (%%bb-tree-left-subbb-tree right))
   (%%bb-tree-right-subbb-tree right)))

(define (%%bb-tree-rotate-single-right parent-elm left right)
  (%%make-bb-tree-nonbalancing
   (%%bb-tree-element left)
   (%%bb-tree-left-subbb-tree left)
   (%%make-bb-tree-nonbalancing parent-elm
                                (%%bb-tree-right-subbb-tree left)
                                right)))

(define (%%bb-tree-rotate-double-left parent-elm left right)
  (let ((right-left (%%bb-tree-left-subbb-tree right)))
    (%%make-bb-tree-nonbalancing
     (%%bb-tree-element right-left)
     (%%make-bb-tree-nonbalancing
      parent-elm
      left
      (%%bb-tree-left-subbb-tree right-left))
     (%%make-bb-tree-nonbalancing
      (%%bb-tree-element right)
      (%%bb-tree-right-subbb-tree right-left)
      (%%bb-tree-right-subbb-tree right)))))

(define (%%bb-tree-rotate-double-right parent-elm left right)
  (let ((left-right (%%bb-tree-right-subbb-tree left)))
    (%%make-bb-tree-nonbalancing
     (%%bb-tree-element left-right)
     (%%make-bb-tree-nonbalancing
      (%%bb-tree-element left)
      (%%bb-tree-left-subbb-tree left)
      (%%bb-tree-left-subbb-tree left-right))
     (%%make-bb-tree-nonbalancing
      parent-elm
      (%%bb-tree-right-subbb-tree left-right)
      right))))

(define (%%make-bb-tree elm left right)
  (let ((left-size (%%bb-tree-size left))
        (right-size (%%bb-tree-size right)))
    (cond
     ((%%< (+ left-size right-size)
           2)
      ;; The bb-tree is too small to be balanced
      (%%make-bb-tree-nonbalancing elm left right))

     ((%%> right-size
           (* *bb-tree-weight* left-size))
      ;; Right side is too heavy
      (let ((right-left-size (%%bb-tree-size
                              (%%bb-tree-left-subbb-tree right)))
            (right-right-size (%%bb-tree-size
                               (%%bb-tree-right-subbb-tree right))))
        (if (%%< right-left-size
                 right-right-size)
            (%%bb-tree-rotate-single-left elm left right)
            (%%bb-tree-rotate-double-left elm left right))))

     ((%%> left-size
           (* *bb-tree-weight* right-size))
      ;; Left side is too heavy
      (let ((left-right-size (%%bb-tree-size
                              (%%bb-tree-right-subbb-tree left)))
            (left-left-size (%%bb-tree-size
                             (%%bb-tree-left-subbb-tree left))))
        (if (%%< left-right-size
                 left-left-size)
            (%%bb-tree-rotate-single-right elm left right)
            (%%bb-tree-rotate-double-right elm left right))))

     (else
      ;; The bb-tree doesn't need to be balanced
      (%%make-bb-tree-nonbalancing elm left right)))))

;; The function %%bb-tree-join is used to join two bb-trees with an element
;; that is between the values in the left bb-tree and the values in the
;; right bb-tree. If the left and right arguments would make a balanced
;; bb-tree then they can be joined immediately. If one bb-tree is
;; significantly larger then it is scanned to find the largest subbb-tree
;; on the side 'facing' the smaller bb-tree that is small enough to
;; balance with the smaller bb-tree. The bb-tree is joined at this position
;; and the higher levels are rebalanced if necessary.
(define (%%bb-tree-join elm left right <?)
  (let loop ((elm elm)
             (left left)
             (right right))
    (cond
     ((eq? left empty-bb-tree) (%%bb-tree-add right elm <?))
     ((eq? right empty-bb-tree) (%%bb-tree-add left elm <?))
     (else
      (let ((left-elm (%%bb-tree-element left))
            (left-size (%%bb-tree-count left))
            (left-left (%%bb-tree-left-subbb-tree left))
            (left-right (%%bb-tree-right-subbb-tree left))

            (right-elm (%%bb-tree-element right))
            (right-size (%%bb-tree-count right))
            (right-left (%%bb-tree-left-subbb-tree right))
            (right-right (%%bb-tree-right-subbb-tree right)))
        (cond
         ((%%> right-size
               (* *bb-tree-weight* left-size))
          ;; Right side is too heavy
          (%%make-bb-tree right-elm
                          (loop elm left right-left)
                          right-right))

         ((%%> left-size
               (* *bb-tree-weight* right-size))
          ;; Left side is too heavy
          (%%make-bb-tree left-elm
                          left-left
                          (loop elm left-right right)))

         (else
          ;; Bb-Tree doesn't need to be balanced
          (%%make-bb-tree-nonbalancing elm left right))))))))

;; Concatenates two bb-trees. Every element in left should be < every
;; element in right.
(define (%%bb-tree-concat left right <?)
  (let loop ((left left)
             (right right))
    (cond
     ((eq? left empty-bb-tree)
      right)

     ((eq? right empty-bb-tree)
      left)

     (else
      (let ((left-elm (%%bb-tree-element left))
            (left-size (%%bb-tree-count left))
            (left-left (%%bb-tree-left-subbb-tree left))
            (left-right (%%bb-tree-right-subbb-tree left))

            (right-elm (%%bb-tree-element right))
            (right-size (%%bb-tree-count right))
            (right-left (%%bb-tree-left-subbb-tree right))
            (right-right (%%bb-tree-right-subbb-tree right)))
        (cond
         ((%%> right-size
               (* *bb-tree-weight* left-size))
          ;; Right side is too heavy
          (%%make-bb-tree right-elm
                          (loop left
                                right-left)
                          right-right))

         ((%%> left-size
               (* *bb-tree-weight* right-size))
          ;; Left side is too heavy
          (%%make-bb-tree left-elm
                          left-left
                          (loop left-right
                                right)))

         (else
          ;; Bb-Tree doesn't need to be balanced
          (%%make-bb-tree (bb-tree-min right)
                          left
                          (bb-tree-delete-min right)))))))))

;; The already-there function can possibly throw an error (or not
;; return in another way), and then no bb-trees will be allocated. If
;; already-there is not supplied, the bb-tree with that element is
;; replaced.
(define (%%bb-tree-add bb-tree elm <? #!key (already-there (lambda () #f)))
  (let loop ((bb-tree bb-tree))
    (if (eq? bb-tree empty-bb-tree)
        (make-bb-tree/internal elm 1 empty-bb-tree empty-bb-tree)
        (let ((bb-tree-elm (%%bb-tree-element bb-tree))
              (bb-tree-left (%%bb-tree-left-subbb-tree bb-tree))
              (bb-tree-right (%%bb-tree-right-subbb-tree bb-tree)))
          (cond
           ((<? elm bb-tree-elm)
            (%%make-bb-tree bb-tree-elm
                            (loop bb-tree-left)
                            bb-tree-right))

           ((<? bb-tree-elm elm)
            (%%make-bb-tree bb-tree-elm
                            bb-tree-left
                            (loop bb-tree-right)))

           (else
            (already-there)
            (%%make-bb-tree elm
                            bb-tree-left
                            bb-tree-right)))))))

(define (bb-tree-add bb-tree elm <? #!key (already-there (lambda () #f)))
  (check-bb-tree bb-tree)
  (%%bb-tree-add bb-tree elm <? already-there: already-there))

(define (bb-tree-delete-min bb-tree)
  (check-bb-tree bb-tree)
  (if (eq? bb-tree empty-bb-tree)
      (error "Can't delete empty bb-tree"))

  (let loop ((bb-tree bb-tree))
    (let ((left (%%bb-tree-left-subbb-tree bb-tree))
          (right (%%bb-tree-right-subbb-tree bb-tree)))
      (if (eq? empty-bb-tree left)
          right
          (%%make-bb-tree (%%bb-tree-element bb-tree)
                          (bb-tree-delete-min left)
                          right)))))

(define (bb-tree-delete-max bb-tree)
  (check-bb-tree bb-tree)
  (if (eq? bb-tree empty-bb-tree)
      (error "Can't delete empty bb-tree"))

  (let loop ((bb-tree bb-tree))
    (let ((left (%%bb-tree-left-subbb-tree bb-tree))
          (right (%%bb-tree-right-subbb-tree bb-tree)))
      (if (eq? right empty-bb-tree)
          left
          (%%make-bb-tree (%%bb-tree-element bb-tree)
                          left
                          (bb-tree-delete-max right))))))

;; This is a utility function for bb-tree-delete
;;
;; left should be (bb-tree-left-subbb-tree parent)
;; right should be (bb-tree-right-subbb-tree parent)
(define (bb-tree-delete-root parent left right <?)
  (cond
   ((eq? right empty-bb-tree)
    left)

   ((eq? left empty-bb-tree)
    right)

   (else
    (let ((min-elm (bb-tree-min right)))
      (%%make-bb-tree min-elm
                      left
                      (bb-tree-delete-min right))))))

(define (bb-tree-delete bb-tree elm <?
                        #!key (not-found
                               (lambda ()
                                 (error "Not found"))))
  (check-bb-tree bb-tree)
  (let loop ((bb-tree bb-tree))
    (if (eq? bb-tree empty-bb-tree)
        (not-found)
        (let ((bb-tree-elm (%%bb-tree-element bb-tree))
              (left (%%bb-tree-left-subbb-tree bb-tree))
              (right (%%bb-tree-right-subbb-tree bb-tree)))
          (cond
           ((<? elm bb-tree-elm)
            (%%make-bb-tree bb-tree-elm
                            (loop left)
                            right))

           ((<? bb-tree-elm elm)
            (%%make-bb-tree bb-tree-elm
                            left
                            (loop right)))

           (else
            (bb-tree-delete-root bb-tree left right <?)))))))

(define (bb-tree-size bb-tree)
  (if (eq? bb-tree empty-bb-tree)
      0
      (bb-tree-count bb-tree)))

(define (bb-tree-search bb-tree elm <? fail found)
  (check-bb-tree bb-tree)

  (let loop ((bb-tree bb-tree))
    (if (eq? bb-tree empty-bb-tree)
        (fail)
        (let ((bb-tree-elm (%%bb-tree-element bb-tree)))
          (cond
           ((<? elm bb-tree-elm)
            (loop (%%bb-tree-left-subbb-tree bb-tree)))
           ((<? bb-tree-elm elm)
            (loop (%%bb-tree-right-subbb-tree bb-tree)))
           (else
            (found bb-tree-elm)))))))

(define (bb-tree-member? bb-tree elm <?)
  (bb-tree-search bb-tree
                  elm
                  <?
                  (lambda () #f)
                  (lambda (bb-tree) #t)))

(define-macro (define-bb-tree-search-min/max name fn)
  `(define (,name bb-tree fail found)
     (check-bb-tree bb-tree)
     (if (eq? bb-tree empty-bb-tree)
         (fail)
         (let loop ((bb-tree bb-tree))
           (let ((subbb-tree (,fn bb-tree)))
             (if (eq? subbb-tree empty-bb-tree)
                 (found (%%bb-tree-element bb-tree))
                 (loop subbb-tree)))))))

(define-bb-tree-search-min/max bb-tree-search-min %%bb-tree-left-subbb-tree)
(define-bb-tree-search-min/max bb-tree-search-max %%bb-tree-right-subbb-tree)

(define (bb-tree-min bb-tree)
  (bb-tree-search-min
   bb-tree
   (lambda () (error "Bb-Tree doesn't have a minimum value"))
   (lambda (value) value)))

(define (bb-tree-max bb-tree)
  (bb-tree-search-max
   bb-tree
   (lambda () (error "Bb-Tree doesn't have a maximum value"))
   (lambda (value) value)))

(define (bb-tree-fold fn base bb-tree)
  (check-bb-tree bb-tree)

  (let loop ((base base)
             (bb-tree bb-tree))
    (if (eq? bb-tree empty-bb-tree)
        base
        (let ((elm (%%bb-tree-element bb-tree))
              (left (%%bb-tree-left-subbb-tree bb-tree))
              (right (%%bb-tree-right-subbb-tree bb-tree)))
          (loop (fn elm
                    (loop base right))
                left)))))

(define (bb-tree-fold-from bb-tree elm <? base fn)
  (check-bb-tree bb-tree)

  (let loop ((base base)
             (bb-tree bb-tree)
             (k (lambda (x) x)))
    (if (eq? bb-tree empty-bb-tree)
        (k base)
        (let ((bb-tree-elm (%%bb-tree-element bb-tree))
              (left (%%bb-tree-left-subbb-tree bb-tree))
              (right (%%bb-tree-right-subbb-tree bb-tree)))
          (cond
           ((<? elm bb-tree-elm)
            (loop base
                  left
                  (lambda (new-base)
                    (fn bb-tree-elm
                        new-base
                        (lambda (res)
                          (loop res
                                right
                                k))))))

           ((<? bb-tree-elm elm)
            (loop base right k))

           (else
            (fn bb-tree-elm
                base
                (lambda (res)
                  (loop res
                        right
                        k)))))))))

(define (bb-tree-backwards-fold-from bb-tree elm <? base fn)
  (check-bb-tree bb-tree)

  (let loop ((base base)
             (bb-tree bb-tree)
             (k (lambda (x) x)))
    (if (eq? bb-tree empty-bb-tree)
        (k base)
        (let ((bb-tree-elm (%%bb-tree-element bb-tree))
              (left (%%bb-tree-left-subbb-tree bb-tree))
              (right (%%bb-tree-right-subbb-tree bb-tree)))
          (cond
           ((<? bb-tree-elm elm)
            (loop base
                  right
                  (lambda (new-base)
                    (fn bb-tree-elm
                        new-base
                        (lambda (res)
                          (loop res
                                left
                                k))))))

           ((<? elm bb-tree-elm)
            (loop base left k))

           (else
            (fn bb-tree-elm
                base
                (lambda (res)
                  (loop res
                        left
                        k)))))))))

(define (bb-tree-split< bb-tree elm <?)
  (check-bb-tree bb-tree)
  (let loop ((bb-tree bb-tree))
    (if (eq? bb-tree empty-bb-tree)
        empty-bb-tree
        (let ((bb-tree-elm (%%bb-tree-element bb-tree))
              (bb-tree-left (%%bb-tree-left-subbb-tree bb-tree))
              (bb-tree-right (%%bb-tree-right-subbb-tree bb-tree)))
          (cond
           ((<? elm bb-tree-elm)
            (loop bb-tree-left))
           ((<? bb-tree-elm elm)
            (%%bb-tree-join bb-tree-elm
                            bb-tree-left
                            (loop bb-tree-right)
                            <?))
           (else
            bb-tree-left))))))


(define (bb-tree-split> bb-tree elm <?)
  (check-bb-tree bb-tree)
  (let loop ((bb-tree bb-tree))
    (if (eq? bb-tree empty-bb-tree)
        empty-bb-tree
        (let ((bb-tree-elm (%%bb-tree-element bb-tree))
              (bb-tree-left (%%bb-tree-left-subbb-tree bb-tree))
              (bb-tree-right (%%bb-tree-right-subbb-tree bb-tree)))
          (cond
           ((<? elm bb-tree-elm)
            (%%bb-tree-join bb-tree-elm
                            (loop bb-tree-left)
                            bb-tree-right
                            <?))
           ((<? bb-tree-elm elm)
            (loop bb-tree-right))
           (else
            bb-tree-right))))))

;; This function is here for testing purposes. The bb-tree-union function
;; is faster but more complex (and thus more bug-prone).
(define (bb-tree-union-slow bb-tree1 bb-tree2 <?)
  (check-bb-tree bb-tree1 bb-tree2)
  (let loop ((bb-tree1 bb-tree1)
             (bb-tree2 bb-tree2))
    (cond
     ((eq? bb-tree1 empty-bb-tree)
      bb-tree2)

     ((eq? bb-tree2 empty-bb-tree)
      bb-tree1)

     (else
      (let* ((bb-tree2-elm (%%bb-tree-element bb-tree2))
             (bb-tree2-left (%%bb-tree-left-subbb-tree bb-tree2))
             (bb-tree2-right (%%bb-tree-right-subbb-tree bb-tree2))

             (bb-tree1<elm (bb-tree-split< bb-tree1 bb-tree2-elm <?))
             (bb-tree1>elm (bb-tree-split> bb-tree1 bb-tree2-elm <?)))
        (%%bb-tree-join bb-tree2-elm
                        (loop bb-tree1<elm
                              bb-tree2-left)
                        (loop bb-tree1>elm
                              bb-tree2-right)
                        <?))))))

(define (bb-tree-union bb-tree1 bb-tree2 <?)
  (define (trim lo hi bb-tree)
    (if (eq? bb-tree empty-bb-tree)
        empty-bb-tree
        (let ((bb-tree-elm (%%bb-tree-element bb-tree))
              (bb-tree-left (%%bb-tree-left-subbb-tree bb-tree))
              (bb-tree-right (%%bb-tree-right-subbb-tree bb-tree)))
          (if (<? lo bb-tree-elm)
              (if (<? bb-tree-elm hi)
                  bb-tree
                  (trim lo hi bb-tree-left))
              (trim lo hi bb-tree-right)))))

  (define (uni-bd bb-tree1 bb-tree2 lo hi)
    (cond
     ((eq? bb-tree2 empty-bb-tree)
      bb-tree1)

     ((eq? bb-tree1 empty-bb-tree)
      (let ((bb-tree-elm (%%bb-tree-element bb-tree2))
            (bb-tree-left (%%bb-tree-left-subbb-tree bb-tree2))
            (bb-tree-right (%%bb-tree-right-subbb-tree bb-tree2)))
        (%%bb-tree-join bb-tree-elm
                        (bb-tree-split> bb-tree-left lo <?)
                        (bb-tree-split< bb-tree-right hi <?)
                        <?)))

     (else
      (let ((bb-tree1-elm (%%bb-tree-element bb-tree1))
            (bb-tree1-left (%%bb-tree-left-subbb-tree bb-tree1))
            (bb-tree1-right (%%bb-tree-right-subbb-tree bb-tree1)))
        ;; Invariant lo < bb-tree1-elm < hi
        (%%bb-tree-join bb-tree1-elm
                        (uni-bd bb-tree1-left
                                (trim lo bb-tree1-elm bb-tree2)
                                lo
                                bb-tree1-elm)
                        (uni-bd bb-tree1-right
                                (trim bb-tree1-elm hi bb-tree2)
                                bb-tree1-elm
                                hi)
                        <?)))))

  ;; All the other versions of uni and trim are specializations of the
  ;; above two functions with lo=-infinity and/or hi=+infinity

  (define (trim-lo lo bb-tree)
    (if (eq? bb-tree empty-bb-tree)
        empty-bb-tree
        (if (<? lo (%%bb-tree-element bb-tree))
            bb-tree
            (trim-lo lo (%%bb-tree-right-subbb-tree bb-tree)))))
  (define (trim-hi hi bb-tree)
    (if (eq? bb-tree empty-bb-tree)
        empty-bb-tree
        (if (<? (%%bb-tree-element bb-tree) hi)
            bb-tree
            (trim-hi hi (%%bb-tree-left-subbb-tree bb-tree)))))

  (define (uni-hi bb-tree1 bb-tree2 hi)
    (cond
     ((eq? bb-tree2 empty-bb-tree)
      bb-tree1)

     ((eq? bb-tree1 empty-bb-tree)
      (%%bb-tree-join (%%bb-tree-element bb-tree2)
                      (%%bb-tree-left-subbb-tree bb-tree2)
                      (bb-tree-split< (%%bb-tree-right-subbb-tree bb-tree2)
                                      hi
                                      <?)
                      <?))

     (else
      (let ((bb-tree1-elm (%%bb-tree-element bb-tree1))
            (bb-tree1-left (%%bb-tree-left-subbb-tree bb-tree1))
            (bb-tree1-right (%%bb-tree-right-subbb-tree bb-tree1)))
        (%%bb-tree-join bb-tree1-elm
                        (uni-hi bb-tree1-left
                                (trim-hi bb-tree1-elm bb-tree2)
                                bb-tree1-elm)
                        (uni-bd bb-tree1-right
                                (trim bb-tree1-elm hi bb-tree2)
                                bb-tree1-elm
                                hi)
                        <?)))))
  (define (uni-lo bb-tree1 bb-tree2 lo)
    (cond
     ((eq? bb-tree2 empty-bb-tree)
      bb-tree1)

     ((eq? bb-tree1 empty-bb-tree)
      (%%bb-tree-join (%%bb-tree-element bb-tree2)
                      (bb-tree-split> (%%bb-tree-left-subbb-tree bb-tree2)
                                      lo
                                      <?)
                      (%%bb-tree-right-subbb-tree bb-tree2)
                      <?))

     (else
      (let ((bb-tree1-elm (%%bb-tree-element bb-tree1))
            (bb-tree1-left (%%bb-tree-left-subbb-tree bb-tree1))
            (bb-tree1-right (%%bb-tree-right-subbb-tree bb-tree1)))
        (%%bb-tree-join bb-tree1-elm
                        (uni-bd bb-tree1-left
                                (trim lo bb-tree1-elm bb-tree2)
                                lo
                                bb-tree1-elm)
                        (uni-lo bb-tree1-right
                                (trim-lo bb-tree1-elm bb-tree2)
                                bb-tree1-elm)
                        <?)))))

  (check-bb-tree bb-tree1 bb-tree2)
  (cond
   ((eq? bb-tree1 empty-bb-tree)
    bb-tree2)

   ((eq? bb-tree2 empty-bb-tree)
    bb-tree1)

   (else
    (let ((bb-tree1-elm (%%bb-tree-element bb-tree1))
          (bb-tree1-left (%%bb-tree-left-subbb-tree bb-tree1))
          (bb-tree1-right (%%bb-tree-right-subbb-tree bb-tree1)))
      (%%bb-tree-join bb-tree1-elm
                      (uni-hi bb-tree1-left
                              (trim-hi bb-tree1-elm bb-tree2)
                              bb-tree1-elm)
                      (uni-lo bb-tree1-right
                              (trim-lo bb-tree1-elm bb-tree2)
                              bb-tree1-elm)
                      <?)))))

(define (bb-tree-difference bb-tree1 bb-tree2 <?)
  (check-bb-tree bb-tree1 bb-tree2)
  (let loop ((bb-tree1 bb-tree1) (bb-tree2 bb-tree2))
    (cond
     ((eq? bb-tree1 empty-bb-tree)
      empty-bb-tree)

     ((eq? bb-tree2 empty-bb-tree)
      bb-tree1)

     (else
      (let ((bb-tree2-elm (%%bb-tree-element bb-tree2))
            (bb-tree2-left (%%bb-tree-left-subbb-tree bb-tree2))
            (bb-tree2-right (%%bb-tree-right-subbb-tree bb-tree2)))
        (%%bb-tree-concat (loop (bb-tree-split< bb-tree1 bb-tree2-elm <?)
                                bb-tree2-left)
                          (loop (bb-tree-split> bb-tree1 bb-tree2-elm <?)
                                bb-tree2-right)
                          <?))))))

(define (bb-tree-intersection bb-tree1 bb-tree2 <?)
  (check-bb-tree bb-tree1 bb-tree2)
  (let loop ((bb-tree1 bb-tree1) (bb-tree2 bb-tree2))
    (if (or (eq? bb-tree1 empty-bb-tree)
            (eq? bb-tree2 empty-bb-tree))
        empty-bb-tree
        (let* ((bb-tree2-elm (%%bb-tree-element bb-tree2))
               (bb-tree2-left (%%bb-tree-left-subbb-tree bb-tree2))
               (bb-tree2-right (%%bb-tree-right-subbb-tree bb-tree2))

               (bb-tree1<bb-tree2-elm (bb-tree-split< bb-tree1 bb-tree2-elm <?))
               (bb-tree1>bb-tree2-elm (bb-tree-split> bb-tree1 bb-tree2-elm <?)))
          (if (bb-tree-member? bb-tree1 bb-tree2-elm <?)
              (%%bb-tree-join bb-tree2-elm
                              (loop bb-tree1<bb-tree2-elm
                                    bb-tree2-left)
                              (loop bb-tree1>bb-tree2-elm
                                    bb-tree2-right)
                              <?)
              (%%bb-tree-concat (loop bb-tree1<bb-tree2-elm
                                      bb-tree2-left)
                                (loop bb-tree1>bb-tree2-elm
                                      bb-tree2-right)
                                <?))))))

(define (bb-tree-rank bb-tree elm <?
                      #!key
                      (fail
                       (lambda ()
                         (error "Bb-Tree doesn't contain" elm))))
  (check-bb-tree bb-tree)

  (let loop ((bb-tree bb-tree) (accum 0))
    (if (eq? bb-tree empty-bb-tree)
        (fail)
        (let ((bb-tree-elm (%%bb-tree-element bb-tree))
              (bb-tree-left (%%bb-tree-left-subbb-tree bb-tree))
              (bb-tree-right (%%bb-tree-right-subbb-tree bb-tree)))
          (cond
           ((<? elm bb-tree-elm)
            (loop bb-tree-left accum))
           ((<? bb-tree-elm elm)
            (loop bb-tree-right
                  (+ accum
                     1
                     (%%bb-tree-size bb-tree-left))))
           (else
            (+ accum
               (%%bb-tree-size bb-tree-left))))))))

(define (bb-tree-index bb-tree idx
                       #!key
                       (fail
                        (lambda ()
                          (error "Bb-Tree has no element with index" idx))))
  (check-bb-tree bb-tree)
  (if (not (fixnum? idx))
      (error "Invalid parameter" idx))

  (let loop ((bb-tree bb-tree) (idx idx))
    (if (eq? bb-tree empty-bb-tree)
        (fail)
        (let* ((bb-tree-left (%%bb-tree-left-subbb-tree bb-tree))
               (left-size (%%bb-tree-size bb-tree-left)))
          (cond
           ((%%< idx left-size)
            (loop bb-tree-left idx))
           ((%%> idx left-size)
            (loop (%%bb-tree-right-subbb-tree bb-tree)
                  (- idx left-size 1)))
           (else
            (%%bb-tree-element bb-tree)))))))

(define (list->bb-tree list <?)
  (let loop ((list list) (accum empty-bb-tree))
    (if (null? list)
        accum
        (loop (cdr list)
              (bb-tree-add accum
                           (car list)
                           <?)))))

(define (bb-tree->list bb-tree)
  (bb-tree-fold cons '() bb-tree))


