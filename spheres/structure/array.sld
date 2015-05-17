;;!!! Functional arrays are like vectors, insofar as they are mutable and
;; allow fast access to items stored at a particular position. Fast
;; here means O(log n).
;; Contrary to vectors functional arrays are unbounded, they can expand
;; and shrink as needed. Adding and removing at the end, i.e. pruning,
;; is cheap.  Moreover, arrays can be typed: adding and updating items
;; works only, if the item passes an item? predicate supplied with the
;; constructor.
;; .author Juergen Lorenz, 2014
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure array)
  (export array?
          array-null?
          make-array
          array
          list->array
          vector->array
          array-repeat
          array-iterate
          array-iterate-while
          array-iterate-until
          array-copy
          array->list
          array->vector
          array-cursor-start!
          array-cursor-next!
          array-cursor-item
          array-cursor-index
          array-cursor-finished?
          array-cursor-goto!
          array-member
          array-memp
          array-memq
          array-memv
          array-first
          array-rest
          array-last
          array-butlast
          array-add!
          array-update!
          array-prune!
          array-apply
          array-reverse
          array-reverse!
          array-swap!
          array-length
          array-range
          array-item
          array-split-at
          array-split-with
          array-drop
          array-drop-while
          array-take
          array-take-while
          array-append
          array-append!
          array-map
          array-mappend
          array-handler
          array-for-each
          array-filter
          array-equ?
          array-equal?
          array-eqv?
          array-eq?
          array-remp
          array-remove
          array-remq
          array-remv
          array-remove-dups
          array-fold-left
          array-fold-right
          array-sorted?
          array-sort!
          array-zip
          array-zip
          array-unzip
          array-interpose
          array-every?
          array-some?
          array-in?
          array-bind
          set?
          set-handler
          set-equ?
          set-item?
          set
          list->set
          vector->set
          make-set
          set-iterate
          set-iterate-while
          set-iterate-until
          set-map
          set->list
          set->vector
          set-in
          set-count
          set-filter
          set-null?
          set-for-each
          set-copy
          set-difference
          set-add!
          set-remove!
          set=
          set>=
          set<=
          set-union
          set-intersection
          set-every?
          set-some?
          set-apply
          array-handlers
          array-handler?
          make-array-handler
          array-handler-repeat
          array-handler-iterate
          array-handler-iterate-while
          array-handler-iterate-until
          nary
          nary?)

  (import (spheres/core base)
          (spheres/core functional))

  ;; (define-syntax array-bind
  ;;   (ir-macro-transformer
  ;;     (lambda (form inject compare?)
  ;;       (let ((pat (cadr form))
  ;;             (arr (caddr form))
  ;;             (xpr (caddr form))
  ;;             (xprs (cdddr form)))
  ;;         (if (list? pat)
  ;;           `(if (= ,(length pat) (array-length ,arr))
  ;;              (array-apply (lambda ,pat ,xpr ,@xprs) ,arr)
  ;;              (error 'array-bind "match error" ',pat ,arr))
  ;;           ;; pseudolist: separate list part
  ;;           (receive (head tail)
  ;;             (let loop ((pat pat) (lst '()))
  ;;               (if (pair? pat)
  ;;                 (loop (cdr pat) (cons (car pat) lst))
  ;;                 (values (reverse lst) pat)))
  ;;             `(if (<= ,(length head) (array-length ,arr))
  ;;                (receive (hd tl) (array-split-at ,(length head) ,arr)
  ;;                  (let ((,tail tl))
  ;;                    (array-apply (lambda ,head ,xpr ,@xprs) hd)))
  ;;                (error 'array-bind "match error" ',pat ,arr))))))))

  (define-macro (array-bind pat arr xpr xprs)
    (if (list? pat)
        `(if (= ,(length pat) (array-length ,arr))
             (array-apply (lambda ,pat ,xpr ,@xprs) ,arr)
             (error 'array-bind "match error" ',pat ,arr))
        ;; pseudolist: separate list part
        (receive (head tail)
                 (let loop ((pat pat) (lst '()))
                   (if (pair? pat)
                       (loop (cdr pat) (cons (car pat) lst))
                       (values (reverse lst) pat)))
                 `(if (<= ,(length head) (array-length ,arr))
                      (receive (hd tl) (array-split-at ,(length head) ,arr)
                               (let ((,tail tl))
                                 (array-apply (lambda ,head ,xpr ,@xprs) hd)))
                      (error 'array-bind "match error" ',pat ,arr)))))

  (include "array.scm"))
