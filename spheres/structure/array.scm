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
;;
;; Author: Juergen Lorenz
;; ju (at) jugilo (dot) de
;;
;; Copyright (c) 2014, Juergen Lorenz
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author nor the names of its contributors may be
;; used to endorse or promote products derived from this software without
;; specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;
;; In this implementation, a functional array is internally represented
;; by a procedure closed over a completely balanced tree which acts via
;; message passing.  To arrive at an index position simply devide the
;; position argument recursively by 2 until it reaches 1 and inspect
;; quotient and remainder: If the latter is zero, follow the left,
;; otherwise the right subtree.
;;
;; Besides the operations like item, update! and add!, which operate
;; on individual indexes we need operations, which operate on the array
;; as a whole, like searching, copying or mapping. Of course, one could
;; use the individual operations looping along the range of indexes.
;; But this is slow, because if we had to go from index 365, say, to
;; 366, we had to repeat the whole path in the local search tree except
;; the last step.  To avoid this we maintain a local cursor which
;; allows to process the array by stepping successively along each tree
;; level in the correct order.
;;
;; Since access, adding and removing is fast, arrays can ideally be
;; used to implement sets. For example, to remove an item, simply swap!
;; it to the end and prune! it. This doesn't work for arrays, since
;; they are ordered by its indices, but it doesn't harm sets, which are
;; unorderd.
;;
;; We'll separate the library into three modules. The first contains the
;; actual closure, named array-handler, which does all the dirty work.
;;
;; The second is a record, which contains the array-handler as a field
;; as well as two index positions, from (included) and upto (excluded)
;; which allow fast subarray operations by simply sharing structure
;; as in the pointer arithmetic of C-arrays. But note, that updating a
;; subarray updates the original array as well. The same happens with
;; the standard list procedure list-tail (but not with subvectors,
;; which are freshly constructed).
;;
;; The third is the set implementation, a record as well, containing
;; the handler and an equality-predicate, from which an item-predicate
;; can be deduced. There is no point to consider ranges, since sets are
;; unordered.
;;

;;-------------------------------------------------------------------------------
;; Helper functions or dependencies

(define (any? x) #t)

(define (list-of? pred)
  (lambda (lst)
    (let loop ([lst lst])
      (cond ((null? lst) #t)
	    ((not (pair? lst)) #f)
	    ((pred (car lst)) (loop (cdr lst)))
	    (else #f)))))

(define (o . fns)
  (if (null? fns)
      (lambda (x) x)
      (let loop ((fns fns))
	(let ((h (car fns))
	      (t (cdr fns)) )
	  (if (null? t)
	      h
	      (lambda (x) (h ((loop t) x))))))))

;;-------------------------------------------------------------------------------

(define-syntax dispatch
  (syntax-rules ()
    ((_ proc0 proc1 ...)
     (lambda (msg)
       (case msg
         ((proc0) proc0)
         ((proc1) proc1)
         ...)))))

(define-syntax assert*
  (syntax-rules ()
    ((_ sym xpr)
     (let ((tmp xpr))
       (if tmp
           tmp
           (error sym "assertion failed" 'xpr))))
    ((_ sym xpr . xprs)
     (begin
       (assert* sym xpr)
       (assert* sym . xprs)))))

;;; tree interface
(define (make-tree) (vector))
(define (make-leaf x) (vector x (vector) (vector)))
(define (top tree) (vector-ref tree 0))
(define (left tree) (vector-ref tree 1))
(define (right tree) (vector-ref tree 2))
(define (top! val tree) (vector-set! tree 0 val))
(define (left! val tree) (vector-set! tree 1 val))
(define (right! val tree) (vector-set! tree 2 val))

(define-values (make-array-handler array-handler?)
  (let ((type (gensym 'array))
        (start (gensym 'start)))
    (values
     ;; constructor
     (case-lambda
      ((item?)
       (let ((item? item?)
             (count 0)                  ; number of all items
             (tree (make-tree))         ; actual data
             ;; to be populated and accessed by cursor-next
             (subtrees (vector))
             ;; to access subtrees
             (tree-index -1)
             (cursor-index -1)
             (cursor-item start)
             (move
              (lambda (k tr)
                (let loop ((k (fxarithmetic-shift-right k 1))
                           (rem (fxmodulo k 2))
                           (tr tr))
                  (if (fx= k 1)
                      (values tr rem)
                      (loop (fxarithmetic-shift-right k 1)
                            (fxmodulo k 2)
                            (if (fx= rem 0)
                                ;; choose left subtree
                                (left tr)
                                ;; choose right subtree
                                (right tr))))))))
         (letrec ((item
                   (lambda (k)
                     (cond
                      ((fx< k 0)
                       (error 'array-item "out of range" k))
                      ((fx>= k count)
                       (error 'array-item "out of range" k count))
                      ((fx= k 0) (top tree))
                      (else
                       (receive (tr rem) (move (fx+ k 1) tree)
                                (if (fx= rem 0)
                                    ;; choose item of left subtree
                                    (top (left tr))
                                    ;; choose item of right subtree
                                    (top (right tr))))))))
                  (add!
                   (lambda (new)
                     (let ((leaf (if (item? new)
                                     (make-leaf new)
                                     (error 'array-add! "wrong item type" new))))
                       (set! count (fx+ count 1))
                       (if (fx= count 1)
                           (set! tree leaf)
                           (receive (tr rem) (move count tree)
                                    (if (fx= rem 0)
                                        ;; insert left
                                        (left! leaf tr)
                                        ;; insert right
                                        (right! leaf tr)))))))
                  (prune!
                   (lambda ()
                     (cond
                      ((fx= count 0)
                       (error 'array-prune! "can't prune empty array"))
                      ((fx= count 1)
                       ;; set state to null state
                       (set! count 0)
                       (set! tree (make-tree))
                       (set! subtrees (vector))
                       (set! tree-index -1)
                       (set! cursor-index -1)
                       (set! cursor-item start))
                      (else
                       (let ((last (fx- count 1)))
                         (receive (tr rem) (move count tree)
                                  (if (fx= rem 0)
                                      ;; remove left leaf
                                      (left! (make-tree) tr)
                                      ;; remove right leaf
                                      (right! (make-tree) tr)))
                         (set! count last))))))
                  (update!
                   (lambda (k new)
                     (cond
                                        ;((fx< k 0)
                                        ; (error 'array-update! "out-of-range" k))
                                        ;((fx>= k count)
                                        ; (error 'array-update! "out-of-range" k))
                                        ; range check done outside handler
                      ((item? new)
                       (if (fx= k 0)
                           (top! new tree)
                           (receive (tr rem) (move (fx+ k 1) tree)
                                    (if (fx= rem 0)
                                        ;; update left item
                                        (top! new (left tr))
                                        ;; update right item
                                        (top! new (right tr))))))
                      (else
                       (error 'array-update! "wrong item type" new)))))
                  (cursor-finished?
                   (lambda ()
                     (fx< cursor-index 0)))
                  (cursor-start!
                   (lambda ()
                     (set! cursor-index -1)
                     (set! cursor-item start)))
                  (cursor-next!
                   (lambda ()
                     (cond
                      ((fx= cursor-index -1)
                       (set! subtrees (vector tree))
                       (set! tree-index 0)
                       (set! cursor-index 0)
                       (set! cursor-item (top tree)))
                      (else
                       (set! tree-index (fx+ tree-index 1))
                       (let ((vlen (vector-length subtrees)))
                         (if (fx= tree-index vlen)
                             (let ((trees (make-vector (fxarithmetic-shift-left vlen 1) start)))
                                        ;(let ((trees (make-vector (fxarithmetic-shift-left vlen 1) finish)))
                               ;; populate new vector with left ...
                               (do ((k 0 (fx+ k 1)))
                                   ((fx= k vlen))
                                 (vector-set! trees
                                              k
                                              (left (vector-ref subtrees k))))
                               ;; ... and with right subtrees of old vector
                               (do ((k 0 (fx+ k 1)))
                                   ((fx= k vlen))
                                 (vector-set! trees
                                              (fx+ vlen k)
                                              (right (vector-ref subtrees k))))
                               ;; update trees and index
                               (set! subtrees trees)
                               (set! tree-index 0)))
                         (cond
                          ((fx= cursor-index (fx- count 1))
                           (cursor-start!))
                          (else
                           (set! cursor-index (fx+ cursor-index 1))
                           (set! cursor-item
                                 (top (vector-ref subtrees
                                                  tree-index)))))))))))
           (dispatch
            type
            item?
            count
            item
            add!
            update!
            prune!
            cursor-item
            cursor-index
            cursor-start!
            cursor-next!
            cursor-finished?))))
      (() (make-array-handler any?)))
     ;; predicate
     ;; (lambda (xpr)
     ;;   (and (procedure? xpr)
     ;;        (condition-case (eq? (xpr 'type) type)
     ;;                        ((exn) #f))))
     (lambda (xpr)
       (and (procedure? xpr)
            (with-exception-handler
             (lambda (e) #f)
             (lambda () (eq? (xpr 'type) type))))))))

;;; other constructors
(define array-handler-repeat
  (case-lambda
   ((item? cnt item)
    (let ((result (make-array-handler item?)))
      (do ((k 0 (fx+ k 1)))
          ((fx= k cnt) result)
        ((result 'add!) item))))
   ((cnt item)
    (array-handler-repeat any? cnt item))))

(define array-handler-iterate
  (case-lambda
   ((item? cnt fn start)
    (let ((result (make-array-handler item?)))
      (do ((k 0 (fx+ k 1)) (item start (fn item)))
          ((fx= k cnt) result)
        ((result 'add!) item))))
   ((cnt fn start)
    (array-handler-iterate any? cnt fn start))))

(define array-handler-iterate-while
  (case-lambda
   ((item? ok? fn start)
    (let ((result (make-array-handler item?)))
      (do ((k 0 (fx+ k 1)) (item start (fn item)))
          ((not (ok? item)) result)
        ((result 'add!) item))))
   ((ok? fn start)
    (array-handler-iterate-while any? ok? fn start))))

(define array-handler-iterate-until
  (case-lambda
   ((item? ok? fn start)
    (let ((result (make-array-handler item?)))
      (do ((k 0 (fx+ k 1)) (item start (fn item)))
          ((ok? item) result)
        ((result 'add!) item))))
   ((ok? fn start)
    (array-handler-iterate-until any? ok? fn start))))

(define (array-handler-messages)
  '(type item?  count item add! update! prune! cursor-item cursor-index cursor-start! cursor-next! cursor-finished?))

;;; making binary comparisons nary
(define (nary? bincmp?)
  (lambda args
    (let loop ((args args))
      (cond
       ((null? args) #t)
       ((null? (cdr args)) #t)
       (else
        (and (bincmp? (car args) (cadr args))
             (loop (cdr args))))))))

;;; making binary operators nary
(define (nary binop)
  (lambda (arg . args)
    (if (null? args)
        arg
        (apply (nary binop) (binop arg (car args)) (cdr args)))))

;;; documentation
(define array-handlers
  (let ((signatures '((array-handler? xpr)
                      (make-array-handler [item?])
                      (array-handler-repeat [item?] cnt item)
                      (array-handler-iterate [item?] cnt fn start)
                      (array-handler-iterate-while [item?] ok? fn start)
                      (array-handler-iterate-until [item?] ok? fn start)
                      (array-handler-messages)
                      (nary binop)
                      (nary? bincmp?)
                      (assert* loc . xprs))))
    (case-lambda
     (() (map car signatures))
     ((sym) (assq sym signatures)))))


(define-record-type array
  (array-maker handler from upto) ; internal
  array?
  (handler array-handler)
  (from array-from array-from-set!) ; internal
  (upto array-upto array-upto-set!)) ; internal

;; (define-record-printer (array arr out)
;;   (display "@" out)
;;   (display (array->list arr) out)
;;   (newline out))

(define make-array                      ; exported
  (case-lambda
   ((item?)
    (let ((handler (make-array-handler item?)))
      (array-maker handler 0 (handler 'count))))
   (()
    (make-array any?))))

(define (array-item? arr)
  ((array-handler arr) 'item?))

(define (array-count arr)
  ((array-handler arr) 'count))

(define (array-length arr)
  (fx- (array-upto arr) (array-from arr)))

(define (array-null? arr)
  (fx= (array-length arr) 0))

(define (array-add! item arr)
  (assert* 'array-add!
           (fx= (array-count arr)
                (array-upto arr)))
  (((array-handler arr) 'add!) item)
  (array-upto-set! arr (fx+ (array-upto arr) 1)))

(define (array-prune! arr)
  (assert* 'array-prune!
           (fx= (array-count arr)
                (array-upto arr)))
  (((array-handler arr) 'prune!))
  (array-upto-set! arr (fx- (array-upto arr) 1)))

(define (array-item k arr)
  (assert* 'array-item
           (fx<= 0 k) (fx< k (array-length arr)))
  (((array-handler arr) 'item) (fx+ k (array-from arr))))

(define (array-update! k item arr)
  (assert* 'array-update!
           (fx<= 0 k) (fx< k (array-length arr)))
  (((array-handler arr) 'update!) (fx+ k (array-from arr)) item))

;;; cursor routines
(define (array-cursor-item arr)
  ((array-handler arr) 'cursor-item))

(define (array-cursor-index arr)
  ((array-handler arr) 'cursor-index))

(define (array-cursor-finished? arr)
  (fx< (array-cursor-index arr) (array-from arr)))

(define (array-cursor-start! arr)
  (let ((from (array-from arr)))
    (((array-handler arr) 'cursor-start!))
    (do ((k 0 (fx+ k 1)))
        ((fx= k from))
      (((array-handler arr) 'cursor-next!)))))

(define (array-cursor-next! arr)
  (if (fx= (array-cursor-index arr)
           (fx- (array-upto arr) 1))
      (array-cursor-start! arr)
      (((array-handler arr) 'cursor-next!))))

(define (array-cursor-goto! ok? arr)
  (array-cursor-next! arr)
  (do ()
      ((or (fx< (array-cursor-index arr)
                (array-from arr))
           (ok? (array-cursor-item arr))))
    (array-cursor-next! arr)))

(define (array-memp ok? arr)
  (array-cursor-start! arr)
  (array-cursor-goto! ok? arr)
  (if (array-cursor-finished? arr)
      #f
      (array-drop (array-cursor-index arr) arr)))

(define (array-memq x arr)
  (array-memp (cut eq? <> x) arr))

(define (array-memv x arr)
  (array-memp (cut eqv? <> x) arr))

(define (array-member x arr)
  (array-memp (cut equal? <> x) arr))

;;; constructors
(define array-repeat
  (case-lambda
   ((item? cnt item)
    (array-maker (array-handler-repeat item? cnt item) 0 cnt))
   ((cnt item)
    (array-repeat any? cnt item))))

(define array-iterate
  (case-lambda
   ((item? cnt fn start)
    (array-maker (array-handler-iterate item? cnt fn start) 0 cnt))
   ((cnt fn start)
    (array-iterate any? cnt fn start))))

(define array-iterate-while
  (case-lambda
   ((item? ok? fn start)
    (let ((handler (array-handler-iterate-while item? ok? fn start)))
      (array-maker handler 0 (handler 'count))))
   ((ok? fn start)
    (array-iterate-while any? ok? fn start))))

(define array-iterate-until
  (case-lambda
   ((item? ok? fn start)
    (let ((handler (array-handler-iterate-until item? ok? fn start)))
      (array-maker handler 0 (handler 'count))))
   ((ok? fn start)
    (array-iterate-until any? ok? fn start))))

(define (array-range from upto arr)
  (assert* 'array-range
           ((nary? fx<=) 0 from upto (array-length arr)))
  (let ((old-from (array-from arr))
        (old-upto (array-upto arr)))
    (let* ((new-from (fx+ old-from from))
           (new-upto (fx+ new-from (fx- upto from))))
      (if ((nary? fx<=) 0 new-from new-upto (array-count arr))
          (array-maker (array-handler arr) new-from new-upto)
          (error 'array-range "out of range" new-from new-upto)))))

(define (array-first arr)
  (array-item 0 arr))

(define (array-rest arr)
  (array-range 1 (array-length arr) arr))

(define (array-last arr)
  (array-item (fx- (array-length arr) 1) arr))

(define (array-butlast arr)
  (array-range 0 (fx- (array-length arr) 1) arr))

(define list->array
  (case-lambda
   ((item? lst)
    (let ((result (make-array item?)))
      (do ((lst lst (cdr lst)))
          ((null? lst) result)
        (array-add! (car lst) result))))
   ((lst)
    (list->array any? lst))))

(define vector->array
  (case-lambda
   ((item? vec)
    (let ((result (make-array item?)))
      (do ((k 0 (fx+ k 1)))
          ((fx= k (vector-length vec)) result)
        (array-add! (vector-ref vec k) result))))
   ((vec)
    (vector->array any? vec))))

(define (array arg/item? . args)
  (assert* 'array (if (procedure? arg/item?)
                      (not (null? args))
                      #t))
  ;; (if (and (procedure? arg/item?)
  ;;          (condition-case (arg/item? (car args))
  ;;                          ((exn) #f)))
  ;;     (list->array arg/item? args)
  ;;     (list->array any? (cons arg/item? args)))
  (if (and (procedure? arg/item?)
           (with-exception-handler
            (lambda (e) #f)
            (lambda () (arg/item? (car args)))))
      (list->array arg/item? args)
      (list->array any? (cons arg/item? args))))

(define (array->list arr)
  (array-cursor-start! arr)
  (let loop ((lst '()))
    (array-cursor-next! arr)
    (if (array-cursor-finished? arr)
        (reverse lst)
        (loop (cons (array-cursor-item arr) lst)))))

(define (array->vector arr)
  (let ((from (array-from arr))
        (result (make-vector (array-length arr) #f)))
    (array-cursor-start! arr)
    (let loop ()
      (array-cursor-next! arr)
      (cond
       ((array-cursor-finished? arr)
        result)
       (else
        (vector-set! result
                     (fx- (array-cursor-index arr) from)
                     (array-cursor-item arr))
        (loop))))))

(define (array-copy arr)
  (let ((result (make-array (array-item? arr))))
    (array-cursor-start! arr)
    (let loop ()
      (array-cursor-next! arr)
      (cond
       ((array-cursor-finished? arr)
        result)
       (else
        (array-add! (array-cursor-item arr) result)
        (loop))))))

(define (array-for-each proc . arrs)
  (assert* 'array-for-each
           ((list-of? array?) arrs) (procedure? proc))
  (for-each array-cursor-start! arrs)
  (let loop ()
    (for-each array-cursor-next! arrs)
    (unless (memq #t (map array-cursor-finished? arrs))
            (apply proc (map array-cursor-item arrs))
            (loop))))

(define (array-map fn/item? arr/fn . arrs)
  (assert* 'array-map (procedure? fn/item?)
           (or (procedure? arr/fn) (array? arr/fn))
           ((list-of? array?) arrs))
  (let ((acheck? (array? arr/fn)))
    (let ((item? (if acheck? any? fn/item?))
          (fn (if acheck? fn/item? arr/fn))
          (arrs (if acheck? (cons arr/fn arrs) arrs)))
      (let ((result (make-array item?)))
        (for-each array-cursor-start! arrs)
        (let loop ()
          (for-each array-cursor-next! arrs)
          (cond
           ((memq #t (map array-cursor-finished? arrs))
            result)
           (else
            (array-add! (apply fn (map array-cursor-item arrs))
                        result)
            (loop))))))))

(define (array-mappend fn . arrs)
  (array-apply array-append
               (apply array-map fn arrs)))

(define (array-append . arrs)
  (assert* 'array-append
           ((list-of? array?) arrs)
           (apply (nary? eq?) (map array-item? arrs))
           (not (null? arrs)))
  (cond
   ((null? (cdr arrs))
    (car arrs))
   ((null? (cddr arrs))
    (let ((arr0 (car arrs)) (arr1 (cadr arrs)))
      (let ((result (array-copy arr0)))
        (array-cursor-start! arr1)
        (let loop ()
          (array-cursor-next! arr1)
          (cond
           ((array-cursor-finished? arr1)
            result)
           (else
            (array-add! (array-cursor-item arr1) result)
            (loop)))))))
   (else
    (array-append (car arrs)
                  (apply array-append (cdr arrs))))))

(define (array-append! . arrs)
  (assert* 'array-append!
           ((list-of? array?) arrs)
           (apply (nary? eq?) (map array-item? arrs)))
  (cond
   ((null? arrs)
    (void))
   ((null? (cdr arrs))
    (void))
   ((null? (cddr arrs))
    (let ((arr0 (car arrs)) (arr1 (cadr arrs)))
      (array-cursor-start! arr1)
      (let loop ()
        (array-cursor-next! arr1)
        (unless (array-cursor-finished? arr1)
                (array-add! (array-cursor-item arr1) arr0)
                (loop)))))
   (else
    (for-each (lambda (arr) (array-append! (car arrs) arr))
              (cdr arrs)))))

(define (array-swap! k l arr)
  (let ((len (array-length arr)))
    (cond
     ((fx< k 0)
      (error 'array-swap! "out of range" k))
     ((fx>= k len)
      (error 'array-swap! "out of range" k))
     ((fx< l 0)
      (error 'array-swap! "out of range" l))
     ((fx>= l len)
      (error 'array-swap! "out of range" l))
     (else
      (let ((x (array-item k arr)))
        (array-update! k (array-item l arr) arr)
        (array-update! l x arr))))))

(define (array-reverse! arr)
  (let ((len (array-length arr)))
    (do ((m 0 (fx+ m 1)) (n (fx- len 1) (fx- n 1)))
        ((fx= m (fxarithmetic-shift-right len 1)))
      (array-swap! m n arr))))

(define (array-reverse arr)
  (let ((result (make-array (array-item? arr))))
    (do ((arr arr (array-butlast arr)))
        ((array-null? arr) result)
      (array-add! (array-last arr) result))))

(define (array-split-at k arr)
  (assert* 'array-split-at
           (array? arr)
           (fx<= 0 k) (fx<= k (array-length arr)))
  (values (array-range 0 k arr)
          (array-range k (array-length arr) arr)))

(define (array-take k arr)
  (array-range 0 k arr))

(define (array-drop k arr)
  (array-range k (array-length arr) arr))

(define (array-split-with ok? arr)
  (array-cursor-start! arr)
  (array-cursor-goto! (o not ok?) arr)
  (array-split-at (array-cursor-index arr) arr))
                                        ;
(define (array-take-while ok? arr)
  (call-with-values
      (lambda () (array-split-with ok? arr))
    (lambda (head tail) head)))

(define (array-drop-while ok? arr)
  (call-with-values
      (lambda () (array-split-with ok? arr))
    (lambda (head tail) tail)))

                                        ;(define (array-fold-left op base . arrs) ; ok
                                        ;  (let loop ((result base))
                                        ;    (cond
                                        ;      ;; all null
                                        ;      ((apply (nary? eq?) #t (map array-null? arrs))
                                        ;       result)
                                        ;      ;; all not null
                                        ;      ((apply (nary? eq?) #f (map array-null? arrs))
                                        ;       (loop (map array-rest arrs)
                                        ;             (apply op result (map array-first arrs))))
                                        ;       (else
                                        ;         (error 'array-fold-right "not of equal length" arrs)))))
(define (array-fold-left op base . arrs)
  (for-each array-cursor-start! arrs)
  (let loop ((result base))
    (for-each array-cursor-next! arrs)
    (cond
     ;; all null
     ((apply (nary? eq?) #t (map array-cursor-finished? arrs))
      result)
     ;; all not null
     ((apply (nary? eq?) #f (map array-cursor-finished? arrs))
      (loop (apply op result (map array-cursor-item arrs))))
     (else
      (error 'array-fold-left "not of equal length" arrs)))))

                                        ;(define (array-fold-right op base . arrs) ; ok
                                        ;  (let loop ((result base))
                                        ;    (cond
                                        ;      ;; all null
                                        ;      ((apply (nary? eq?) #t (map array-null? arrs))
                                        ;       result)
                                        ;      ;; all not null
                                        ;      ((apply (nary? eq?) #f (map array-null? arrs))
                                        ;       (loop (map array-butlast arrs)
                                        ;             (apply op (append (map array-last arrs) (list result)))))
                                        ;      (else
                                        ;        (error 'array-fold-right "not of equal length" arrs)))))
(define (array-fold-right op base . arrs)
  (for-each array-cursor-start! arrs)
  (let loop ()
    (for-each array-cursor-next! arrs)
    (cond
     ;; all null
     ((apply (nary? eq?) #t (map array-cursor-finished? arrs))
      base)
     ;; all not null
     ((apply (nary? eq?) #f (map array-cursor-finished? arrs))
      (apply op
             (append (map array-cursor-item arrs)
                     (list (loop)))))
     (else
      (error 'array-fold-right "not of equal length" arrs)))))

(define (array-sorted? <? arr)
  (array-cursor-start! arr)
  (let loop ()
    (array-cursor-next! arr)
    (array-cursor-next! arr)
    (if (array-cursor-finished? arr)
        #t
        (let ((item0 (array-cursor-item arr))
              (item1 (begin (array-cursor-next! arr)
                            (array-cursor-item arr))))
          (if (<? item0 item1)
              (loop)
              #f)))))

;;; a combination of quick sort and insertion sort
(define (array-sort! <? arr)
  (let recur ((l 0) (r (array-length arr)))
    (cond
     ((fx< (fx- r l) 2)
      ;; nothing to do
      (void))
     ((fx< (fx- r l) 10)
      ;; insertion sort
      (do ((m (fx+ l 1) (fx+ m 1)))
          ((fx= m r))
        (let ((item (array-item m arr)))
          (do ((k (fx- m 1) (fx- k 1)))
              ((or (fx< k l) (<? (array-item k arr) item))
               (array-update! (fx+ k 1) item arr))
            (array-update! (fx+ k 1) (array-item k arr) arr)))))
     (else
      ;; quick sort with median-of-three pivot
      (let ((m (fxarithmetic-shift-right r 1)) (r- (fx- r 1)))
        (if (<? (array-item m arr) (array-item l arr))
            (array-swap! l m arr))
        (if (<? (array-item r- arr) (array-item l arr))
            (array-swap! l r- arr))
        (if (<? (array-item r- arr) (array-item m arr))
            (array-swap! m r- arr))
        ;; now the median of the tree resides in position m
        ;; swap it to the left and use it as pivot
        (array-swap! l m arr)
        ;; partition
        (let ((rh r-)
              (lh (fx+ l 1))
              (boundary l)
              (pivot (array-item l arr)))
          (let loop ((n rh) (m lh))
            (unless (fx= m n)
                    (set! rh (do ((k n (fx- k 1)))
                                 ((or (fx= k lh)
                                      (<? (array-item k arr) pivot))
                                  k)))
                    (set! lh (do ((k m (fx+ k 1)))
                                 ((or (fx= k rh)
                                      (<? pivot (array-item k arr)))
                                  k)))
                    (array-swap! lh rh arr)
                    (loop rh lh)))
          (if (<? (array-item lh arr) pivot)
              (set! boundary lh))
          ;; pivot to correct position
          (array-swap! l boundary arr)
          (recur l boundary)
          (recur (fx+ boundary 1) r)
          ))))))

(define (array-apply fn . args)
  (let ((args (reverse args)))
    (assert* 'array-apply
             (not (null? args))
             (array? (car args)))
    (let ((head (reverse (array->list (car args))))
          (tail (cdr args)))
      (apply fn (reverse (append head tail))))))

(define (array-equ? equ? . arrs)
  (cond
   ((not (apply (nary? fx=) (map array-length arrs)))
    #f)
   ((not (apply (nary? eq?) (map array-item? arrs)))
    #f)
   (else
    (for-each array-cursor-start! arrs)
    (let loop ()
      (for-each array-cursor-next! arrs)
      (cond
       ((apply (nary? eq?)
               (cons #t (map array-cursor-finished? arrs)))
        #t)
       ((apply (nary? equ?) (map array-cursor-item arrs))
        (loop))
       (else #f))))))

(define (array-eq? . arrs)
  (apply array-equ? eq? arrs))

(define (array-eqv? . arrs)
  (apply array-equ? eqv? arrs))

(define (array-equal? . arrs)
  (apply array-equ? equal? arrs))

(define (array-filter ok? arr)
  (let ((yes (make-array (array-item? arr)))
        (no  (make-array (array-item? arr))))
    (array-cursor-start! arr)
    (let loop ()
      (array-cursor-next! arr)
      (cond
       ((array-cursor-finished? arr)
        (values yes no))
       ((ok? (array-cursor-item arr))
        (array-add! (array-cursor-item arr) yes)
        (loop))
       (else
        (array-add! (array-cursor-item arr) no)
        (loop))))))
                                        ;
(define (array-remp ok? arr)
  (call-with-values (lambda () (array-filter ok? arr))
    (lambda (a b) b)))
                                        ;
(define (array-remove item arr)
  (array-remp (cut equal? <> item) arr))

(define (array-remq item arr)
  (array-remp (cut eq? <> item) arr))

(define (array-remv item arr)
  (array-remp (cut eqv? <> item) arr))

(define (array-remove-dups equ? arr)
  (let ((result (make-array (array-item? arr))))
    (array-cursor-start! arr)
    (let loop ()
      (array-cursor-next! arr)
      (cond
       ((array-cursor-finished? arr)
        result)
       (else
        (array-cursor-start! result)
        (array-cursor-goto! (cut equ? <> (array-cursor-item arr)) result)
        (if (array-cursor-finished? result) ; not found
            (array-add! (array-cursor-item arr) result))
        (loop))))))

(define (array-unzip arr)
  (let ((arr0 (make-array (array-item? arr)))
        (arr1 (make-array (array-item? arr))))
    (array-cursor-start! arr)
    (let loop ((k 0))
      (array-cursor-next! arr)
      (cond
       ((array-cursor-finished? arr)
        (values arr0 arr1))
       (else
        (if (fxeven? k)
            (array-add! (array-cursor-item arr) arr0)
            (array-add! (array-cursor-item arr) arr1))
        (loop (fx+ k 1)))))))

(define (array-zip arr0 arr1)
  (assert* 'array-zip
           (array? arr0)
           (array? arr1)
           (eq? (array-item? arr0) (array-item? arr1)))
  (let ((result (make-array (array-item? arr0))))
    (array-cursor-start! arr0)
    (array-cursor-start! arr1)
    (array-cursor-next! arr0)
    (array-cursor-next! arr1)
    (do ()
        ((and (array-cursor-finished? arr0)
              (array-cursor-finished? arr1)))
      (cond
       ((array-cursor-finished? arr0)
        (array-add! (array-cursor-item arr1) result)
        (array-cursor-next! arr1))
       ((array-cursor-finished? arr1)
        (array-add! (array-cursor-item arr0) result)
        (array-cursor-next! arr0))
       (else
        (array-add! (array-cursor-item arr0) result)
        (array-cursor-next! arr0)
        (array-add! (array-cursor-item arr1) result)
        (array-cursor-next! arr1))))
    result))

(define (array-interpose sep arr)
  (assert* 'array-interpose
           (array? arr)
           ((array-item? arr) sep))
  (let ((result (make-array (array-item? arr))))
    (array-cursor-start! arr)
    ;; add first item
    (unless (array-null? arr)
            (array-cursor-next! arr)
            (array-add! (array-cursor-item arr) result))
    (let loop ()
      (array-cursor-next! arr)
      (cond
       ((array-cursor-finished? arr)
        result)
       (else
        (array-add! sep result)
        (array-add! (array-cursor-item arr) result)
        (loop))))))

(define (array-every? ok? arr)
  (array-cursor-start! arr)
  (array-cursor-goto! (o not ok?) arr)
  (array-cursor-finished? arr))

(define (array-some? ok? arr)
  (array-cursor-start! arr)
  (array-cursor-goto! ok? arr)
  (not (array-cursor-finished? arr)))

(define (array-in? =? arr0 arr1)
  (cond
   ((array-null? arr0)
    #t)
   ((array-null? arr1)
    (array-null? arr0))
   (else
    (array-cursor-start! arr0)
    (array-cursor-start! arr1)
    (let loop ()
      (array-cursor-next! arr0)
      (array-cursor-goto! (cut =? <> (array-cursor-item arr0)) arr1)
      (cond
       ((fx< (array-cursor-index arr0) (array-from arr0))
        #t)
       ((fx= (array-cursor-item arr0) (array-cursor-item arr1))
        (loop))
       (else #f))))))

;; ;;; documentation
;; (define arrays
;;   (let ((signatures '((array? xpr)
;;                       (array-null? xpr)
;;                       (make-array [item?])
;;                       (array [item?] . args)
;;                       (list->array [item?] lst)
;;                       (vector->array [item?] vec)
;;                       (array-repeat [item?] cnt item)
;;                       (array-iterate [item?] cnt fn start)
;;                       (array-iterate-while [item?] ok? fn start)
;;                       (array-iterate-until [item?] ok? fn start)
;;                       (array-copy arr)
;;                       (array->list arr)
;;                       (array->vector arr)
;;                       (array-cursor-start! arr)
;;                       (array-cursor-next! arr)
;;                       (array-cursor-item arr)
;;                       (array-cursor-index arr)
;;                       (array-cursor-finished? arr)
;;                       (array-cursor-goto! ok? arr)
;;                       (array-memp ok? arr)
;;                       (array-member item arr)
;;                       (array-memq item arr)
;;                       (array-memv item arr)
;;                       (array-handler arr)
;;                       (array-first arr)
;;                       (array-rest arr)
;;                       (array-last arr)
;;                       (array-butlast arr)
;;                       (array-add! item arr)
;;                       (array-update! index item arr)
;;                       (array-prune! arr)
;;                       (array-apply fn . args)
;;                       (array-reverse arr)
;;                       (array-reverse! arr)
;;                       (array-swap! k l arr)
;;                       (array-length arr)
;;                       (array-count arr)
;;                       (array-range from upto arr) ; subarray
;;                       (array-item k arr)          ; ref
;;                       (array-split-at k arr)
;;                       (array-split-with ok? arr)
;;                       (array-drop k arr)
;;                       (array-drop-while ok? arr)
;;                       (array-take k arr)
;;                       (array-take-while ok? arr)
;;                       (array-append . arrs)
;;                       (array-append! . arrs)
;;                       (array-map [item?] fn . arrs)
;;                       (array-mappend fn . arrs)
;;                       (array-for-each fn . arrs)
;;                       (array-filter ok? arr)
;;                       (array-equ? equ? . arrs)
;;                       (array-equal? . arrs)
;;                       (array-eqv? . arrs)
;;                       (array-eq? . arrs)
;;                       (array-remp ok? arr)
;;                       (array-remove item arr)
;;                       (array-remq item arr)
;;                       (array-remv item arr)
;;                       (array-remove-dups equ? arr)
;;                       (array-fold-left op base . arrs)
;;                       (array-fold-right op base . arrs)
;;                       (array-sorted? <? arr)
;;                       (array-sort! <? arr)
;;                       (array-zip arr0 arr1)
;;                       (array-unzip arr)
;;                       (array-interpose sep arr)
;;                       (array-every? ok? arr)
;;                       (array-some? ok? arr)
;;                       (array-in? =? arr0 arr1)
;;                       (array-bind (x ... . xs) arr xpr . xprs))))
;;     (case-lambda
;;      (() (map car signatures))
;;      ((sym) (assq sym signatures)))))


(define-record-type set
  (set-maker handler equ?) ; internal
  set?
  (handler set-handler)
  (equ? set-equ?))

;; (define-record-printer (set st out)
;;   (let ((str (->string (set->list st))))
;;     (string-set! str 0 #\{)
;;     (string-set! str (fx- (string-length str) 1) #\})
;;     (display str out)
;;     (newline out)))

;; (define (cmp->type equ?)                ; internal
;;   (lambda (x)
;;     (condition-case (equ? x x)
;;                     ((exn type) #f))))

(define (cmp->type equ?)                ; internal
  (lambda (x)
    (with-exception-handler
     (lambda (e) #f)
     (lambda () (equ? x x)))))

(define make-set ; exported
  (case-lambda
   ((equ?)
    (set-maker (make-array-handler (cmp->type equ?)) equ?))
   (()
    (make-set eqv?))))

(define set-iterate
  (case-lambda
   ((equ? cnt fn start)
    (set-maker (array-handler-iterate (cmp->type equ?) cnt fn start) equ?))
   ((cnt fn start)
    (set-iterate eqv? cnt fn start))))

(define set-iterate-while
  (case-lambda
   ((equ? ok? fn start)
    (set-maker (array-handler-iterate-while (cmp->type equ?) ok? fn
                                            start)
               equ?))
   ((ok? fn start)
    (set-iterate-while eqv? ok? fn start))))

(define set-iterate-until
  (case-lambda
   ((equ? ok? fn start)
    (set-maker (array-handler-iterate-until (cmp->type equ?) ok? fn start)
               equ?))
   ((ok? fn start)
    (set-iterate-until eqv? ok? fn start))))

(define (set arg/equ? . args)
  (assert* 'array (if (procedure? arg/equ?)
                      (not (null? args))
                      #t))
  ;; (if (and (procedure? arg/equ?)
  ;;          (condition-case (arg/equ? (car args) (car args))
  ;;                          ((exn) #f)))
  ;;     (list->set arg/equ? args)
  ;;     (list->set eqv? (cons arg/equ? args)))
  (if (and (procedure? arg/equ?)
           (with-exception-handler
            (lambda (e) #f)
            (lambda () (arg/equ? (car args) (car args)))))
      (list->set arg/equ? args)
      (list->set eqv? (cons arg/equ? args))))

(define list->set
  (case-lambda
   ((equ? lst)
    (let ((result (make-set equ?)))
      (do ((lst lst (cdr lst)))
          ((null? lst) result)
        (set-add! (car lst) result))))
   ((lst)
    (list->set eqv? lst))))

(define vector->set
  (case-lambda
   ((equ? vec)
    (let ((result (make-set equ?)))
      (do ((k 0 (fx+ k 1)))
          ((fx= k (vector-length vec)) result)
        (set-add! (vector-ref vec k) result))))
   ((vec)
    (vector->set eqv? vec))))

(define (set-item? st)
  ((set-handler st) 'item?))

(define (set-count st)
  ((set-handler st) 'count))

(define (set-null? xpr)
  (and (set? xpr)
       (zero? ((set-handler xpr) 'count))))

(define (set-in item st)
  (if (set-null? st)
      #f
      (let ((handler (set-handler st))
            (equ? (set-equ? st)))
        ((handler 'cursor-start!))
        (let loop ()
          ((handler 'cursor-next!))
          (cond
           (((handler 'cursor-finished?))
            #f)
           ((equ? item (handler 'cursor-item))
            (handler 'cursor-index))
           (else
            (loop)))))))

(define (set<= set0 set1)
  (cond
   ((set-null? set0) #t)
   ((eq? (set-equ? set0) (set-equ? set1))
    (let ((handler (set-handler set0)))
      ((handler 'cursor-start!))
      (let loop ()
        ((handler 'cursor-next!))
        (cond
         (((handler 'cursor-finished?))
          #t)
         ((set-in (handler 'cursor-item) set1)
          (loop))
         (else #f)))))
   (else #f)))

(define (set>= set0 set1)
  (set<= set1 set0))

(define (set= set0 set1)
  (and (set<= set0 set1) (set>= set0 set1)))

(define (set-add! item st)
  (assert* 'set-add!
           ((set-item? st) item))
  (unless (set-in item st)
          (((set-handler st) 'add!) item)))

(define (set-remove! item st)
  (assert* 'set-remove!
           (not (set-null? st))
           ((set-item? st) item))
  (let ((handler (set-handler st)))
    (let ((ind (set-in item st))
          (last (fx- (handler 'count) 1)))
      (if ind
          (let ((x ((handler 'item) ind)))
            ((handler 'update!) ind
             ((handler 'item) last))
            ((handler 'update!) last x)
            ((handler 'prune!)))))))

(define (set->list st)
  (if (set-null? st)
      '()
      (let ((handler (set-handler st)))
        ((handler 'cursor-start!))
        (let loop ((lst '()))
          ((handler 'cursor-next!))
          (cond
           (((handler 'cursor-finished?))
            (reverse lst))
           (else
            (loop (cons (handler 'cursor-item) lst))))))))

(define (set->vector st)
  (if (set-null? st)
      '#()
      (let ((handler (set-handler st))
            (result (make-vector (set-count st) #f)))
        ((handler 'cursor-start!))
        (let loop ()
          ((handler 'cursor-next!))
          (cond
           (((handler 'cursor-finished?))
            result)
           (else
            (vector-set! result
                         (handler 'cursor-index)
                         (handler 'cursor-item))
            (loop)))))))

(define (set-filter ok? st)
  (let ((yes (make-set (set-equ? st)))
        (no  (make-set (set-equ? st))))
    (if (set-null? st)
        (values yes no)
        (let ((handler (set-handler st)))
          ((handler 'cursor-start!))
          (let loop ()
            ((handler 'cursor-next!))
            (cond
             (((handler 'cursor-finished?))
              (values yes no))
             ((ok? (handler 'cursor-item))
              (set-add! (handler 'cursor-item) yes)
              (loop))
             (else
              (set-add! (handler 'cursor-item) no)
              (loop))))))))

(define (set-map fn/equ? set/fn . sets)
  (assert* 'set-map (procedure? fn/equ?)
           (or (procedure? set/fn) (set? set/fn))
           ((list-of? set?) sets))
  (let ((check? (set? set/fn)))
    (let ((equ? (if check? eqv? fn/equ?))
          (fn (if check? fn/equ? set/fn))
          (sets (if check? (cons set/fn sets) sets)))
      (let ((result (make-set equ?)))
        (if (memq #t (map set-null? sets))
            result
            (let ((handlers (map set-handler sets)))
              (for-each (lambda (hd) ((hd 'cursor-start!))) handlers)
              (let loop ()
                (for-each (lambda (hd) ((hd 'cursor-next!))) handlers)
                (cond
                 ((memq #t (map (lambda (hd) ((hd 'cursor-finished?)))
                                handlers))
                  result)
                 (else
                  (set-add! (apply fn (map (lambda (hd) (hd 'cursor-item))
                                           handlers))
                            result)
                  (loop))))))))))

(define (set-for-each proc . sets)
  (assert* 'set-for-each
           ((list-of? set?) sets) (procedure? proc))
  (unless (memq #t (map set-null? sets))
          (let ((handlers (map set-handler sets)))
            (for-each (lambda (hd) ((hd 'cursor-start!))) handlers)
            (let loop ()
              (for-each (lambda (hd) ((hd 'cursor-next!))) handlers)
              (unless (memq #t (map (lambda (hd) ((hd 'cursor-finished?)))
                                    handlers))
                      (apply proc (map (lambda (hd) (hd 'cursor-item)) handlers))
                      (loop))))))

(define (set-copy st)
  (let ((result (make-set (set-equ? st))))
    (if (set-null? st)
        result
        (let ((handler (set-handler st)))
          ((handler 'cursor-start!))
          (let loop ()
            ((handler 'cursor-next!))
            (cond
             (((handler 'cursor-finished?)) result)
             (else
              (set-add! (handler 'cursor-item) result)
              (loop))))))))

(define (set-difference set0 set1)
  (assert* 'set-difference (eq? (set-equ? set0) (set-equ? set1)))
  (let ((result (set-copy set0)))
    (if (set-null? set1)
        result
        (let ((handler (set-handler set1)))
          ((handler 'cursor-start!))
          (let loop ()
            ((handler 'cursor-next!))
            (cond
             (((handler 'cursor-finished?))
              result)
             ((set-in (handler 'cursor-item) set0)
              (set-remove! (handler 'cursor-item) result)
              (loop))
             (else
              (loop))))))))

(define (set-union . sets)
  (assert* 'set-union
           (not (null? sets))
           ((list-of? set?) sets)
           (apply (nary? eq?) (map set-equ? sets)))
  (cond
   ((null? (cdr sets))
    (car sets))
   ((null? (cddr sets))
    (let ((result (set-copy (car sets))))
      (if (set-null? (cadr sets))
          result
          (let ((handler (set-handler (cadr sets))))
            ((handler 'cursor-start!))
            (let loop ()
              ((handler 'cursor-next!))
              (cond
               (((handler 'cursor-finished?))
                result)
               (else
                (set-add! (handler 'cursor-item) result)
                (loop))))))))
   (else
    (set-union (car sets) (apply set-union (cdr sets))))))

(define (set-intersection . sets)
  (assert* 'set-intersection
           (not (null? sets))
           ((list-of? set?) sets)
           (apply (nary? eq?) (map set-equ? sets)))
  (cond
   ((null? (cdr sets))
    (car sets))
   ((null? (cddr sets))
    (let ((result (make-set (set-equ? (car sets)))))
      (if (set-null? (cadr sets))
          result
          (let ((handler (set-handler (cadr sets))))
            ((handler 'cursor-start!))
            (let loop ()
              ((handler 'cursor-next!))
              (if ((handler 'cursor-finished?))
                  result
                  (let ((ind (set-in (handler 'cursor-item) (car sets))))
                    (if ind
                        (set-add! (handler 'cursor-item) result))
                    (loop))))))))
   (else
    (set-intersection (car sets) (apply set-intersection (cdr sets))))))

(define (set-every? ok? st)
  (if (set-null? st)
      #t
      (let ((handler (set-handler st)))
        ((handler 'cursor-start!))
        (let loop ()
          ((handler 'cursor-next!))
          (cond
           (((handler 'cursor-finished?)) #t)
           ((ok? (handler 'cursor-item)) (loop))
           (else #f))))))

(define (set-some? ok? st)
  (if (set-null? st)
      #f
      (let ((handler (set-handler st)))
        ((handler 'cursor-start!))
        (let loop ()
          ((handler 'cursor-next!))
          (cond
           (((handler 'cursor-finished?)) #f)
           ((ok? (handler 'cursor-item)) #t)
           (else (loop)))))))

(define (set-apply fn . args)
  (let ((args (reverse args)))
    (assert* 'set-apply
             (not (null? args))
             (set? (car args)))
    (let ((head (reverse (set->list (car args))))
          (tail (cdr args)))
      (apply fn (reverse (append head tail))))))

;; ;; documentation procedure
;; (define sets
;;   (let ((signatures '((set? xpr)
;;                       (make-set [equ?])
;;                       (set-iterate [equ?] n fn start)
;;                       (set-iterate-while [equ?] ok? fn start)
;;                       (set-iterate-until [equ?] ok? fn start)
;;                       (list->set [equ?] lst)
;;                       (vector->set [equ?] vec)
;;                       (set [equ?] . args)
;;                       (set->list st)
;;                       (set->vector st)
;;                       (set-in item st)
;;                       (set<= set0 set1)
;;                       (set= set0 set1)
;;                       (set>= set0 set1)
;;                       (set-filter ok? st)
;;                       (set-map [equ?] fn . sets)
;;                       (set-for-each proc . sets)
;;                       (set-null? xpr)
;;                       (set-add! item st)
;;                       (set-remove! item st)
;;                       (set-count st)
;;                       (set-copy st)
;;                       (set-difference set0 set1)
;;                       (set-union . sets)
;;                       (set-intersection . sets)
;;                       (set-every? ok? st)
;;                       (set-some? ok? st)
;;                       (set-apply fn . args)
;;                       (set-handler st)
;;                       (set-equ? st)
;;                       (set-item? st))))
;;     (case-lambda
;;      (() (map car signatures))
;;      ((sym) (assq sym signatures)))))





;; (import arrays)
;; (define (array-merge <? arr0 arr1) ; internal
;;  (let loop ((arr0 arr0)
;;             (arr1 arr1))
;;    (cond
;;      ((array-null? arr0) arr1)
;;      ((array-null? arr1) arr0)
;;      (else
;;         ;ok, but far too slow
;;        (let ((item0 (array-first arr0))
;;              (item1 (array-first arr1)))
;;          (cond
;;            ((<? item0 item1)
;;             (array-append (array (array-item? arr0) item0)
;;                           (loop (array-rest arr0) arr1)))
;;            (else
;;              (array-append (array (array-item? arr0) item1)
;;                            (loop arr0 (array-rest arr1))))))))))
;; merge sort
;; (define (array-sort <? arr)
;;  (let loop ((arr arr))
;;    (let ((len (array-length arr)))
;;      (if (< len 2)
;;        (array-copy arr)
;;        (receive (head tail) (array-split-at (fxshr len 1) arr)
;;          (array-merge <? (loop head) (loop tail)))))))
