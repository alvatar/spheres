;;!!! Mutable heap with priority-queue operations and O(1) membership-testing
;; .author Peter Danenberg, 2015
;; .author Alvaro Castro-Castilla, 2015
;;
;; .source https://github.com/klutometis/heap
;; .version c5673e1306a608a898fe148134f049a7713e794b

;; "The heap data-structure"
;; (>? "Greater-than relation for keys")
;; (<? "Less-than relation for keys")
;; (inf "Infinity w.r.t. the inequality `>?'")
;; (data "Vector data-store underlying heap")
;; (size "Size of the heap as distinct from size of data")
;; (membership "Mapping from data to indices")
;; (@to "heap")
(define-structure heap
  >?
  <?
  inf
  data
  size
  membership)

(define-structure element
  key
  datum)

(define (parent i)
  (- (inexact->exact (floor (/ (+ i 1) 2))) 1))

(define (left i)
  (+ (* 2 i) 1))

(define (right i)
  (+ (* 2 i) 1 1))

;; "Is the heap empty?"
;; (heap "The heap to check")
;; (@to "boolean")
(define (heap-empty? heap)
  (zero? (heap-size heap)))

(define (heap-length heap)
  (vector-length (heap-data heap)))

(define (heap-ref heap i)
  (vector-ref (heap-data heap) i))

(define (heap-set! heap i element)
  (vector-set! (heap-data heap) i element)
  (hash-table-set!
   (heap-membership heap)
   (element-datum element)
   i))

(define (heap-swap! heap i j)
  (define (vector-swap! vec i j)
    (let ((x (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j x)))
  (vector-swap! (heap-data heap) i j)
  (hash-table-set!
   (heap-membership heap)
   (element-datum (heap-ref heap i))
   i)
  (hash-table-set!
   (heap-membership heap)
   (element-datum (heap-ref heap j))
   j))

;; "For a given datum, determine its index in the heap; or return
;; #f."
;; (heap "The heap in which to check")
;; (datum "The datum to check for")
;; (@to "integer or #f")
;; (@internal)
(define (heap-index heap datum)
  (hash-table-ref/default (heap-membership heap) datum #f))

;; "Is this datum a member in the heap?"
;; (heap "The heap in which to check")
;; (datum "The datum to check for")
;; (@to "boolean")
(define (heap-member? heap datum)
  (and (heap-index heap datum) #t))

;; "Find the key, given the datum."
;; (heap "The heap in which to search")
;; (datum "The datum whose key to find")
(define (heap-key heap datum)
  (let ((index (heap-index heap datum)))
    (and index (element-key (heap-ref heap index)))))

(define (heapify!/index heap i)
  (let ((heap->? (heap->? heap))
        (left (left i))
        (right (right i)))
    (let* ((extremum (if (and (< left (heap-size heap))
                              (heap->?
                               (element-key (heap-ref heap left))
                               (element-key (heap-ref heap i))))
                         left
                         i))
           (extremum (if (and (< right (heap-size heap))
                              (heap->?
                               (element-key (heap-ref heap right))
                               (element-key (heap-ref heap extremum))))
                         right
                         extremum)))
      (if (not (= extremum i))
          (begin (heap-swap! heap i extremum)
                 (heapify!/index heap extremum))))))

;; (define (heapify! heap datum)
;;   @("Given a heap-datum, reëstablish the heap-property."
;;     (heap "The heap in which to heapify")
;;     (datum "The datum whose element to heapify"))
;;   (let ((i (heap-index heap datum)))
;;     (if i
;;         (heapify! heap i)
;;         (error "Datum not found -- HEAPIFY!"))))

;; "Initial size of the heap data-store; exponentially resized on
;; overflow."
(define initial-heap-size
  (make-parameter 100))

;; "Make a max-heap."
;; (size "Initial heap-size")
;; (@to "max-heap")
(define make-max-heap
  (case-lambda
   (()
    (make-max-heap (initial-heap-size)))
   ((initial-heap-size)
    (make-heap >
               <
               -inf.0
               (make-vector initial-heap-size)
               0
               (make-hash-table)))))

;; "Make a min-heap."
;; (size "Initial heap-size")
;; (@to "min-heap")
(define make-min-heap
  (case-lambda
   (()
    (make-min-heap (initial-heap-size)))
   ((initial-heap-size)
    (make-heap <
               >
               +inf.0
               (make-vector initial-heap-size)
               0
               (make-hash-table)))))

;; (define (build-heap! heap)
;;   @("Heapify the entire data-store."
;;     (heap "The heap to heapify"))
;;   (heap-size-set! heap (vector-length (heap-data heap)))
;;   (let ((median (inexact->exact (floor (/ (heap-size heap) 2)))))
;;     ;; Should be i - 1 here?
;;     (do ((i (sub1 median) (sub1 i)))
;;         ((negative? i))
;;       (heapify!/index heap i))))

;; "Peak at the heap's extremum (min or max)."
;; (heap "The heap at which to peek")
;; (@to "datum")
(define (heap-extremum heap)
  (if (heap-empty? heap)
      (error "Heap underflow -- HEAP-EXTREMUM")
      (element-datum (heap-ref heap 0))))

;; "Return and delete the heap's extremum (min or max)."
;; (heap "The heap from which to extract")
;; (@to "datum")
(define (heap-extract-extremum! heap)
  (let ((extremum (heap-extremum heap)))
    (heap-set! heap 0 (heap-ref heap (- (heap-size heap) 1)))
    (heap-size-set! heap (- (heap-size heap) 1))
    (heapify!/index heap 0)
    extremum))

(define (heap-change-key!/index heap i new-key)
  (let ((old-key (element-key (heap-ref heap i))))
    (if ((heap-<? heap) new-key old-key)
        (error "Key violates heap-gradient -- HEAP-CHANGE-KEY!")
        (begin
          (element-key-set! (heap-ref heap i) new-key)
          (do ((i i (parent i)))
              ;; Do we also need to check for (negative? i)?
              ((or (zero? i)
                   ((heap->? heap)
                    (element-key (heap-ref heap (parent i)))
                    (element-key (heap-ref heap i)))))
            (heap-swap! heap i (parent i)))))))

;; "Change the key of the element with datum to new-key along the
;; heap-gradient."
;; (heap "The heap in which to change")
;; (datum "The datum whose key to change")
;; (new-key "The new key to assign to element with datum")
(define (heap-change-key! heap datum new-key)
  (let ((i (heap-index heap datum)))
    (if i
        (heap-change-key!/index heap i new-key)
        (error "Datum not found -- HEAP-CHANGE-KEY!"))))

;;; That's surely a bug, isn't it? Why not just insert and error out
;;; if the datum exists. On the other hand, it resembles hash-table
;;; behaviour.
;; "Insert a new element into the heap if the datum does not exist;
;; otherwise, adjust its key."
;; (heap "The heap in which to insert")
;; (element "The element to be inserted")
(define (heap-insert! heap key datum)
  ;; vector-resize is a Chicken Scheme library procedure
  ;; XXX optimize
  (define (vector-resize vec n init)
    (let ((new-vec (make-vector n init)))
      (let recur ((k (vector-length vec)))
        (if (zero? k)
            new-vec
            (begin (vector-set! new-vec k (vector-ref vec k))
                   (recur (-- k)))))))
  (if (heap-member? heap datum)
      (heap-change-key! heap datum key)
      (let ((heap-size (heap-size heap)))
        (if (= heap-size (heap-length heap))
            ;; Exponential resizing-strategy
            (heap-data-set! heap (vector-resize (heap-data heap)
                                                (* 2 heap-size))))
        (heap-size-set! heap (+ heap-size 1))
        (let ((element (make-element (heap-inf heap) datum)))
          (heap-set! heap heap-size element)
          (heap-change-key!/index heap heap-size key)))))

;; "Delete the ith element from the heap"
;; (heap "The heap from which to delete")
;; (i "The index of the element to delete")
;; (@internal)
(define (heap-delete!/index heap i)
  ;; Hypothesis
  (let ((heap-size (- (heap-size heap) 1)))
    (if (negative? heap-size)
        (error "Heap underflow -- HEAP-DELETE!")
        (let ((datum (element-datum (heap-ref heap i))))
          ;; (debug (##sys#slot datum 0)
          ;;        (##sys#slot datum 1)
          ;;        (##sys#slot datum 2)
          ;;        (##sys#slot datum 3))
          ;; (debug (heap-member? heap datum))
          (hash-table-delete! (heap-membership heap) datum)
          ;; (debug (heap-member? heap datum))
          (heap-size-set! heap heap-size)
          ;; (debug (heap-member? heap datum))
          (heap-set! heap i (heap-ref heap heap-size))
          ;; (debug (heap-member? heap datum))
          (heapify!/index heap i)
          ;; (debug (heap-member? heap datum))
          ))))

;; "Delete the element with datum"
;; (heap "The heap from which to delete")
;; (datum "The datum whose element to delete")
(define (heap-delete! heap datum)
  (let ((i (heap-index heap datum)))
    (if i
        (heap-delete!/index heap i)
        (error "Datum not found -- HEAP-DELETE!"))))

(define (element-copy element)
  (make-element (element-key element)
                (element-datum element)))

(define (heap-copy heap)
  (define (vector-for-each f vec)
    (let recur ((i 0)
                (len (vector-length vec)))
      (cond ((fx< i len)
             (f i (vector-ref vec i))
             (recur (fx+ i 1) len)))))
  (let ((copy (make-heap
               (heap->? heap)
               (heap-<? heap)
               (heap-inf heap)
               (make-vector (heap-length heap))
               (heap-size heap)
               (hash-table-copy (heap-membership heap)))))
    (vector-for-each
     (lambda (i x)
       ;; (let ((element (vector-ref (heap-data heap) i)))
       ;;   (when (element? element)
       ;;         (vector-set! (heap-data copy) i (element-copy element))))
       (when (element? x)
             (vector-set! (heap-data copy) i (element-copy x))))
     (heap-data heap))
    copy))

;; "Convert the heap into a key-sorted list of values by iteratively
;; extracting the extremum; see also [[heap->alist]]."
;; (heap "The heap to convert")
;; (@to "list")
(define (heap->list heap)
  ;; Or can we work on the data directly?
  (let ((heap (heap-copy heap)))
    (do ((list '() (cons (heap-extract-extremum! heap) list)))
        ((heap-empty? heap) list))))

;; "Convert the heap into a key-sorted list of key-value pairs by iteratively extracting the extremum; see also [[heap->list]]."
;; (heap "The heap to convert")
;; (@to "alist")
(define (heap->alist heap)
  (let ((heap (heap-copy heap)))
    (do ((lst
          '()
          (let ((datum (heap-extremum heap)))
            (cons (list (heap-key heap datum)
                        (heap-extract-extremum! heap))
                  lst))))
        ((heap-empty? heap) lst))))
