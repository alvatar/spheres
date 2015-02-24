
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The most basic set interface.  We provide a data type, low-level
;; constructor, and membership test.  This allows libraries to provide
;; an iset API without needing to include the full iset library.

(define-record-type Integer-Set
  (%make-iset start end bits left right)
  iset?
  (start iset-start iset-start-set!)
  (end   iset-end   iset-end-set!)
  (bits  iset-bits  iset-bits-set!)
  (left  iset-left  iset-left-set!)
  (right iset-right iset-right-set!))

(define (make-iset . opt)
  (if (null? opt)
      (%make-iset 0 0 0 #f #f)
      (let ((end (if (pair? (cdr opt)) (cadr opt) (car opt))))
        (%make-iset (car opt) end #f #f #f))))

(define (iset-contains? iset n)
  (let lp ((is iset))
    (let ((start (iset-start is)))
      (if (< n start)
          (let ((left (iset-left is))) (and left (lp left)))
          (let ((end (iset-end is)))
            (if (> n end)
                (let ((right (iset-right is))) (and right (lp right)))
                (let ((bits (iset-bits is)))
                  (or (not bits)
                      (bit-set? (- n start) bits)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for constructing and joining isets.

(define bits-thresh 128)  ; within 128 we join into a bitmap
;;(define bits-max 512)     ; don't make bitmaps larger than this

(define (bit-set n index)
  (bitwise-ior n (arithmetic-shift 1 index)))

(define (bit-clear n index)
  (if (bit-set? index n)
      (- n (arithmetic-shift 1 index))
      n))

(define (iset . args)
  (list->iset args))

(define (list->iset! ls iset)
  (for-each (lambda (i) (iset-adjoin1! iset i)) ls)
  iset)

(define (list->iset ls . opt)
  (list->iset! ls (if (pair? opt) (iset-copy (car opt)) (make-iset))))

(define (iset-copy iset)
  (and iset
       (%make-iset
        (iset-start iset)
        (iset-end iset)
        (iset-bits iset)
        (iset-copy (iset-left iset))
        (iset-copy (iset-right iset)))))

(define (iset-copy-node iset)
  (%make-iset (iset-start iset) (iset-end iset) (iset-bits iset) #f #f))

(define (iset-max-end iset)
  (cond ((iset-right iset) => iset-max-end)
        (else (iset-end iset))))

(define (iset-min-start iset)
  (cond ((iset-left iset) => iset-min-start)
        (else (iset-start iset))))

(define (iset-insert-left! iset new)
  (let ((left (iset-left iset)))
    (if (and left (< (iset-end new) (iset-start left)))
        (iset-right-set! new left)
        (iset-left-set! new left)))
  (iset-left-set! iset new))

(define (iset-insert-right! iset new)
  (let ((right (iset-right iset)))
    (if (and right (< (iset-end new) (iset-start right)))
        (iset-right-set! new right)
        (iset-left-set! new right)))
  (iset-right-set! iset new))

(define (range->bits start end)
  (- (arithmetic-shift 1 (+ 1 (- end start))) 1))

(define (iset-squash-bits! iset)
  (let ((bits (iset-bits iset)))
    (if (and bits (= bits (range->bits (iset-start iset) (iset-end iset))))
        (iset-bits-set! iset #f))))

(define (iset-should-merge-left? a b)
  (and (< (- (iset-start a) (iset-end b))
          bits-thresh)
       (or (not (iset-left a))
           (> (iset-start b) (iset-max-end (iset-left a))))))

(define (iset-should-merge-right? a b)
  (and (< (- (iset-start b) (iset-end a))
          bits-thresh)
       (or (not (iset-right a))
           (< (iset-end b) (iset-min-start (iset-right a))))))

(define (iset-merge-left! a b)
  (if (or (iset-bits a) (iset-bits b)
          (< (+ 1 (iset-end b)) (iset-start a)))
      (let* ((a-bits (or (iset-bits a)
                         (range->bits (iset-start a) (iset-end a))))
             (b-bits (or (iset-bits b)
                         (range->bits (iset-start b) (iset-end b))))
             (shift (- (iset-start a) (iset-start b)))
             (bits (bitwise-ior b-bits (arithmetic-shift a-bits shift))))
        (iset-bits-set! a bits)))
  (iset-start-set! a (iset-start b)))

(define (iset-merge-right! a b)
  (if (or (iset-bits a) (iset-bits b)
          (< (+ 1 (iset-end a)) (iset-start b)))
      (let* ((a-bits (or (iset-bits a)
                         (range->bits (iset-start a) (iset-end a))))
             (b-bits (or (iset-bits b)
                         (range->bits (iset-start b) (iset-end b))))
             (shift (- (iset-start b) (iset-start a)))
             (bits (bitwise-ior a-bits (arithmetic-shift b-bits shift))))
        (iset-bits-set! a bits)))
  (iset-end-set! a (iset-end b)))

(define (iset-adjoin1! is n)
  (iset-adjoin-node! is (%make-iset n n #f #f #f)))

;; adjoin just the node b (ignoring left/right) to the full iset a
(define (iset-adjoin-node! a b)
  (cond
   ((iset-empty? a)
    (iset-start-set! a (iset-start b))
    (iset-end-set! a (iset-end b))
    (iset-bits-set! a (iset-bits b)))
   ((not (iset-empty? b))
    (let ((a-start (iset-start a))
          (a-end (iset-end a))
          (a-bits (iset-bits a))
          (b-start (iset-start b))
          (b-end (iset-end b))
          (b-bits (iset-bits b)))
      (cond
       ;;         aaaa...
       ;; ...bbbb
       ((<= b-end a-start)
        (if (iset-should-merge-left? a b)
            (iset-merge-left! a b)
            (iset-adjoin-node-left! a b)))
       ;; ...aaaa
       ;;         bbbb...
       ((>= b-start a-end)
        (if (iset-should-merge-right? a b)
            (iset-merge-right! a b)
            (iset-adjoin-node-right! a b)))
       ;; ...aaaaa...
       ;;  ...bb...
       ((and (>= b-start a-start) (<= b-end a-end))
        (if a-bits
            (let ((b-bits (arithmetic-shift
                           (or b-bits (range->bits b-start b-end))
                           (- b-start a-start))))
              (iset-bits-set! a (bitwise-ior a-bits b-bits))
              (iset-squash-bits! a))))
       (else
        ;; general case: split, recurse, join sides
        (let ((ls (iset-node-split b a-start a-end)))
          (if (car ls)
              (iset-adjoin-node-left! a (car ls)))
          (iset-adjoin-node! a (cadr ls))
          (if (car (cddr ls))
              (iset-adjoin-node-right! a (car (cddr ls)))))))))))

(define (iset-adjoin-node-left! iset node)
  (if (iset-left iset)
      (iset-adjoin-node! (iset-left iset) node)
      (iset-left-set! iset node)))

(define (iset-adjoin-node-right! iset node)
  (if (iset-right iset)
      (iset-adjoin-node! (iset-right iset) node)
      (iset-right-set! iset node)))

;; start and/or end are inside the node, split into:
;;   1. node before start, if any
;;   2. node between start and end
;;   3. node after end, if any
(define (iset-node-split node start end)
  (list (and (< (iset-start node) start)
             (iset-node-extract node (iset-start node) (- start 1)))
        (iset-node-extract node start end)
        (and (> (iset-end node) end)
             (iset-node-extract node (+ end 1) (iset-end node)))))

(define (iset-node-extract node start end)
  (cond
   ((iset-bits node)
    => (lambda (node-bits)
         (let* ((bits
                 (bitwise-and
                  (arithmetic-shift node-bits (- (iset-start node) start))
                  (range->bits start end)))
                (new-end (min end (+ start (integer-length bits)))))
           (%make-iset start new-end bits #f #f))))
   (else
    (%make-iset (max start (iset-start node))
                (min end (iset-end node))
                #f #f #f))))

(define (iset-adjoin! iset . ls)
  (list->iset! ls iset))

(define (iset-adjoin iset . ls)
  (list->iset ls iset))

;; delete directly in this node
(define (%iset-delete1! iset n)
  (let ((start (iset-start iset))
        (end (iset-end iset))
        (bits (iset-bits iset)))
    (cond
     (bits
      (iset-bits-set! iset (bit-clear bits (- n start))))
     ((= n start)
      (if (= n end)
          (iset-bits-set! iset 0)
          (iset-start-set! iset (+ n 1))))
     ((= n end)
      (iset-end-set! iset (- n 1)))
     (else
      (iset-end-set! iset (- n 1))
      (iset-insert-right! iset (make-iset (+ n 1) end))))))

(define (iset-delete1! iset n)
  (let lp ((is iset))
    (let ((start (iset-start is)))
      (if (< n start)
          (let ((left (iset-left is)))
            (if left (lp left)))
          (let ((end (iset-end is)))
            (if (> n end)
                (let ((right (iset-right is)))
                  (if right (lp right)))
                (%iset-delete1! is n)))))))

(define (iset-delete! iset . args)
  (for-each (lambda (i) (iset-delete1! iset i)) args)
  iset)

(define (iset-delete iset . args)
  (apply iset-delete! (iset-copy iset) args))

(define (iset-map proc iset)
  (iset-fold (lambda (i is) (iset-adjoin! is (proc i))) (make-iset) iset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level set operations.
;;
;; Union is optimized to work at the node level.  Intersection and
;; difference iterate over individual elements and so have a lot of
;; room for improvement, at the expense of the complexity of
;; iset-adjoin-node!.

(define (iset-union2! a b)
  (iset-for-each-node
   (lambda (is)
     (iset-adjoin-node! a is))
   b))

(define (iset-union! . args)
  (let* ((a (and (pair? args) (car args)))
         (b (and (pair? args) (pair? (cdr args)) (cadr args))))
    (cond
     (b
      (iset-union2! a b)
      (apply iset-union! a (cddr args)))
     (a a)
     (else (make-iset)))))

(define (iset-union . args)
  (if (null? args)
    (make-iset)
    (apply iset-union! (iset-copy (car args)) (cdr args))))

(define (iset-intersection! a . args)
  (let ((b (and (pair? args) (car args))))
    (cond
     (b
      (iset-for-each
       (lambda (i) (if (not (iset-contains? b i)) (iset-delete1! a i)))
       a)
      (apply iset-intersection! a (cdr args)))
     (else a))))

(define (iset-intersection a . args)
  (apply iset-intersection! (iset-copy a) args))

(define (iset-difference! a . args)
  (if (null? args)
      a
      (begin
        (iset-for-each (lambda (i) (iset-delete1! a i)) (car args))
        (apply iset-difference! a (cdr args)))))

(define (iset-difference a . args)
  (apply iset-difference! (iset-copy a) args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursors

(define (iset-empty? iset)
  (and (iset? iset)
       (cond ((iset-bits iset) => zero?) (else #f))
       (let ((l (iset-left iset))) (or (not l) (iset-empty? l)))
       (let ((r (iset-right iset))) (or (not r) (iset-empty? r)))))

(define-record-type Iset-Cursor
  (make-iset-cursor node pos stack)
  iset-cursor?
  (node iset-cursor-node iset-cursor-node-set!)
  (pos iset-cursor-pos iset-cursor-pos-set!)
  (stack iset-cursor-stack iset-cursor-stack-set!))

;; Create a new iset cursor pointing to the first element of iset,
;; with an optional stack argument.
(define (%iset-cursor iset . o)
  (iset-cursor-advance
   (make-iset-cursor iset
                     (or (iset-bits iset) (iset-start iset))
                     (if (pair? o) (car o) '()))))

(define (iset-cursor iset . o)
  (let ((stack (if (pair? o) (car o) '())))
    (if (iset-left iset)
        (iset-cursor (iset-left iset) (cons iset stack))
        (%iset-cursor iset stack))))

;; Continue to the next node in the search stack.
(define (iset-cursor-pop cur)
  (let ((node (iset-cursor-node cur))
        (stack (iset-cursor-stack cur)))
    (cond
     ((iset-right node)
      (iset-cursor (iset-right node) stack))
     ((pair? stack)
      (%iset-cursor (car stack) (cdr stack)))
     (else
      cur))))

;; Advance to the next node+pos that can be referenced if at the end
;; of this node's range.
(define (iset-cursor-advance cur)
  (let ((node (iset-cursor-node cur))
        (pos (iset-cursor-pos cur)))
    (cond
     ((if (iset-bits node) (zero? pos) (> pos (iset-end node)))
      (iset-cursor-pop cur))
     (else cur))))

(define (iset-cursor-next iset cur)
  (iset-cursor-advance
   (let ((node (iset-cursor-node cur))
         (pos (iset-cursor-pos cur))
         (stack (iset-cursor-stack cur)))
     (let ((pos (if (iset-bits node) (bitwise-and pos (- pos 1)) (+ pos 1))))
       (make-iset-cursor node pos stack)))))

(define (iset-ref iset cur)
  (let ((node (iset-cursor-node cur))
        (pos (iset-cursor-pos cur)))
    (cond
     ((iset-bits node)
      (if (zero? pos)
          (error "cursor reference past end of iset")
          (+ (iset-start node)
             (integer-length (- pos (bitwise-and pos (- pos 1))))
             -1)))
     (else
      (if (> pos (iset-end node))
          (error "cursor reference past end of iset")
          pos)))))

(define (end-of-iset? cur)
  (let ((node (iset-cursor-node cur)))
    (and (if (iset-bits node)
             (zero? (iset-cursor-pos cur))
             (> (iset-cursor-pos cur) (iset-end node)))
         (not (iset-right node))
         (null? (iset-cursor-stack cur)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equality

(define (iset2= is1 is2)
  (let lp ((cur1 (iset-cursor is1))
           (cur2 (iset-cursor is2)))
    (cond ((end-of-iset? cur1) (end-of-iset? cur2))
          ((end-of-iset? cur2) #f)
          ((= (iset-ref is1 cur1) (iset-ref is2 cur2))
           (lp (iset-cursor-next is1 cur1) (iset-cursor-next is2 cur2)))
          (else
           #f))))

(define (iset2<= is1 is2)
  (let lp ((cur1 (iset-cursor is1))
           (cur2 (iset-cursor is2)))
    (cond ((end-of-iset? cur1))
          ((end-of-iset? cur2) #f)
          (else
           (let ((i1 (iset-ref is1 cur1))
                 (i2 (iset-ref is1 cur2)))
             (cond ((> i1 i2)
                    (lp cur1 (iset-cursor-next is2 cur2)))
                   ((= i1 i2)
                    (lp (iset-cursor-next is1 cur1)
                        (iset-cursor-next is2 cur2)))
                   (else
                    ;; (< i1 i2) - i1 won't occur in is2
                    #f)))))))

(define (iset= . o)
  (or (null? o)
      (let lp ((a (car o)) (ls (cdr o)))
        (or (null? ls) (and (iset2= a (car ls)) (lp (car ls) (cdr ls)))))))

(define (iset<= . o)
  (or (null? o)
      (let lp ((a (car o)) (ls (cdr o)))
        (or (null? ls) (and (iset2<= a (car ls)) (lp (car ls) (cdr ls)))))))

(define (iset>= . o)
  (apply iset<= (reverse o)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding

(define (iset-fold-node kons knil iset)
  (let lp ((is iset) (acc knil))
    (let* ((left (iset-left is))
           (acc (kons is (if left (lp left acc) acc)))
           (right (iset-right is)))
      (if right (lp right acc) acc))))

(define (iset-fold kons knil iset)
  (iset-fold-node
   (lambda (is acc)
     (let ((start (iset-start is))
           (end (iset-end is))
           (bits (iset-bits is)))
       (if bits
           (let ((limit (+ 1 (- end start))))
             (do ((n1 bits n2)
                  (n2 (bitwise-and bits (- bits 1)) (bitwise-and n2 (- n2 1)))
                  (acc acc (kons (+ start (integer-length (- n1 n2)) -1) acc)))
                 ((zero? n1) acc)))
           (do ((i start (+ i 1))
                (acc acc (kons i acc)))
               ((> i end) acc)))))
   knil
   iset))

(define (iset-for-each-node proc iset)
  (iset-fold-node (lambda (node acc) (proc node)) #f iset))

(define (iset-for-each proc iset)
  (iset-fold (lambda (i acc) (proc i)) #f iset))

(define (iset->list iset)
  (reverse (iset-fold cons '() iset)))

(define (iset-size iset)
  (iset-fold-node
   (lambda (is acc)
     (let ((bits (iset-bits is)))
       (+ acc (if bits
                  (bit-count bits)
                  (+ 1 (- (iset-end is) (iset-start is)))))))
   0
   iset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optimizing Iset Representation

(define (iset-balance iset)
  (and iset
       (let ((nodes '()))
         (iset-for-each-node
          (lambda (is) (set! nodes (cons (iset-copy-node is) nodes)))
          iset)
         (let reduce ((nodes (reverse nodes)))
           (let ((len (length nodes)))
             (case len
               ((0) #f)
               ((1) (car nodes))
               (else
                (let ((mid (quotient len 2)))
                  (let lp ((i 0) (ls nodes) (left '()))
                    (if (= i mid)
                        (let ((res (car ls)))
                          (iset-left-set! res (reduce (reverse left)))
                          (iset-right-set! res (reduce (cdr ls)))
                          res)
                        (lp (+ i 1) (cdr ls) (cons (car ls) left))))))))))))

(define (iset-balance! iset)
  (iset-balance iset))

;; remove leading 0's in bits before squashing
(define (iset-trim-and-squash-bits! is)
  (if (iset-bits is)
      (let ((end (iset-end is)))
        (let lp ((bits (iset-bits is))
                 (start (iset-start is)))
          (cond
           ((zero? bits)
            (iset-start-set! is start)
            (iset-bits-set! is 0))
           ((>= start end)
            (iset-start-set! is start)
            (iset-bits-set! is #f)
            (if (even? (arithmetic-shift bits -1))
                (iset-end-set! is start)))
           ((even? bits)
            (lp (arithmetic-shift bits -1) (+ start 1)))
           (else
            (iset-start-set! is start)
            (iset-bits-set! is bits))))))
  (iset-squash-bits! is)
  is)

;; overwrite a node in place
(define (iset-set-node! a b)
  (iset-start-set! a (iset-start b))
  (iset-end-set! a (iset-end b))
  (iset-bits-set! a (iset-bits b)))

;; safe to insert left since we've already visited all left nodes
(define (iset-node-replace! is nodes)
  (cond
   ((pair? nodes)
    (iset-set-node! is (car nodes))
    (let loop ((is is) (ls (cdr nodes)))
      (cond
       ((pair? ls)
        (iset-insert-left! is (car ls))
        (loop (iset-left is) (cdr ls))))))))

;; compact a list of consecutive bit ranges for an iset
(define (iset-node-split-ranges! is ranges)
  (let ((start (iset-start is))
        (end (iset-end is))
        (bits (iset-bits is)))
    (let lp ((ls (reverse ranges)) (nodes '()) (last 0))
      (if (pair? ls)
          (let ((lo (caar ls)) (hi (cdar ls)))
            (lp (cdr ls)
                (cons (make-iset (+ start lo) (+ start hi -1))
                      (if (< last lo) ;; trailing bit range
                          (cons (iset-trim-and-squash-bits!
                                 (%make-iset
                                  (+ start last)
                                  (+ start lo -1)
                                  (extract-bit-field (- lo last) last bits)
                                  #f
                                  #f))
                                nodes)
                          nodes))
                hi))
          (let ((nodes
                 (if (< (+ start last) end) ;; trailing bit range
                     (cons (iset-trim-and-squash-bits!
                            (%make-iset (+ start last)
                                        end
                                        (arithmetic-shift bits (- last))
                                        #f
                                        #f))
                           nodes)
                     nodes)))
            (iset-node-replace! is nodes))))))

;; Compact bit ranges of long consecutive chars in a single node into
;; ranges.  Loop over the bits, and convert any consecutive bit
;; patterns longer than span into new start/end nodes.
(define (iset-optimize-node! is span)
  (iset-squash-bits! is)
  (let* ((bits (iset-bits is))
         (len (and bits (integer-length bits))))
    (cond
     (bits
      (letrec
          ((full  ;; in a full bit range from [since..i)
            (lambda (i since ranges)
              (cond
               ((or (>= i len) (not (bit-set? i bits)))
                ;; if the current span is long enough, push to ranges
                (if (>= (- i since) span)
                    (sparse (+ i 1) (cons (cons since i) ranges))
                    (sparse (+ i 1) ranges)))
               (else
                (full (+ i 1) since ranges)))))
           (sparse  ;; [i-1] is not set
            (lambda (i ranges)
              (cond
               ((>= i len)
                ;; done - if there are any ranges to compact, do so
                (if (pair? ranges)
                    (iset-node-split-ranges! is ranges)))
               ((bit-set? i bits)
                (full (+ i 1) i ranges))
               (else
                (sparse (+ i 1) ranges))))))
        (sparse 0 '()))))))

;; Remove empty nodes.
(define (%iset-prune! is)
  (cond
   ((not is)
    #f)
   (else
    (iset-left-set! is (%iset-prune! (iset-left is)))
    (iset-right-set! is (%iset-prune! (iset-right is)))
    (if (and (eq? 0 (iset-bits is))
             (not (iset-left is))
             (not (iset-right is)))
        #f
        is))))

(define (iset-prune! is)
  (or (%iset-prune! is) (iset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (iset-optimize! is . opt)
  (let ((span (if (pair? opt) (car opt) (* 40 8)))
        (is (iset-prune! is)))
    (iset-for-each-node (lambda (node) (iset-optimize-node! node span)) is)
    (iset-prune! is)))

(define (iset-optimize iset . opt)
  (apply iset-optimize! (iset-copy iset) opt))

;; write an efficient expression which evaluates to the iset
(define (iset->code iset)
  (and iset
       `(%make-iset ,(iset-start iset)
                    ,(iset-end iset)
                    ,(iset-bits iset)
                    ,(iset->code (iset-left iset))
                    ,(iset->code (iset-right iset)))))
