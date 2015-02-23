;;!!! Integer Sets
;; .author Alex Shinn, 2004-2006
;; .author Alvaro Castro-Castilla
;; Copyright (c) 2004-2006 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; An integer set is a set of exact integers optimized for minimal space
;; usage and fast membership lookup.  General set operations are
;; provided based on the character set operations found in SRFI-14.
;;
;; Creating isets:
;;
;; (make-iset)     ; an empty integer set
;; (make-iset n)   ; a set of the single integer N
;; (make-iset n m) ; a set of the range of all integers from N-M inclusive
;;
;; The following procedures are provided as direct analogs of the
;; SRFI-14 procedures, accepting and returning isets and integers in
;; place of char-sets and characters:
;;
;; Creating isets:
;;
;; (iset-copy is)            ; a new copy of IS
;; (iset n ...)              ; an iset containing the elements N...
;; (list->iset ls [base-is]) ; an iset containing all the integers in
;;                           ; list LS, union BASE-IS if provided
;; (list->iset! ls base-is)  ; same as above, allowed but not required to
;;                           ; modify base-is
;;
;; Querying isets:
;;
;; (iset-size is)          ; return the # of elements in IS
;; (iset-contains? is n)   ; test N for membership in IS
;; (iset->list is)         ; returns a list of all integers in IS
;; (iset-any pred is)      ; apply PRED to every element of IS, returning
;;                         ; the first element it finds (order unspecified)
;; (iset-every pred is)    ; returns #t if every element of IS satisfies
;;                         ; the predicate PRED (order unspecified)
;;
;; Predicates:
;;
;; (iset? obj)     ; #t iff obj is an integer set
;; (iset= is ...)  ; #t iff all arguments are equivalent integer sets
;; (iset<= is ...) ; #t iff the arguments are monotonically increasing sets
;; (iset>= is ...) ; #t iff the arguments are monotonically decreasing sets
;;
;; Iteration:
;;
;; (iset-fold kons knil is)       ; char-set-fold
;; (iset-unfold f p g [base-is])  ; char-set-unfold
;; (iset-unfold! f p g base-is)   ; char-set-unfold!
;; (iset-for-each proc is)        ; char-set-for-each
;; (iset-map proc is)             ; char-set-for-each
;; (iset-filter pred is [bas-is]) ; char-set-filter
;; (iset-filter! pred is base-is) ; char-set-filter!
;;
;; Cursors:
;;
;; (iset-cursor iset)
;; (iset-ref iset cursor)
;; (iset-cursor-next iset cursor)
;; (end-of-iset? iset)
;;
;; Set operations:
;;
;; (iset-adjoin is n ...)         ; char-set-adjoin
;; (iset-delete is n ...)         ; char-set-delete
;;
;; (iset-adjoin! is n ...)        ; char-set-adjoin!
;; (iset-delete! is n ...)        ; char-set-delete!
;;
;; (iset-union is1 ...)                  ; char-set-union
;; (iset-intersection is1 ...)           ; char-set-intersection
;; (iset-difference is1 is2 ...)         ; char-set-difference
;; (iset-xor is1 ...)                    ; char-set-xor
;; (iset-diff+intersection is1 is2 ...)  ; char-set-diff+intersection
;;
;; (iset-union! is1 ...)                 ; char-set-union!
;; (iset-intersection! is1 ...)          ; char-set-intersection!
;; (iset-difference! is1 is2 ...)        ; char-set-difference!
;; (iset-xor! is1 ...)                   ; char-set-xor!
;; (iset-diff+intersection! is1 is2 ...) ; char-set-diff+intersection!


(cond-expand
 (gambit
  (declare (fixnum)))
 (else (void)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;!! Integer Sets

(define-record-type <iset>
  (%make-iset start end bits left right)
  iset?
  (start iset-start set-iset-start!)
  (end   iset-end   set-iset-end!)
  (bits  iset-bits  set-iset-bits!)
  (left  iset-left  set-iset-left!)
  (right iset-right set-iset-right!))

(define *bits-thresh* 128)  ; within 128 we join into a bit-vector
(define *bits-max* 512)     ; don't make bit-vectors larger than this

(define* (make-iset (start 0)
                    (end start)
                    (bits (make-bit-vector 1 #f)))
  (%make-iset start end bits #f #f))

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
        (and-let* ((bits (iset-bits iset))) (bit-vector-copy bits))
        (iset-copy (iset-left iset))
        (iset-copy (iset-right iset)))))

(define (iset-copy-node iset)
  (%make-iset (iset-start iset) (iset-end iset) (iset-bits iset) #f #f))

(define (iset-set-node! a b)
  (set-iset-start! a (iset-start b))
  (set-iset-end! a (iset-end b))
  (set-iset-bits! a (iset-bits b)))

;; (define (iset-swap-nodes! a b)
;;   (let ((tmp (iset-copy-node a)))
;;     (iset-set-node! a b)
;;     (iset-set-node! b tmp)))

(define (iset2<= a b)
  (iset-every (lambda (i) (iset-contains? b i)) a))

(define (iset<= . args)
  (or (null? args)
      (let ((rest (cdr args)))
        (or (null? rest)
            (and (iset2<= (car args) (car rest))
                 (apply iset<= rest))))))

(define (iset>= . args)
  (apply iset<= (reverse args)))

(define (iset= . args)
  (and (apply iset<= args)
       (apply iset>= args)))

(define (iset-empty? iset)
  (and (iset? iset)
       (cond ((iset-bits iset) => bit-vector-empty?) (else #f))
       (let ((l (iset-left iset))) (or (not l) (iset-empty? l)))
       (let ((r (iset-right iset))) (or (not r) (iset-empty? r)))))

(define (iset-contains? iset n)
  (let lp ((is iset))
    (let ((start (iset-start is)))
      (if (< n start)
          (and-let* ((left (iset-left is))) (lp left))
          (let ((end (iset-end is)))
            (if (> n end)
                (and-let* ((right (iset-right is))) (lp right))
                (let ((bits (iset-bits is)))
                  (or (not bits)
                      (bit-vector-ref bits (- n start))))))))))

(define (iset-max-end iset)
  (cond ((iset-right iset) => iset-max-end)
        (else (iset-end iset))))

(define (iset-min-start iset)
  (cond ((iset-left iset) => iset-min-start)
        (else (iset-start iset))))

(define (iset-insert-left! iset new)
  (let ((left (iset-left iset)))
    (if (and left (< (iset-end new) (iset-start left)))
        (set-iset-right! new left)
        (set-iset-left! new left)))
  (set-iset-left! iset new))

(define (iset-insert-right! iset new)
  (let ((right (iset-right iset)))
    (if (and right (< (iset-end new) (iset-start right)))
        (set-iset-right! new right)
        (set-iset-left! new right)))
  (set-iset-right! iset new))

(define (iset-squash-bits! iset)
  (and-let* ((bits (iset-bits iset)))
            (if (bit-vector-full? bits (- (iset-end iset) (iset-start iset)))
                (set-iset-bits! iset #f))))

(define (iset-adjoin1! iset n)
  (cond
   ((iset-empty? iset)
    (set-iset-start! iset n)
    (set-iset-end! iset n)
    (set-iset-bits! iset #f))
   (else
    (let ((start (iset-start iset))
          (end (iset-end iset))
          (bits (iset-bits iset)))
      (cond
       ((< n start)
        (let ((s-diff (- start n)))
          (if (and-let* ((left (iset-left iset))
                         (m-end (iset-max-end left)))
                        (or (< n m-end)
                            (< (- n m-end) s-diff)))
              (iset-adjoin1! (iset-left iset) n)
              (cond
               ((and (< s-diff *bits-thresh*)
                     (< (- end n) *bits-max*))
                (set-iset-start! iset n)
                (let ((bits2 (bit-vector-shift
                              (or bits (range->bit-vector start end))
                              s-diff)))
                  (bit-vector-set! bits2 0 #t)
                  (set-iset-bits! iset bits2)
                  (iset-squash-bits! iset)))
               (else (iset-insert-left! iset (make-iset n)))))))
       ((> n end)
        (let ((e-diff (- n end)))
          (if (and-let* ((right (iset-right iset))
                         (m-start (iset-min-start right)))
                        (or (> n m-start)
                            (> (- n m-start) e-diff)))
              (iset-adjoin1! (iset-right iset) n)
              (cond
               ((and (< e-diff *bits-thresh*)
                     (< (- n start) *bits-max*))
                (set-iset-end! iset n)
                (set-iset-bits! iset (bit-vector-set (or bits (range->bit-vector start end))
                                                     (- n start)
                                                     #t))
                (iset-squash-bits! iset))
               (else (iset-insert-right! iset (make-iset n)))))))
       (bits
        (bit-vector-set! bits (- n start) #t)
        (iset-squash-bits! iset)))))))

(define (iset-adjoin-node! a b)
  (cond
   ((iset-empty? a)
    (set-iset-start! a (iset-start b))
    (set-iset-end! a (iset-end b))
    (set-iset-bits! a (iset-bits b)))
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
       ((< b-end a-start)
        (let ((near-diff (- a-start b-end))
              (start-diff (- a-start b-start))
              (far-diff (- a-end b-start)))
          (if (and-let* ((left (iset-left a))
                         (m-end (iset-max-end left)))
                        (or (< b-end m-end)
                            (< (- b-end m-end) near-diff)))
              (iset-adjoin-node! (iset-left a) b)
              (cond
               ((and (< near-diff *bits-thresh*)
                     (< far-diff *bits-max*))
                (set-iset-start! a b-start)
                (let ((bits (bit-vector-shift
                             (or a-bits (range->bit-vector a-start a-end))
                             start-diff))
                      (lo-bits (or b-bits (range->bit-vector b-start b-end))))
                  (set-iset-bits! a (bit-vector-ior! bits lo-bits))
                  (iset-squash-bits! a)))
               (else (iset-insert-left! a (iset-copy-node b)))))))
       ;; ...aaaa
       ;;         bbbb...
       ((> b-start a-end)
        (let ((near-diff (- b-start a-end))
              (start-diff (- b-start a-start))
              (far-diff (- b-end a-start)))
          (if (and-let* ((right (iset-right a))
                         (m-start (iset-min-start right)))
                        (or (> b-start m-start)
                            (> (- b-start m-start) near-diff)))
              (iset-adjoin-node! (iset-right a) b)
              (cond
               ((and (< near-diff *bits-thresh*)
                     (< far-diff *bits-max*))
                (set-iset-end! a b-end)
                (set-iset-bits!
                 a
                 (bit-vector-ior!
                  (or a-bits (range->bit-vector a-start a-end))
                  (bit-vector-shift
                   (or b-bits (range->bit-vector b-start b-end))
                   start-diff)))
                (iset-squash-bits! a))
               (else (iset-insert-right! a (iset-copy-node b)))))))
       ;; aaaa...
       ;;   bbbb...
       ((> b-start a-start)
        (set-iset-end! a (max a-end b-end))
        (when (or a-bits b-bits)
              (set-iset-bits!
               a
               (bit-vector-ior!
                (or a-bits (range->bit-vector a-start a-end))
                (bit-vector-shift
                 (or b-bits (range->bit-vector b-start b-end))
                 (- b-start a-start))))
              (iset-squash-bits! a)))
       ;;   aaaa...
       ;; bbbb...
       ((< b-start a-start)
        (set-iset-start! a b-start)
        (set-iset-end! a (max a-end b-end))
        (when (or a-bits b-bits)
              (set-iset-bits!
               a
               (bit-vector-ior!
                (bit-vector-shift
                 (or a-bits (range->bit-vector a-start a-end))
                 (- a-start b-start))
                (or b-bits (range->bit-vector b-start b-end))))
              (iset-squash-bits! a)))
       ;; aaaa...
       ;; bbbb...
       (else
        (set-iset-end! a (max a-end b-end))
        (when (or a-bits b-bits)
              (set-iset-bits!
               a
               (bit-vector-ior!
                (or a-bits (range->bit-vector a-start a-end))
                (or b-bits (range->bit-vector b-start b-end))))
              (iset-squash-bits! a))))))))

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
      (bit-vector-set! bits (- n start) #f))
     ((= n start)
      (if (= n end)
          (set-iset-bits! iset '#u8())
          (set-iset-start! iset (+ n 1))))
     ((= n end)
      (set-iset-end! iset (- n 1)))
     (else
      (set-iset-end! iset (- n 1))
      (iset-insert-right! iset (make-iset (+ n 1) end))))))

(define (iset-delete1! iset n)
  (let lp ((is iset))
    (let ((start (iset-start is)))
      (if (< n start)
          (and-let* ((left (iset-left is)))
                    (lp left))
          (let ((end (iset-end is)))
            (if (> n end)
                (and-let* ((right (iset-right is)))
                          (lp right))
                (%iset-delete1! is n)))))))

(define (iset-delete! iset . args)
  (for-each (lambda (i) (iset-delete1! iset i)) args)
  iset)

(define (iset-delete iset . args)
  (apply iset-delete! (iset-copy iset) args))


;;-------------------------------------------------------------------------------
;;!! Iteration

(define (iset-fold-node kons knil iset)
  (let lp ((is iset) (acc knil))
    (let* ((acc2 (cond ((iset-left is) => (lambda (left) (lp left acc)))
                       (else acc)))
           (acc3 (kons is acc2)))
      (cond ((iset-right is) => (lambda (right) (lp right acc3)))
            (else acc3)))))

(define (iset-fold kons knil iset)
  (iset-fold-node
   (lambda (is acc)
     (let ((start (iset-start is))
           (end (iset-end is))
           (bits (iset-bits is)))
       (if bits
           (let ((limit (+ 1 (- end start))))
             (let lp ((i 0) (acc acc))
               (if (= i limit)
                   acc
                   (lp (+ i 1)
                       (if (bit-vector-ref bits i)
                           (kons (+ i start) acc)
                           acc)))))
           (let lp ((i start) (acc acc))
             (if (> i end)
                 acc
                 (lp (+ i 1) (kons i acc)))))))
   knil iset))

(define (iset-unfold f p g seed . opt)
  (let ((base-is (if (pair? opt) (iset-copy (car opt)) (make-iset))))
    (iset-unfold! f p g seed base-is)))

(define (iset-unfold! f p g seed base-is)
  (let lp ((seed seed))
    (if (p seed)
      base-is
      (begin
        (iset-adjoin1! base-is (f seed))
        (lp (g seed))))))

(define (iset-for-each-node proc iset)
  (let lp ((is iset))
    (and-let* ((left (iset-left is)))
              (lp left))
    (proc is)
    (and-let* ((right (iset-right is)))
              (lp right))))

(define (iset-for-each proc iset)
  (iset-for-each-node
   (lambda (is)
     (let ((start (iset-start is))
           (end (iset-end is))
           (bits (iset-bits is)))
       (if bits
           (let ((limit (+ 1 (- end start))))
             (do ((i 0 (+ i 1)))
                 ((= i limit))
               (if (bit-vector-ref bits i)
                   (proc (+ i start)))))
           (do ((i start (+ i 1)))
               ((> i end))
             (proc i)))))
   iset))

(define (iset-map proc iset)
  (let ((res (make-iset)))
    (iset-for-each (lambda (i) (iset-adjoin1! res (proc i))) iset)
    res))

(define (iset-filter! proc iset base-is)
  (iset-for-each (lambda (i) (if (proc i) (iset-adjoin1! base-is i))) iset)
  base-is)

(define (iset-filter proc iset . opt)
  (iset-filter! proc iset (if (pair? opt) (iset-copy (car opt)) (make-iset))))

(define (iset->list iset)
  (let ((res '()))
    (iset-for-each (lambda (i) (set! res (cons i res))) iset)
    (reverse res)))

(define (iset-any proc iset)
  (call-with-current-continuation
   (lambda (return)
     (iset-for-each (lambda (i) (cond ((proc i) => return))) iset))))

(define (iset-every proc iset)
  (call-with-current-continuation
   (lambda (return)
     (iset-for-each (lambda (i) (if (not (proc i)) (return #f))) iset)
     #t)))

(define (iset-size iset)
  (iset-fold-node
   (lambda (is acc)
     (let ((start (iset-start is))
           (end (iset-end is))
           (bits (iset-bits is)))
       (+ acc
          (if bits
              (bit-vector-count bits)
              (+ 1 (- end start))))))
   0 iset))

(define (iset-cursor iset)
  (call-with-current-continuation
   (lambda (return)
     (iset-for-each
      (lambda (i)
        (call-with-current-continuation
         (lambda (inside)
           (return (lambda (command)
                     (case command
                       ((ref) i)
                       ((next) (inside #t))
                       ((end?) #f)))))))
      iset)
     (lambda (command)
       (case command
         ((ref next) (error "past end of cursor"))
         ((end?) #t))))))

(define (iset-ref iset cur) (cur 'ref))

(define (iset-cursor-next iset cur) (cur 'next))

(define (end-of-iset? cur) (cur 'end?))


;; these could be _much_ better optimized

(define (iset-union2! a b)
  (iset-for-each-node
   (lambda (is)
     (iset-adjoin-node! a is))
   b))

(define* (iset-union! (a #f) (b #f) . rest)
  (cond
   (b
    (iset-union2! a b)
    (apply iset-union! a rest))
   (a a)
   (else (make-iset))))

(define (iset-union . args)
  (if (null? args)
      (make-iset)
      (apply iset-union! (iset-copy (car args)) (cdr args))))

(define* (iset-intersection! a (b #f) . rest)
  (cond
   (b
    (iset-for-each
     (lambda (i) (unless (iset-contains? b i) (iset-delete1! a i)))
     a)
    (apply iset-intersection! a rest))
   (else a)))

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

(define (iset-diff+intersection! . args)
  (let ((diff (apply iset-difference args)))
    (values diff (apply iset-intersection! args))))

(define (iset-diff+intersection . args)
  (values (apply iset-difference args) (apply iset-intersection args)))

(define (iset-xor . args)
  (iset-difference! (apply iset-union args) (apply iset-intersection args)))

(define (iset-xor! . args)
  (let ((union (apply iset-union args)))
    (iset-difference! union (apply iset-intersection! args))))


;; optimizing representation

(define (iset-balance iset)
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
                     (set-iset-left! res (reduce (reverse left)))
                     (set-iset-right! res (reduce (cdr ls)))
                     res)
                   (lp (+ i 1) (cdr ls) (cons (car ls) left)))))))))))

(define (iset-balance! iset)
  (iset-balance iset))

;; safe to insert left since we've already visited all left ndoes
(define (iset-node-replace! is nodes)
  (when (pair? nodes)
        (iset-set-node! is (car nodes))
        (let loop ((is is) (ls (cdr nodes)))
          (when (pair? ls)
                (iset-insert-left! is (car ls))
                (loop (iset-left is) (cdr ls))))))

(define (iset-node-split-ranges! is ranges)
  (let ((start (iset-start is))
        (end (iset-end is))
        (bits (iset-bits is)))
    (let loop ((ls (reverse ranges)) (nodes '()) (last 0))
      (if (pair? ls)
          (let ((lo (caar ls)) (hi (cdar ls)))
            (loop (cdr ls)
                  (cons (make-iset (+ start (* 8 lo)) (+ start (* 8 hi) -1))
                        (if (< last lo)
                            (cons (make-iset (+ start (* 8 last))
                                             (+ start (* 8 lo) -1)
                                             (subu8vector bits last lo))
                                  nodes)
                            nodes))
                  hi))
          (iset-node-replace!
           is
           (if (< (+ start (* 8 last)) end)
               (cons (make-iset (+ start (* 8 last))
                                end
                                (subu8vector bits last (u8vector-length bits)))
                     nodes)
               nodes))))))

(define (iset-optimize-node! is span)
  (iset-squash-bits! is)
  (and-let* ((bits (iset-bits is))
             (len (u8vector-length bits)))
            (letrec
                ((full
                  (lambda (i since ranges)
                    (cond
                     ((or (>= i len) (not (= 255 (u8vector-ref bits i))))
                      (if (>= (- i since) span)
                          (sparse (+ i 1) (cons (cons since i) ranges))
                          (sparse (+ i 1) ranges)))
                     (else
                      (full (+ i 1) since ranges)))))
                 (sparse
                  (lambda (i ranges)
                    (cond
                     ((>= i len)
                      (if (pair? ranges)
                          (iset-node-split-ranges! is ranges)))
                     ((= 255 (u8vector-ref bits i))
                      (full (+ i 1) i ranges))
                     (else
                      (sparse (+ i 1) ranges))))))
              (sparse 0 '()))))

;; Optimize consecutive runs of 255 into ranges.  a single ranged node
;; takes up 5 slots (20 bytes on a 32-bit machine, 40 on 64-bit), so it
;; is more space efficient to switch to a range at this point.  Ranges
;; are faster because there is no need to perform bit-vector tests,
;; however they introduce more nodes which could require a longer
;; traversal.

(define (iset-optimize! iset . opt)
  (let ((span (if (pair? opt) (car opt) 20)))
    (iset-for-each-node (lambda (is) (iset-optimize-node! is span)) iset)
    iset))

(define (iset-optimize iset . opt)
  (apply iset-optimize! (iset-copy iset) opt))

;; ;; write an efficient expression which evaluates to the iset
;; (define* (iset-write-code is (pretty? #f) (p (current-output-port)))
;;   (let ((acc (if pretty?
;;                  (lambda (x) (string-append x "    "))
;;                  (lambda (x) x)))
;;         (sep (if pretty? "\n" " ")))
;;     (let loop ((is is) (indent ""))
;;       (if is
;;           (begin
;;             (fprintf p "~A(%make-iset ~A ~A" indent
;;                      (iset-start is) (iset-end is))
;;             (cond ((iset-bits is) => (lambda (bits) (fprintf p " '~A" bits)))
;;                   (else (display " #f" p)))
;;             (cond
;;              ((or (iset-left is) (iset-right is))
;;               (display sep p)
;;               (loop (iset-left is) (acc indent))
;;               (display sep p)
;;               (loop (iset-right is) (acc indent)))
;;              (else
;;               (display " #f #f")))
;;             (display ")" p))
;;           (fprintf p "~A#f" indent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debugging aids

;; (define (iset-dump iset)
;;   (with-output-to-string
;;     (lambda ()
;;       (display "<iset:")
;;       (iset-for-each-node
;;        (lambda (is)
;;          (let ((start (iset-start is))
;;                (end (iset-end is))
;;                (bits (iset-bits is)))
;;            (cond
;;              (bits (printf " [~A-~A: ~S]" start end bits))
;;              ((> end start) (printf " [~A-~A]" start end))
;;              (else (printf " ~A" start)))))
;;        iset)
;;       (display ">"))))

;; (define (iset-compact? iset)
;;   (call-with-current-continuation
;;    (lambda (return)
;;      (let ((p-start #f)
;;            (p-end #f))
;;        (iset-for-each-node
;;         (lambda (is)
;;           (let ((start (iset-start iset))
;;                 (end (iset-end iset)))
;;             (if (and p-start
;;                      (> (- start p-end) *bits-thresh*)
;;                      (< (- end p-start) *bits-max*))
;;               (return #f))
;;             (set! p-start start)
;;             (set! p-end end)))
;;         iset))
;;      #t)))
