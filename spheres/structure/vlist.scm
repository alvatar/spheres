;;!!! This module provides an implementations of vlists, a functional list-like
;; data structure described by Phil Bagwell in "Fast Functional Lists,
;; Hash-Lists, Dequeues and Variable-Length Arrays", EPFL Technical Report,
;; 2002.
;; .author Ludovic Courtès, 2009-2012
;; .author Ivan Raikov, 2012
;; .author Alvaro Castro-Castilla, 2015
;; .license LGPL
;;
;; The idea is to store vlist elements in increasingly large contiguous blocks
;; (implemented as vectors here).  These blocks are linked to one another using
;; a pointer to the next block (called `block-base' here) and an offset within
;; that block (`block-offset' here).  The size of these blocks form a geometric
;; series with ratio `block-growth-factor'.
;;
;; In the best case (e.g., using a vlist returned by `list->vlist'),
;; elements from the first half of an N-element vlist are accessed in O(1)
;; (assuming `block-growth-factor' is 2), and `vlist-length' takes only
;; O(ln(N)).  Furthermore, the data structure improves data locality since
;; vlist elements are adjacent, which plays well with caches.


(define block-growth-factor (make-parameter 2))

(define (make-block base offset size hash-tab?)
  ;; Return a block (and block descriptor) of SIZE elements pointing to BASE
  ;; at OFFSET.  If HASH-TAB? is true, a "hash table" is also added.
  ;; We could improve locality here by having a single vector but currently
  ;; the extra arithmetic outweighs the benefits (!).
  (vector (make-vector size)
          base offset size 0
          (and hash-tab? (make-vector size #f))))


(define-syntax vlist%define-block-accessor
  (syntax-rules ()
    ((_ name index)
     (define (name block)
       (vector-ref block index)))))

(vlist%define-block-accessor block-content 0)
(vlist%define-block-accessor block-base 1)
(vlist%define-block-accessor block-offset 2)
(vlist%define-block-accessor block-size 3)
(vlist%define-block-accessor block-next-free 4)
(vlist%define-block-accessor block-hash-table 5)

(define (increment-block-next-free! block)
  (vector-set! block 4
               (+ (block-next-free block) 1)))

(define (block-append! block value)
  ;; This is not thread-safe.  To fix it, see Section 2.8 of the paper.
  (let ((offset (block-next-free block)))
    (increment-block-next-free! block)
    (vector-set! (block-content block) offset value)
    #t))

(define (block-ref block offset)
  (vector-ref (block-content block) offset))

(define (block-ref* block offset)
  (let ((v (block-ref block offset)))
    (if (block-hash-table block)
        (car v) ;; hide the vhash link
        v)))

(define (block-hash-table-ref block offset)
  (vector-ref (block-hash-table block) offset))

(define (block-hash-table-set! block offset value)
  (vector-set! (block-hash-table block) offset value))

(define block-null
  ;; The null block.
  (make-block #f 0 0 #f))


;;-------------------------------------------------------------------------------
;;!! Vlist

(define-record-type <vlist>
  ;; A vlist is just a base+offset pair pointing to a block.
  ;; Allocating a <vlist> record in addition to the block at each
  ;; `vlist-cons' call is inefficient.  However, Bagwell's hack to avoid it
  ;; (Section 2.2) would require GC_ALL_INTERIOR_POINTERS, which would be a
  ;; performance hit for everyone.
  (make-vlist base offset)
  vlist?
  (base vlist-base)
  (offset  vlist-offset))

(define vlist-null
  ;; The empty vlist.
  (make-vlist block-null 0))

(define (block-cons item vlist hash-tab?)
  (let loop ((base   (vlist-base vlist))
             (offset (+ 1 (vlist-offset vlist))))
    (if (and (< offset (block-size base))
             (= offset (block-next-free base))
             (block-append! base item))
        (make-vlist base offset)
        (let ((size (cond ((eq? base block-null) 1)
                          ((< offset (block-size base))
                           ;; new vlist head
                           1)
                          (else
                           (* (block-growth-factor)
                              (block-size base))))))
          ;; Prepend a new block pointing to BASE.
          (loop (make-block base (- offset 1) size hash-tab?)
                0)))))

;;! Return a new vlist with {item} as its head and {vlist} as its tail
(define (vlist-cons item vlist)
  ;; Note: Calling `vlist-cons' on a vhash will not do the right thing: it
  ;; doesn't box ITEM so that it can have the hidden "next" link used by
  ;; vhash items, and it passes `#f' as the HASH-TAB? argument to
  ;; `block-cons'.  However, inserting all the checks here has an important
  ;; performance penalty, hence this choice.
  (block-cons item vlist #f))

;;! Return the head of {vlist}.
(define (vlist-head vlist)
  (let ((base   (vlist-base vlist))
        (offset (vlist-offset vlist)))
    (block-ref* base offset)))

;;! Return the tail of {vlist}.
(define (vlist-tail vlist)
  (let ((base   (vlist-base vlist))
        (offset (vlist-offset vlist)))
    (if (> offset 0)
        (make-vlist base (- offset 1))
        (make-vlist (block-base base)
                    (block-offset base)))))

;; Return true if {vlist} is empty.
(define (vlist-null? vlist)
  (let ((base (vlist-base vlist)))
    (and (not (block-base base))
         (= 0 (block-size base)))))

;;! Return a new vlist whose contents correspond to {lst}.
(define (list->vlist lst)
  (vlist-reverse (fold vlist-cons vlist-null lst)))

;;! Fold over {vlist}, calling {proc} for each element.
(define (vlist-fold proc init vlist)
  ;; FIXME: Handle multiple lists.
  (let loop ((base   (vlist-base vlist))
             (offset (vlist-offset vlist))
             (result init))
    (if (eq? base block-null)
        result
        (let* ((next  (- offset 1))
               (done? (< next 0)))
          (loop (if done? (block-base base) base)
                (if done? (block-offset base) next)
                (proc (block-ref* base offset) result))))))

;;! Fold over {vlist}, calling {proc} for each element, starting from the last element.
(define (vlist-fold-right proc init vlist)
  (define len (vlist-length vlist))
  (let loop ((index  (- len 1))
             (result init))
    (if (< index 0)
        result
        (loop (- index 1)
              (proc (vlist-ref vlist index) result)))))

;;! Return a new {vlist} whose content are those of {vlist} in reverse order.
(define (vlist-reverse vlist)
  (vlist-fold vlist-cons vlist-null vlist))

;;! Map {proc} over the elements of {vlist} and return a new vlist.
(define (vlist-map proc vlist)
  (vlist-fold (lambda (item result)
                (vlist-cons (proc item) result))
              vlist-null
              (vlist-reverse vlist)))

;;! Return a new list whose contents match those of {vlist}.
(define (vlist->list vlist)
  (vlist-fold-right cons '() vlist))

;;! Return the element at index {index} in {vlist}.
(define (vlist-ref vlist index)
  (let loop ((index   index)
             (base    (vlist-base vlist))
             (offset  (vlist-offset vlist)))
    (if (<= index offset)
        (block-ref* base (- offset index))
        (loop (- index offset 1)
              (block-base base)
              (block-offset base)))))

;;! Return a new vlist that does not contain the {count} first elements of {vlist}.
(define (vlist-drop vlist count)
  (let loop ((count  count)
             (base   (vlist-base vlist))
             (offset (vlist-offset vlist)))
    (if (<= count offset)
        (make-vlist base (- offset count))
        (loop (- count offset 1)
              (block-base base)
              (block-offset base)))))

;;! Return a new vlist that contains only the {count} first elements of {vlist}.
(define (vlist-take vlist count)
  (let loop ((count  count)
             (vlist  vlist)
             (result vlist-null))
    (if (= 0 count)
        (vlist-reverse result)
        (loop (- count 1)
              (vlist-tail vlist)
              (vlist-cons (vlist-head vlist) result)))))

;;! Return a new vlist containing all the elements from {vlist} that satisfy {pred}.
(define (vlist-filter pred vlist)
  (vlist-fold-right (lambda (e v)
                      (if (pred e)
                          (vlist-cons e v)
                          v))
                    vlist-null
                    vlist))

;;! Return a new vlist corresponding to {vlist} without the elements {equal?} to {x}.
(define (vlist-delete x vlist #!optional (equal? equal?))
  (vlist-filter (lambda (e)
                  (not (equal? e x)))
                vlist))

;;! Return the length of {vlist}.
(define (vlist-length vlist)
  (let loop ((base (vlist-base vlist))
             (len  (vlist-offset vlist)))
    (if (eq? base block-null)
        len
        (loop (block-base base)
              (+ len 1 (block-offset base))))))

;;! Return a new vlist.  See the description of SRFI-1 `unfold' for details.
(define (vlist-unfold p f g seed
                      #!optional (tail-gen (lambda (x) vlist-null)))
  (let uf ((seed seed))
    (if (p seed)
        (tail-gen seed)
        (vlist-cons (f seed)
                    (uf (g seed))))))

;;! Return a new vlist.  See the description of SRFI-1 `unfold-right' for details.
(define (vlist-unfold-right p f g seed #!optional (tail vlist-null))
  (let uf ((seed seed) (lis tail))
    (if (p seed)
        lis
        (uf (g seed) (vlist-cons (f seed) lis)))))

;;! Append the given lists.
(define (vlist-append . vlists)
  (if (null? vlists)
      vlist-null
      (fold-right (lambda (vlist result)
                    (vlist-fold-right (lambda (e v)
                                        (vlist-cons e v))
                                      result
                                      vlist))
                  vlist-null
                  vlists)))

;;! Call {proc} on each element of {vlist}.  The result is unspecified.
(define (vlist-for-each proc vlist)
  (vlist-fold (lambda (item x)
                (proc item))
              (if #f #f)
              vlist))


;;-------------------------------------------------------------------------------
;;!! Hash Lists, aka. `VHash'

;; Assume keys K1 and K2, H = hash(K1) = hash(K2), and two values V1 and V2
;; associated with K1 and K2, respectively.  The resulting layout is a
;; follows:
;;
;;     ,--------------------.
;;     | ,-> (K1 . V1) ---. |
;;     | |                | |
;;     | |   (K2 . V2) <--' |
;;     | |                  |
;;     +-|------------------+
;;     | |                  |
;;     | |                  |
;;     | `-- O <---------------H
;;     |                    |
;;     `--------------------'
;;
;; The bottom part is the "hash table" part of the vhash, as returned by
;; `block-hash-table'; the other half is the data part.  O is the offset of
;; the first value associated with a key that hashes to H in the data part.
;; The (K1 . V1) pair has a "hidden" link to the (K2 . V2) pair; hiding the
;; link is handled by `block-ref'.
;;
;; This API potentially requires users to repeat which hash function and which
;; equality predicate to use.  This can lead to unpredictable results if they
;; are used in consistenly, e.g., between `vhash-cons' and `vhash-assoc', which
;; is undesirable, as argued in http://savannah.gnu.org/bugs/?22159 .  OTOH, two
;; arguments can be made in favor of this API:
;;
;;  - It's consistent with how alists are handled in SRFI-1.
;;
;;  - In practice, users will probably consistenly use either the `q', the `v',
;;    or the plain variant (`vlist-cons' and `vlist-assoc' without any optional
;;    argument), i.e., they will rarely explicitly pass a hash function or
;;    equality predicate.

;;! Return true if {obj} is a hash list.
(define (vhash? obj)
  (and (vlist? obj)
       (let ((base (vlist-base obj)))
         (and base
              (vector? (block-hash-table base))))))

;;! Return a new hash list based on {vhash} where {key} is associated
;; with {value}.  Use {hash} to compute {key}'s hash.
(define (vhash-cons key value vhash #!optional (hash hash))
  (let* ((key+value (cons key value))
         (entry     (cons key+value #f))
         (vlist     (block-cons entry vhash #t))
         (base      (vlist-base vlist))
         (khash     (hash key (block-size base))))
    (let ((o (block-hash-table-ref base khash)))
      (if o (set-cdr! entry o)))
    (block-hash-table-set! base khash
                           (vlist-offset vlist))
    vlist))

(define vhash-consq (cut vhash-cons <> <> <> eq?-hash))
(define vhash-consv (cut vhash-cons <> <> <> eqv?-hash))

(define (%vhash-fold* proc init key vhash equal? hash)
  ;; Fold over all the values associated with KEY in VHASH.
  (define khash
    (let ((size (block-size (vlist-base vhash))))
      (and (> size 0) (hash key size))))
  (let loop ((base       (vlist-base vhash))
             (khash      khash)
             (offset     (and khash
                              (block-hash-table-ref (vlist-base vhash)
                                                    khash)))
             (max-offset (vlist-offset vhash))
             (result     init))
    (let ((answer (and offset (block-ref base offset))))
      (cond ((and (pair? answer)
                  (<= offset max-offset)
                  (let ((answer-key (caar answer)))
                    (equal? key answer-key)))
             (let ((result      (proc (cdar answer) result))
                   (next-offset (cdr answer)))
               (loop base khash next-offset max-offset result)))
            ((and (pair? answer) (cdr answer))
             =>
             (lambda (next-offset)
               (loop base khash next-offset max-offset result)))
            (else
             (let ((next-base (block-base base)))
               (if (and next-base (> (block-size next-base) 0))
                   (let* ((khash  (hash key (block-size next-base)))
                          (offset (block-hash-table-ref next-base khash)))
                     (loop next-base khash offset (block-offset base)
                           result))
                   result)))))))

;;! Fold over all the values associated with {key} in {vhash}, with
;; each call to {proc} having the form {(proc value result)},
;; where {result} is the result of the previous call to {proc} and
;; {init} the value of {result} for the first call to {proc}."
(define (vhash-fold* proc init key vhash
                     #!optional (equal? equal?) (hash hash))
  (%vhash-fold* proc init key vhash equal? hash))

;;! Same as {vhash-fold*}, but using {eq?-hash} and {eq?}.
(define (vhash-foldq* proc init key vhash)
  (%vhash-fold* proc init key vhash eq? eq?-hash))

;;! Same as {vhash-fold*}, but using {eqv?-hash} and {eqv?}.
(define (vhash-foldv* proc init key vhash)
  (%vhash-fold* proc init key vhash eqv? eqv?-hash))

;;! A specialization of `vhash-fold*' that stops when the first value
;; associated with KEY is found or when the end-of-list is reached.
;; Inline to make sure `vhash-assq' gets to use the `eq?' instruction
;; instead of calling the `eq?'  subr.
(define (%vhash-assoc key vhash equal? hash)
  (define khash
    (let ((size (block-size (vlist-base vhash))))
      (and (> size 0) (hash key size))))
  (let loop ((base       (vlist-base vhash))
             (khash      khash)
             (offset     (and khash
                              (block-hash-table-ref (vlist-base vhash)
                                                    khash)))
             (max-offset (vlist-offset vhash)))
    (let ((answer (and offset (block-ref base offset))))
      (cond ((and (pair? answer)
                  (<= offset max-offset)
                  (let ((answer-key (caar answer)))
                    (equal? key answer-key)))
             (car answer))
            ((and (pair? answer) (cdr answer))
             =>
             (lambda (next-offset)
               (loop base khash next-offset max-offset)))
            (else
             (let ((next-base (block-base base)))
               (and next-base
                    (> (block-size next-base) 0)
                    (let* ((khash  (hash key (block-size next-base)))
                           (offset (block-hash-table-ref next-base khash)))
                      (loop next-base khash offset
                            (block-offset base))))))))))

;;! Return the first key/value pair from {vhash} whose key is equal to
;; {key} according to the {equal?} equality predicate.
(define (vhash-assoc key vhash #!optional (equal? equal?) (hash hash))
  (%vhash-assoc key vhash equal? hash))

;;! Return the first key/value pair from {vhash} whose key is {eq?} to
;; {key}.
(define (vhash-assq key vhash)
  (%vhash-assoc key vhash eq? eq?-hash))

;;! Return the first key/value pair from {vhash} whose key is {eqv?} to
;; {key}.
(define (vhash-assv key vhash)
  (%vhash-assoc key vhash eqv? eqv?-hash))

;;! Remove all associations from {vhash} with {key}, comparing keys with {equal?}.
(define (vhash-delete key vhash #!optional (equal? equal?) (hash hash))
  (if (vhash-assoc key vhash equal? hash)
      (vlist-fold (lambda (k+v result)
                    (let ((k (car k+v))
                          (v (cdr k+v)))
                      (if (equal? k key)
                          result
                          (vhash-cons k v result hash))))
                  vlist-null
                  vhash)
      vhash))

(define vhash-delq (cut vhash-delete <> <> eq? eq?-hash))
(define vhash-delv (cut vhash-delete <> <> eqv? eqv?-hash))

;;! Fold over the key/pair elements of {vhash} from left to right, with
;; each call to {proc} having the form {({proc} key value result)},
;; where {result} is the result of the previous call to {proc} and
;; {init} the value of {result} for the first call to {proc}.
(define (vhash-fold proc init vhash)
  (vlist-fold (lambda (key+value result)
                (proc (car key+value) (cdr key+value)
                      result))
              init
              vhash))

;;! Fold over the key/pair elements of {vhash} from right to left, with
;; each call to {proc} having the form {({proc} key value result)},
;; where {result} is the result of the previous call to {proc} and
;; {init} the value of {result} for the first call to {proc}.
(define (vhash-fold-right proc init vhash)
  (vlist-fold-right (lambda (key+value result)
                      (proc (car key+value) (cdr key+value)
                            result))
                    init
                    vhash))

;;! Return the vhash corresponding to {alist}, an association list.
(define (alist->vhash alist #!optional (hash hash))
  (fold-right (lambda (pair result)
                (vhash-cons (car pair) (cdr pair) result hash))
              vlist-null
              alist))
