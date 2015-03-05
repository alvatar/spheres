;;!!! Hash Tries: Persistent Trie-Structured Hash Tables
;; .author Taylor R. Campbell, 2009
;; .author Alvaro Castro-Castilla, 2015

;; Copyright (c) 2009, Taylor R. Campbell
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; * Neither the names of the authors nor the names of contributors
;;   may be used to endorse or promote products derived from this
;;   software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.




;; This implements what are called Hash Array Mapped Tries (HAMT) in
;;
;;   Phil Bagwell, `Ideal Hash Trees', Technical Report, 2001.
;;
;; Provided that the hash function have no collisions and yield hash
;; values in a bounded interval, hash tries admit search and update in
;; constant worst-case time (!), bounded by a somewhat larger constant
;; than what one usually finds for the worst-case time of search or
;; replacement, and the amortized time of insertion or deletion, in
;; hash tables.  There is no complicated incremental rehashing going
;; on like in some real-time hash tables to attain these constant time
;; bounds; in fact, there is never any rehashing.  A little more
;; precisely, `constant' means logarithmically proportional to the
;; length of the interval of the hash values, or, practically,
;; linearly proportional to the number of bits in the hash, with small
;; constant factors.
;;
;; Yes, this is the same data structure as Clojure uses to implement
;; its hash maps and hash sets.  Similarly to Clojure, this code uses
;; hash collision buckets rather than the paper's suggestion of
;; repeating hashes salted by the trie depth.
;;
;; Although the pronunciation is identical, and despite the title of
;; Bagwell's paper, a hash trie is not a hash tree.  Sorry.  Nor do
;; these hash tries have any relation to what Knuth calls hash tries.
;;
;; This code depends on SRFIs 8 (RECEIVE), 9 (DEFINE-RECORD-TYPE), 23
;; (ERROR), and 33 (Integer Bitwise Operations)


;;-------------------------------------------------------------------------------
;; Hash Trie Structure

(define-record-type <hash-trie-type>
  (%make-hash-trie-type key-equality-predicate
                        key-hash-function
                        operation/search
                        operation/lookup
                        operation/member?
                        operation/update
                        operation/insert
                        operation/modify
                        operation/intern
                        operation/delete)
  hash-trie-type?
  (key-equality-predicate hash-trie-type.key-equality-predicate)
  (key-hash-function hash-trie-type.key-hash-function)
  (operation/search hash-trie-type.operation/search)
  (operation/lookup hash-trie-type.operation/lookup)
  (operation/member? hash-trie-type.operation/member?)
  (operation/update hash-trie-type.operation/update)
  (operation/insert hash-trie-type.operation/insert)
  (operation/modify hash-trie-type.operation/modify)
  (operation/intern hash-trie-type.operation/intern)
  (operation/delete hash-trie-type.operation/delete))

(define-record-type <hash-trie>
  (%make-hash-trie type count root)
  hash-trie?
  (type hash-trie.type)
  (count hash-trie.count)
  (root hash-trie.root))

(define (make-hash-trie type)
  (%make-hash-trie type 0 (non-node)))

(define (hash-trie/type hash-trie)
  (hash-trie.type hash-trie))

(define (hash-trie/count hash-trie)
  (hash-trie.count hash-trie))

(define (hash-trie/empty? hash-trie)
  (zero? (hash-trie.count hash-trie)))

(define (hash-trie-type/key-equality-predicate hash-trie-type)
  (hash-trie-type.key-equality-predicate hash-trie-type))

(define (hash-trie-type/key-hash-function hash-trie-type)
  (hash-trie-type.key-hash-function hash-trie-type))


;;-------------------------------------------------------------------------------
;;!! Hash Trie Operations

(define (hash-trie/search hash-trie key if-found if-not-found)
  ((hash-trie-type.operation/search (hash-trie.type hash-trie))
   hash-trie key if-found if-not-found))

(define (hash-trie/lookup hash-trie key default)
  ((hash-trie-type.operation/lookup (hash-trie.type hash-trie))
   hash-trie key default))

(define (hash-trie/member? hash-trie key)
  ((hash-trie-type.operation/member? (hash-trie.type hash-trie))
   hash-trie key))

(define (hash-trie/update hash-trie key if-found if-not-found)
  ((hash-trie-type.operation/update (hash-trie.type hash-trie))
   hash-trie key if-found if-not-found))

(define (hash-trie/insert hash-trie key datum)
  ((hash-trie-type.operation/insert (hash-trie.type hash-trie))
   hash-trie key datum))

(define (hash-trie/modify hash-trie key default modifier)
  ((hash-trie-type.operation/modify (hash-trie.type hash-trie))
   hash-trie key default modifier))

(define (hash-trie/intern hash-trie key generator)
  ((hash-trie-type.operation/intern (hash-trie.type hash-trie))
   hash-trie key generator))

(define (hash-trie/delete hash-trie key)
  ((hash-trie-type.operation/delete (hash-trie.type hash-trie))
   hash-trie key))

;;-------------------------------------------------------------------------------
;;!! Nodes

;; A node has one of the following two structures:
;;
;; #(<child-map> <child> ...)
;;
;;   A branch stored compactly with a bit map of children; see the
;;   page below titled `Branch Child Maps'.  Children are indexed by
;;   BITS-PER-CHUNK chunks of their hashes, beginning at the root with
;;   their lowest-order bits.
;;
;; (<hash> (<key> . <datum>) ...)
;;
;;   A hash collision bucket, all of whose keys share a common hash,
;;   whose suffix is <hash>.  (Suffix, not whole hash, because we know
;;   the prefix by getting to this node in the first place.)

(define (non-node)
  #f)

(define (bucket? object)
  (pair? object))

(define (make-bucket hash associations)
  (cons hash associations))

(define (bucket/hash bucket)
  (car bucket))

(define (bucket/list bucket)
  (cdr bucket))

(define (branch? object)
  ;; (and (vector? object)
  ;;      (> (vector-length object) 1))
  (vector? object))

(define (branch/count branch)
  (- (vector-length branch) 1))

(define (make-unary-branch chunk child)
  (vector (child-map/insert (empty-child-map) chunk) child))

(define (make-binary-branch chunk-a child-a chunk-b child-b)
  ;; Assumption: (not (= chunk-a chunk-b))
  (let ((child-map
         (child-map/insert
          (child-map/insert (empty-child-map) chunk-a)
          chunk-b)))
    (if (< chunk-a chunk-b)
        (vector child-map child-a child-b)
        (vector child-map child-b child-a))))

(define (branch/lookup branch chunk)
  (let ((child-map (vector-ref branch 0)))
    (cond ((child-map/full? child-map)
           (vector-ref branch (+ chunk 1)))
          ((child-map/contains? child-map chunk)
           (vector-ref branch (child-map/chunk->index child-map chunk)))
          (else
           (non-node)))))


;;-------------------------------------------------------------------------------
;;!! Branch Update

;; The only mutation in this whole file happens here, and it is
;; restricted to the initialization of new vectors for branches.

;;++ The following loops are probably worth unrolling, since they will
;;++ run for no more than 2^b iterations, where b is BITS-PER-CHUNK.

(define (branch/insert branch chunk child)
  (let ((length (+ (vector-length branch) 1))
        (child-map (vector-ref branch 0)))
    (let ((branch* (make-vector length))
          (child-map* (child-map/insert child-map chunk)))
      (vector-set! branch* 0 child-map*)
      (let ((index (child-map/chunk->index child-map* chunk)))
        (do ((i 1 (+ i 1)))
            ((>= i index))
          (vector-set! branch* i (vector-ref branch i)))
        (vector-set! branch* index child)
        (let loop ((i index))
          (let ((i* (+ i 1)))
            (if (< i* length)
                (begin
                  (vector-set! branch* i* (vector-ref branch i))
                  (loop i*))))))
      branch*)))

(define (branch/replace branch chunk child)
  (let ((length (vector-length branch)))
    (if (and (= length 2)
             (bucket? child)
             ;; Buckets are guaranteed to be non-empty.
             ;; (pair? (bucket/list child))
             (null? (cdr (bucket/list child))))
        (make-bucket (join-hash chunk (bucket/hash child))
                     (bucket/list child))
        (let ((branch* (make-vector length)))
          (let ((index (child-map/chunk->index (vector-ref branch 0) chunk)))
            (do ((i 0 (+ i 1)))
                ((>= i index))
              (vector-set! branch* i (vector-ref branch i)))
            (vector-set! branch* index child)
            (do ((i (+ index 1) (+ i 1)))
                ((>= i length))
              (vector-set! branch* i (vector-ref branch i))))
          branch*))))

(define (branch/delete branch chunk)
  ;; This is called only when the number of children in the branch is
  ;; greater than 1.  If you change that invariant, change this
  ;; definition.
  (let ((length (vector-length branch))
        (child-map (vector-ref branch 0)))
    (let ((child-map* (child-map/delete child-map chunk))
          (index (child-map/chunk->index child-map chunk)))
      (if (= length 3)              ; Ergo, CHILD-MAP* is a singleton.
          (let ((child (vector-ref branch (- 3 index))))
            ;; Check whether the other child (2 if INDEX is 1, 1 if
            ;; INDEX is 2) is a singleton bucket.
            (if (and (bucket? child)
                     ;; Buckets are guaranteed to be non-empty.
                     ;; (pair? (bucket/list child))
                     (null? (cdr (bucket/list child))))
                (make-bucket
                 (join-hash (singleton-child-map/chunk child-map*)
                            (bucket/hash child))
                 (bucket/list child))
                (vector child-map* child)))
          (let* ((length* (- length 1))
                 (branch* (make-vector length*)))
            (vector-set! branch* 0 child-map*)
            (do ((i 1 (+ i 1)))
                ((>= i index))
              (vector-set! branch* i (vector-ref branch i)))
            (let loop ((i index))
              (if (< i length*)
                  (let ((i* (+ i 1)))
                    (vector-set! branch* i (vector-ref branch i*))
                    (loop i*))))
            branch*)))))


;;-------------------------------------------------------------------------------
;;!!! Branch Child Maps

;; A branch is a compact vector of children, without storage for
;; indices that are not used.  The child map is a map of all the
;; indices to bits saying whether or not the branch has a child for
;; the respective index.  For example, if each branch has sixteen
;; children (giving four bits per chunk), and some branch has the
;; zeroth, eighth, and eleventh children, its child map will be
;; #b100100000001.  We find which index in the vector to use (1 for
;; the zeroth child, 2 for the eighth, and 3 for the eleventh) by
;; counting the bits that are set below and including the index bit in
;; the child map.

(define (bit-mask size)
  (bitwise-not (arithmetic-shift -1 size)))

;; If you change BITS-PER-CHUNK, change FULL-CHILD-MAP below.
;;
;; Why this particular choice of BITS-PER-CHUNK, rather than the
;; paper's suggested 5?  This makes child maps fit in the range of
;; fixnums for most Scheme systems.

(define bits-per-chunk 4)

(define (split-hash hash)
  (values (bitwise-and hash (bitwise-not (arithmetic-shift -1 bits-per-chunk)))
          (arithmetic-shift hash (- bits-per-chunk))))

(define (join-hash chunk hash)
  (bitwise-ior chunk (arithmetic-shift hash bits-per-chunk)))

(define (full-child-map)
  ;; (bit-mask (arithmetic-shift 1 bits-per-chunk))
  #xffff)

(define (empty-child-map)
  0)

(define (child-map/full? child-map)
  (= child-map (full-child-map)))

(define (child-map/insert child-map chunk)
  (bitwise-ior child-map (arithmetic-shift 1 chunk)))

(define (child-map/delete child-map chunk)
  (bitwise-and child-map (bitwise-not (arithmetic-shift 1 chunk))))

(define (child-map/contains? child-map chunk)
  (not (zero? (bitwise-and child-map (arithmetic-shift 1 chunk)))))

(define (child-map/chunk->index child-map chunk)
  (bit-count (bitwise-and child-map (bit-mask (+ chunk 1)))))

(define (singleton-child-map/chunk child-map)
  ;; Assumption: CHILD-MAP has only one bit set.
  ;; (- (integer-length child-map) 1)
  (bit-count (- child-map 1)))


;;-------------------------------------------------------------------------------
;;!! Making Hash Trie Types

(define (make-hash-trie-type key=? key-hash)
  (define (search hash-trie key if-found if-not-found)
    (define (leaf-search bucket hash)
      (let ((hash* (bucket/hash bucket))
            (associations (bucket/list bucket)))
        (if (= hash hash*)
            (linear-search associations)
            (if-not-found))))
    ;; Buckets are guaranteed to be non-empty.  Invert the usual linear
    ;; search loop.
    (define (linear-search associations)
      (let ((association (car associations)))
        (if (key=? key (car association))
            (if-found (cdr association))
            (let ((associations (cdr associations)))
              (if (pair? associations)
                  (linear-search associations)
                  (if-not-found))))))
    (define (trie-search branch hash)
      (receive (chunk hash*) (split-hash hash)
               (let ((node (branch/lookup branch chunk)))
                 (cond ((branch? node) (trie-search node hash*))
                       ((bucket? node) (leaf-search node hash*))
                       (else (if-not-found))))))
    ;; Unrolling the first iteration of this loop avoids computing the
    ;; hash when there is only one bucket.  Probably a gratuitous
    ;; optimization.
    (let ((node (hash-trie.root hash-trie)))
      (cond ((branch? node) (trie-search node (key-hash key)))
            ((bucket? node)
             ;; Perform at most one of KEY-HASH or KEY=? if possible.
             ;; This is really silly.
             (let ((list (bucket/list node)))
               (cond ((pair? (cdr list)) (leaf-search node (key-hash key)))
                     ((key=? key (caar list)) (if-found (cdar list)))
                     (else (if-not-found)))))
            (else
             (if-not-found)))))
  (define (lookup hash-trie key default)
    (search hash-trie key
            (lambda (datum)
              datum)
            (lambda ()
              default)))
  (define (member? hash-trie key)
    (search hash-trie key
            (lambda (datum)
              datum
              #t)
            (lambda ()
              #f)))
  ;; MAKE-HASH-TRIE-TYPE, continued: update algorithm
  (define (update hash-trie key if-found if-not-found)
    (define (leaf-search bucket hash replace delete)
      (let ((hash* (bucket/hash bucket))
            (associations (bucket/list bucket)))
        (if (not (= hash hash*))
            (if-not-found
             (lambda (datum)
               (replace
                +1
                (branch-node hash (cons (cons key datum) '())
                             hash* associations))))
            (linear-search associations
                           (lambda (count associations)
                             (replace count (make-bucket hash associations)))
                           delete))))
    ;; Buckets are guaranteed to be non-empty.  Invert the usual linear
    ;; search loop.
    (define (linear-search associations replace delete)
      (let ((association (car associations))
            (associations (cdr associations)))
        (cond ((key=? key (car association))
               (if-found (cdr association)
                         (lambda (datum)
                           (replace 0 (cons (cons key datum) associations)))
                         delete))
              ((pair? associations)
               (linear-search associations
                              (lambda (count associations)
                                (replace count
                                         (cons association associations)))
                              (lambda ()
                                (replace -1 associations))))
              (else
               (if-not-found
                (lambda (datum)
                  (replace +1 (cons (cons key datum) '()))))))))
    ;; MAKE-HASH-TRIE-TYPE, continued: update algorithm, continued
    (define (trie-search branch hash replace delete)
      (receive (chunk hash*) (split-hash hash)
               (let ((node (branch/lookup branch chunk)))
                 (if node
                     (let ((replace
                            (lambda (count node)
                              (replace count (branch/replace branch chunk node))))
                           (delete
                            (if (= 1 (branch/count branch))
                                delete
                                (lambda ()
                                  (replace -1 (branch/delete branch chunk))))))
                       (cond ((branch? node) (trie-search node hash* replace delete))
                             ((bucket? node) (leaf-search node hash* replace delete))
                             (else (error "Invalid hash trie node:" node) #f)))
                     (if-not-found
                      (lambda (datum)
                        (replace
                         +1
                         (branch/insert branch
                                        chunk
                                        (make-bucket hash*
                                                     (cons (cons key datum)
                                                           '()))))))))))
    (define (replace-root count-adjustment root-node)
      (%make-hash-trie (hash-trie.type hash-trie)
                       (+ (hash-trie.count hash-trie) count-adjustment)
                       root-node))
    (define (delete-root)
      (replace-root -1 (non-node)))
    (let ((node (hash-trie.root hash-trie)))
      (cond ((branch? node)
             (trie-search node (key-hash key) replace-root delete-root))
            ((bucket? node)
             (leaf-search node (key-hash key) replace-root delete-root))
            (else
             (if-not-found
              (lambda (datum)
                (replace-root +1
                              (make-bucket (key-hash key)
                                           (cons (cons key datum) '())))))))))
  ;; MAKE-HASH-TRIE-TYPE continued: branching, and update utilities
  (define (branch-node hash-a associations-a hash-b associations-b)
    ;; Assumption: (not (= hash-a hash-b))
    (receive (chunk-a hash-a) (split-hash hash-a)
             (receive (chunk-b hash-b) (split-hash hash-b)
                      (if (= chunk-a chunk-b)
                          (make-unary-branch
                           chunk-a
                           (branch-node hash-a associations-a hash-b associations-b))
                          (make-binary-branch
                           chunk-a (make-bucket hash-a associations-a)
                           chunk-b (make-bucket hash-b associations-b))))))
  (define (insert hash-trie key datum)
    (update hash-trie key
            (lambda (datum* replace delete)
              datum* delete             ;ignore
              (replace datum))
            (lambda (insert)
              (insert datum))))
  (define (modify hash-trie key default modifier)
    (update hash-trie key
            (lambda (datum replace delete)
              delete                    ;ignore
              (replace (modifier datum)))
            (lambda (insert)
              (insert (modifier default)))))
  (define (intern hash-trie key generator)
    (update hash-trie key
            (lambda (datum replace delete)
              replace delete            ;ignore
              (values datum hash-trie))
            (lambda (insert)
              (let ((datum (generator key)))
                (values datum (insert datum))))))
  (define (delete hash-trie key)
    (update hash-trie key
            (lambda (datum replace delete)
              datum replace             ;ignore
              (delete))
            (lambda (insert)
              insert                    ;ignore
              hash-trie)))
  (%make-hash-trie-type key=? key-hash
                        search lookup member?
                        update insert modify intern delete))


;;-------------------------------------------------------------------------------
;;!! FNV-Based (Fowler-Noll-Vo) Hash Functions

;; This definition doesn't give us the real FNV hash, but it is likely
;; to fit within most Schemes' fixnum arithmetic.  The FNV prime
;; (#x1000193) is the usual 32-bit FNV prime, but the offset basis
;; (#x1cf42a8) differs.  It was computed like the usual FNV offset
;; basis, by applying the same iteration but starting with a hash of 0
;; to the 32 octets `chongo <Landon Curt Noll> /\../\'.  Probably this
;; is all a load of nonsense, because 25-bit modular arithmetic
;; instead of 32-bit modular arithmetic probably changes all the
;; constraints that were put on the design of the parameters, but this
;; seems to work and to give a reasonable distribution in some random,
;; unscientific tests.

(define (string-hash string)
  (let loop ((index 0) (hash #x1cf42a8))
    (if (< index (string-length string))
        (loop (+ index 1)
              (bitwise-xor (char->integer (string-ref string index))
                           (bitwise-and #x1ffffff (* hash #x1000193))))
        hash)))

(define (symbol-hash symbol)
  (string-hash (symbol->string symbol)))

(define (exact-integer-hash k)
  (let loop ((k k) (hash #x1cf42a8))
    (if (zero? k)
        hash
        (loop (arithmetic-shift k -8)
              (bitwise-xor (bitwise-and k #xFF)
                           (bitwise-and #x1ffffff (* hash #x1000193)))))))

(define (real-number-hash x)
  (cond ((integer? x)
         (exact-integer-hash (inexact->exact x)))
        ;; This is appealing because it involves no non-integer
        ;; arithmetic, but it gives 1/2 and 0.5 distinct hashes, while
        ;; (= 1/2 0.5).
        ;; ((exact? x)
        ;;  (bitwise-xor
        ;;   (exact-integer-hash (bitwise-xor #xfacade (numerator x)))
        ;;   (exact-integer-hash (denominator x))))
        (else
         (let* ((integral-part (truncate x))
                (fractional-part (round (/ 1 (- x integral-part)))))
           ;; TRUNCATE and ROUND guarantee that INEXACT->EXACT here
           ;; will yield an exact integer, fit for BITWISE-XOR and for
           ;; EXACT-INTEGER-HASH.
           (bitwise-xor
            (exact-integer-hash
             (bitwise-xor #xfedcad (inexact->exact integral-part)))
            (exact-integer-hash (inexact->exact fractional-part)))))))

(define (complex-number-hash z)
  (if (real? z)
      (real-number-hash z)
      ;;++ This does not distinguish the imaginary and real parts.
      (bitwise-xor #xdeface
                   (bitwise-xor (real-number-hash (imag-part z))
                                (real-number-hash (real-part z))))))


;;-------------------------------------------------------------------------------
;;!! Miscellaneous Hash Trie Types

(define hash-trie-type:complex-number
  (make-hash-trie-type = complex-number-hash))

(define hash-trie-type:real-number
  (make-hash-trie-type = real-number-hash))

(define hash-trie-type:exact-integer
  (make-hash-trie-type = exact-integer-hash))

(define hash-trie-type:symbol
  (make-hash-trie-type eq? symbol-hash))

(define hash-trie-type:string
  (make-hash-trie-type string=? string-hash))

;; (define hash-trie-type:string-ci
;;   (make-hash-trie-type string-ci=? string-hash-ci))

;;; With the following definitions of HASH-TRIE/BUCKET-FOLD and
;;; HASH-TRIE/FOLD, the last call to the combinator is a tail call.

(define (hash-trie/fold hash-trie initial-value combinator)
  (define (fold-bucket associations value)
    ;; Buckets are guaranteed to be non-empty.  Invert the usual FOLD
    ;; loop.
    (let ((association (car associations))
          (associations (cdr associations)))
      (if (pair? associations)
          (fold-bucket associations
                       (combinator (car association) (cdr association) value))
          (combinator (car association) (cdr association) value))))
  (define (fold-branch branch index value)
    ;; Branches are guaranteed to have at least one child.
    (if (> index 2)
        (let ((index (- index 1)))
          (fold-branch branch
                       index
                       (fold-node (vector-ref branch index) value)))
        (fold-node (vector-ref branch 1) value)))
  (define (fold-node node value)
    (cond ((branch? node) (fold-branch node (vector-length node) value))
          ((bucket? node) (fold-bucket (bucket/list node) value))
          (else (error "Invalid hash trie node:" node) #f)))
  (let ((root (hash-trie.root hash-trie)))
    (if root
        (fold-node root initial-value)
        initial-value)))

(define (hash-trie/key-list hash-trie)
  (hash-trie/fold hash-trie '()
                  (lambda (key datum list)
                    datum               ;ignore
                    (cons key list))))

(define (hash-trie/datum-list hash-trie)
  (hash-trie/fold hash-trie '()
                  (lambda (key datum list)
                    key                 ;ignore
                    (cons datum list))))

(define (hash-trie->alist hash-trie)
  (hash-trie/fold hash-trie '()
                  (lambda (key datum alist)
                    (cons (cons key datum) alist))))

(define (alist->hash-trie alist hash-trie-type)
  (let loop ((alist alist) (hash-trie (make-hash-trie hash-trie-type)))
    (if (pair? alist)
        (loop (cdr alist)
              (hash-trie/insert hash-trie (caar alist) (cdar alist)))
        hash-trie)))

;; (define (hash-trie->stream hash-trie)
;;   (define (bucket->stream list tail)
;;     (if (pair? list)
;;         (stream-cons (car list) (lazy (bucket->stream (cdr list) tail)))
;;         tail))
;;   (define (branch->stream branch tail)
;;     (let recur ((index (vector-length branch)) (value value))
;;       (if (> index 2)
;;           (let ((index (- index 1)))
;;             (node->stream (vector-ref branch index)
;;                           (lazy (recur index tail))))
;;           tail)))
;;   (define (node->stream node tail)
;;     (cond ((branch? node) (branch->stream node tail))
;;           ((bucket? node) (bucket->stream (bucket/list node) tail))
;;           (else (error "Invalid hash trie node:" node) #f)))
;;   (lazy (let ((root (hash-trie.root hash-trie)))
;;           (if root
;;               (node->stream root stream-nil)
;;               stream-nil))))
;;
;; (define (stream->hash-trie stream hash-trie-type)
;;   (let loop ((stream stream) (hash-trie (make-hash-trie hash-trie-type)))
;;     (if (stream-pair? stream)
;;         (loop (stream-cdr stream)
;;               (hash-trie/insert hash-trie (stream-car stream)))
;;         hash-trie)))
;;
;; ;;; Iterator for foof-loop; see
;; ;;; <http://mumble.net/~campbell/scheme/foof-loop.txt>.
;;
;; (define-syntax in-hash-trie
;;   (syntax-rules ()
;;     ((IN-HASH-TRIE (key-variable datum-variable)
;;                    (hash-trie-expression)
;;                    next . rest)
;;      (next (((HASH-TRIE) hash-trie-expression))      ;Outer bindings
;;            ((STREAM (HASH-TRIE->STREAM HASH-TRIE)    ;Loop variables
;;                     STREAM*))
;;            ()                                        ;Entry bindings
;;            ((NOT (STREAM-PAIR? STREAM)))             ;Termination conditions
;;            (((key-variable datum-variable)           ;Body bindings
;;              (LET ((ASSOCIATION (STREAM-CAR STREAM)))
;;                (VALUES (CAR ASSOCIATION) (CDR ASSOCIATION))))
;;             ((STREAM*) (STREAM-CDR STREAM)))
;;            ()                                        ;Final bindings
;;            next . rest))))
