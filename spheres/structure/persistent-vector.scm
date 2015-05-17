;;!!! Persistent Vector
;; .author Francesco Bracchi, 2013
;; .source https://github.com/francesco-bracchi/gambit-persistent
;; .version 702cf28b6f60605e874eb036699df0bc20fc9bc2
;;
;; this is the standard, without focus or last optimizations, just plain persistent vector in log(n) / log (lbf) access time

;; This data type is a persistent vector.
;;
;; the internal representation is a tree with nodes that have bf (branching factor)
;; branches each.
;;
;; If you think at the vector index as a binary number, the first log2(bf) bits
;; corresponds to the branch at root, the second block of bits of the same length
;; to the branch in the second level and so on.
;;
;; Values are stored in the leaves.
;;
;; This configuration, for a reasonable branching factor (by default 32) can be
;; bound to a constant (if we want to store 2^32 elements, with branching factor of 16
;; 32 / 4 = 8 is the height of the tree, therefore it needs 8 steps to reach an
;; element.
;;
;; #### length
;; returns the number of elements contained in the vector
;;
;;     (persistent-vector-length <persistent-vector>)
;;     ;; -> int
;;
;; the operation is performed in `O(1)`
;;
;; ### get
;;
;;     (persistent-vector-ref <persistent-vector> j)
;;     ;; -> value
;;
;; this operation is performed in `O(log<k>(n))` where `k` is the branching factor,
;; and `n` is the total number of element in the vector. the default branching factor
;; is 32.
;;
;; ### set
;; this operation is performed (as get) in `O(log<k>(n))` where `k` is the branching factor,
;; and `n` is the total number of element in the vector. the default branching factor
;; is 32.
;;
;;     (persistent-vector-set <persistent-vector> j <value>)
;;     ;; -> <persistent-vector>
;;
;; ### map/for-each
;;
;; these operations apply the same function to the whole vector, in one case returning
;; a new vector, in the other ignoring it
;;
;;     (persistent-vector-map <function> <persistent-vector> <vectors> ...)
;;     ;; -> <persistent-vector>
;;     (persistent-vector-for-each <function> <persistent-vector> <vectors> ...)
;;     ;; -> undefined
;;
;; ### push
;;
;; This operation changes the length of the vector, and adds a new element at
;; the end (i.e. at the `(persistent-vector-length <persistent-vector>)` position.
;;
;; This operation is performed (as get) in `O(log<k>(n))` where `k` is the branching factor,
;; and `n` is the total number of element in the vector. the default branching factor
;; is 32.
;;
;;     (persistent-vector-push <persistent-vector> <value>
;;     ;; -> <persistent-vector>


(declare (standard-bindings)
	 (extended-bindings)
	 (block)
	 (fixnum)
	 (inline-primitives)
	 (not safe)
	 (not debug)
	 (not debug-location)
	 (not debug-source)
	 (not debug-environments))

(define-type persistent-vector-exception
  extender: define-persistent-vector-exception
  id: 5c88ff96-b623-48d5-b313-5a5ceddb974c
  (procedure read-only: unprintable:)
  (arguments read-only: unprintable:)
  (arg-num read-only: unprintable:))

(define-persistent-vector-exception persistent-vector-range-exception
  id: e1df09e4-57fa-4fda-8a62-7335e8994276)

(define-persistent-vector-exception persistent-vector-type-exception
  id: 6f03074f-f8f1-4cf8-81e6-619a788bcf2b)

(define
  persistent-vector-range-exception-procedure
  persistent-vector-exception-procedure)
(define
  persistent-vector-range-exception-arguments
  persistent-vector-exception-arguments)
(define
  persistent-vector-range-exception-arg-num
  persistent-vector-exception-arg-num)

(define
  persistent-vector-type-exception-procedure
  persistent-vector-exception-procedure)
(define
  persistent-vector-type-exception-arguments
  persistent-vector-exception-arguments)
(define
  persistent-vector-type-exception-arg-num
  persistent-vector-exception-arg-num)

(define (range-error p a n)
  (raise (make-persistent-vector-range-exception p a n)))

(define (type-error p a n)
  (raise (make-persistent-vector-type-exception p a n)))

(define-type persistent-vector
  prefix: macro-
  constructor: macro-make-persistent-vector
  id: 0a731ff2-dfd8-4869-927f-4b6bfc49d19b
  predicate: macro-persistent-vector?
  macros:
  (length read-only: unprintable:)
  (lbf read-only: unprintable:)
  (tree read-only: unprintable:)
                                        ;cache

  (depth read-only: unprintable:)
  (bf read-only: unprintable:))

(define-macro (macro-calc-persistent-vector-bf pv) `(arithmetic-shift 1 (macro-persistent-vector-lbf ,pv)))

(define (k pv)
  (let((lbf (macro-persistent-vector-lbf pv)))
    (lambda (j d)
      (extract-bit-field lbf (* d lbf) j))))

(define (r pv)
  (let((lbf (macro-persistent-vector-lbf pv)))
    (lambda (j d)
      (extract-bit-field (* d lbf) 0 j))))

(define (macro-calc-persistent-vector-depth pv)
  (quotient (integer-length (- (macro-persistent-vector-length pv) 1))
	    (macro-persistent-vector-lbf pv)))


;; (define (vector-for pv j)
;;   (let((k (k pv))
;;        (r (r pv)))
;;     (do ((d (macro-persistent-vector-depth pv) (- d 1))
;; 	 (j j (r j d))
;; 	 (t (macro-persistent-vector-tree pv) (vector-ref t (k j d))))
;; 	((<= d 0) t))))

(define (vector-for pv j)
  (let((k (k pv))
       (r (r pv)))
    (let vector-for ((d (macro-persistent-vector-depth pv))
		     (j j)
		     (t (macro-persistent-vector-tree pv)))
      (if (<= d 0) t
	  (vector-for (- d 1)
		      (r j d)
		      (vector-ref t (k j d)))))))

(define (initialize pv init)
  (macro-make-persistent-vector (macro-persistent-vector-length pv)
				(macro-persistent-vector-lbf pv)
				(make-tree pv init)
				(macro-calc-persistent-vector-depth pv)
				(macro-calc-persistent-vector-bf pv)))

(define (unsafe-make size lbf init)
  (initialize (macro-make-persistent-vector size lbf #f #f #f) init))

(define (make-vector&init size init)
  (do ((v (make-vector size))
       (j 0 (+ j 1)))
      ((>= j size) v)
    (vector-set! v j (init j))))

(define (make-tree pv init)
  (let((k (k pv))
       (r (r pv))
       (lbf (macro-persistent-vector-lbf pv)))
    (let make-tree ((d (macro-calc-persistent-vector-depth pv))
		    (l (macro-persistent-vector-length pv))
		    (offset 0))
      (if (<= d 0) (make-vector&init l (lambda (j) (init (+ j offset))))
	  (let*((m (- l 1))
		(k0 (k m d))
		(r0 (r m d))
		(s0 (arithmetic-shift 1 (* lbf d))))
	    (make-vector&init
	     (+ k0 1)
	     (lambda (j) (make-tree (- d 1)
				    (if (= j k0) (+ 1 r0) s0)
				    (+ offset (* j s0))))))))))

(define (unsafe-ref pv j)
  (vector-ref (vector-for pv j) (bitwise-and j (- (macro-persistent-vector-bf pv) 1))))

(define (vector-set t j v)
  (let((copy (vector-copy t)))
    (vector-set! copy j v)
    copy))

(define (tree-set pv j v)
  (let((k (k pv))
       (r (r pv)))
    (let set ((d (macro-persistent-vector-depth pv))
	      (j j)
	      (t (macro-persistent-vector-tree pv)))
      (let((k0 (k j d))
	   (r0 (r j d)))
	(if (<= d 0) (vector-set t k0 v)
	    (vector-set t k0 (set (- d 1) r0 (vector-ref t k0))))))))

(define (unsafe-set pv j v)
  (macro-make-persistent-vector (macro-persistent-vector-length pv)
				(macro-persistent-vector-lbf pv)
				(tree-set pv j v)
				(macro-persistent-vector-depth pv)
				(macro-persistent-vector-bf pv)))

(define (slow-map fn v vs)
  (make-persistent-vector
   (macro-persistent-vector-length v)
   lbf: (macro-persistent-vector-lbf v)
   init: (lambda (j)
	   (apply fn (cons (unsafe-ref v j) (map (lambda (v) (unsafe-ref v j))
						 vs))))))

(define (tree-map d fn t ts)
  (make-vector&init
   (vector-length t)
   (if (= d 0)
       (lambda (j) (apply fn (cons (vector-ref t j) (map (lambda (t) (vector-ref t j)) ts))))
       (lambda (j) (tree-map (- d 1) fn (vector-ref t j) (map (lambda (t) (vector-ref t j)) ts))))))

(define (fast-map fn v vs)
  (make-persistent-vector
   (macro-persistent-vector-length v)
   lbf: (macro-persistent-vector-lbf v)
   init: (tree-map (macro-persistent-vector-depth v)
		   fn
		   (macro-persistent-vector-tree v)
		   (map (lambda (v) (macro-persistent-vector-tree v)) vs))))


(define (unsafe-for-each fn pv)
  (tree-for-each fn pv))

(define (tree-for-each fn pv)
  (let((bf (macro-persistent-vector-bf pv)))
    (let for-each ((d (macro-persistent-vector-depth pv))
		   (offset 0)
		   (t (macro-persistent-vector-tree pv)))
      (if (= d 0)
	  (vector-for-each (lambda (j v) (fn (+ j offset) (vector-ref t j))) t)
	  (vector-for-each (lambda (j v) (for-each (- d 1) (+ offset (* j bf d)) (vector-ref t j))) t)))))

(define (vector-for-each fn v)
  (do ((j 0 (+ j 1)))
      ((>= j (vector-length v)))
    (fn j (vector-ref v j))))

(define (unsafe-push pv v)
  (let((len (macro-persistent-vector-length pv))
       (lbf (macro-persistent-vector-lbf pv)))
    (macro-make-persistent-vector (+ 1 len)
				  lbf
				  (or (tree-push pv v) (tree-slidedown pv v))
				  (quotient (integer-length len) lbf)
				  (macro-persistent-vector-bf pv))))


(define (tree-slidedown pv v)
  (vector (macro-persistent-vector-tree pv) (tree-list (macro-persistent-vector-depth pv) v)))

(define (tree-list d v)
  (if (<= d 1) (vector v)
      (vector (tree-list (- d 1) v))))

(define (tree-push pv v)
  (let((lbf (macro-persistent-vector-lbf pv))
       (bf (macro-persistent-vector-bf pv)))
    (let push ((d (macro-persistent-vector-depth pv))
	       (t (macro-persistent-vector-tree pv)))
      (let((l (vector-length t)))
	(cond
	 ;; we are in a leaf but can't push
	 ((and (= d 0) (= l bf)) #f)

	 ;; we are in a leaf and can push
	 ((= d 0) (make-vector&init (+ 1 l) (lambda (j) (if (< j l) (vector-ref t j) v))))

	 ;; we are in a node, so try to push in the rightmost branch
	 ((push (- d 1) (vector-ref t (- l 1))) =>
	  (lambda (r) (make-vector&init l (lambda (j) (if (< j (- l 1)) (vector-ref t j) r)))))

	 ;; we are in a node, pushing downwards failed, if we have enough rooms we can create a new branch
	 ((< l bf)
	  (make-vector&init (+ 1 l) (lambda (j)  (if (< j l) (vector-ref t j) (tree-list d v)))))

	 ;; otherwise fail
	 (else #f))))))

(define (tree->reverse-vector-list d t rs)
  (if (= d 0) (cons t rs)
      (do ((rs rs (tree->reverse-vector-list (- d 1) (vector-ref t j) rs))
	   (j 0 (+ j 1)))
	  ((>= j (vector-length t)) rs))))

(define (some? t? vs)
  (let some ((vs vs) (j 0))
    (cond
     ((null? vs) #f)
     ((t? (car vs)) j)
     (else (some (cdr vs) (+ j 1))))))

(define (vector-reduce fn i t)
  (do ((j 0 (+ j 1))
       (i i (fn i (vector-ref t j))))
      ((>= j (vector-length t)) i)))

(define (tree-reduce fn i pv)
  (let tree-reduce ((i i)
		    (t (macro-persistent-vector-tree pv))
		    (d (macro-persistent-vector-depth pv)))
    (if (= d 0)
	(vector-reduce fn i t)
	(vector-reduce (lambda (i child) (tree-reduce i child (- d 1))) i t))))

(define (unsafe-reduce fn i pv)
  (tree-reduce fn i pv))


;; external interface

;; test
(define (persistent-vector? pv) (macro-persistent-vector? pv))

;; creation
(define (make-persistent-vector size #!key (lbf 5) (init 0))
  (if (not (procedure? init)) (let ((i0 init)) (set! init (lambda (_) i0))))
  (cond
   ((not (integer? size)) (type-error make-persistent-vector (list size lbf init) 0))
   ((< size 0) (range-error make-persistent-vector (list size lbf init) 0))
   ((not (integer? lbf)) (type-error make-persistent-vector (list size lbf init) 1))
   ((<= lbf 0) (range-error make-persistent-vector (list size lbf init) 1))
   (else (unsafe-make size lbf init))))

(define (persistent-vector . vs)
  (list->persistent-vector vs))

;; length
(define (persistent-vector-length pv)
  (if (not (macro-persistent-vector? pv)) (type-error persistent-vector-length (list pv) 0)
      (macro-persistent-vector-length pv)))

;; get
(define (persistent-vector-ref pv j)
  (cond
   ((not (macro-persistent-vector? pv)) (type-error persistent-vector-ref (list pv j) 0))
   ((not (integer? j)) (type-error persistent-vector-ref (list pv j) 1))
   ((< j 0) (range-error persistent-vector-ref (list pv j) 1))
   ((>= j (macro-persistent-vector-length pv)) (range-error persistent-vector-ref (list pv j) 1))
   (else (unsafe-ref pv j))))

;; set
(define (persistent-vector-set pv j v)
  (cond
   ((not (macro-persistent-vector? pv)) (type-error persistent-vector-set (list pv j v) 0))
   ((not (integer? j)) (type-error persistent-vector-set (list pv j v) 1))
   ((<= j 0) (range-error persistent-vector-set (list pv j v) 1))
   ((>= j (macro-persistent-vector-length pv)) (range-error persistent-vector-set (list pv j v) 1))
   (else (unsafe-set pv j v))))

;; map
(define (persistent-vector-map fn v . vs)
  (cond
   ((not (procedure? fn)) (type-error persistent-vector-map `(,fn ,v ,@vs) 0))
   ((not (macro-persistent-vector? v)) (type-error persistent-vector-map `(,fn ,v ,@vs) 1))
   ((some? (lambda (v0) (not (macro-persistent-vector? v0))) vs) => (lambda (j) (type-error persistent-vector-map `(,fn ,v ,@vs) (+ 2 j))))
   ((some? (lambda (v0) (not (= (macro-persistent-vector-length v)  (macro-persistent-vector-length v0)))) vs) =>
    (lambda (j) (range-error persistent-vector-map `(,fn ,v ,@vs) (+ 2 j))))
   ((some? (lambda (v0) (not (= (macro-persistent-vector-lbf v) (macro-persistent-vector-lbf v0)))) vs) (slow-map fn v vs))
   (else (fast-map fn v vs))))

;; for-each
(define (persistent-vector-for-each fn v)
  (cond
   ((not (procedure? fn)) (type-error persistent-vector-for-each `(,fn ,v) 0))
   ((not (macro-persistent-vector? v)) (type-error persistent-vector-for-each `(,fn ,v) 1))
   (else (unsafe-for-each fn v))))

;; push
(define (persistent-vector-push pv v)
  (if (not (macro-persistent-vector? pv)) (type-error persistent-vector-push (list pv v) 0)
      (unsafe-push pv v)))

;; reduce
(define (persistent-vector-reduce fn i pv)
  (if (not (macro-persistent-vector? pv)) (type-error persistent-vector-reduce (list fn i pv) 2)
      (unsafe-reduce fn i pv)))

;; conversion
(define (vector->persistent-vector v #!key (lbf 5))
  (make-persistent-vector
   (vector-length v)
   lbf: lbf
   init: (lambda (j) (vector-ref v j))))

(define (persistent-vector->vector pv)
  (apply vector-append (reverse (tree->reverse-vector-list (macro-persistent-vector-depth pv) (macro-persistent-vector-tree pv) '()))))

(define (list->persistent-vector l)
  (vector->persistent-vector (list->vector l)))

(define (persistent-vector->list pv)
  (vector->list (persistent-vector->vector pv)))

(define (persistent-vector->string pv)
  (list->string (persistent-vector->list pv)))

(define (string->persistent-vector pv)
  (list->persistent-vector (string->list pv)))

;; (define *top* 323232)
;; (define *pv* (time (make-persistent-vector *top* init: (lambda (x) x))))

;; (pp *top*)
;; (define total (time (persistent-vector-reduce + 0 *pv*)))

;; (pp (time (do ((j 0 (+ j 1))
;; 	       (c 0 (+ c j)))
;; 	      ((>= j *top*) c))))

;; (set! *pv* (persistent-vector-push *pv* (- total)))

;; (pp (time (persistent-vector-reduce + 0 *pv*)))

;; (display "")

