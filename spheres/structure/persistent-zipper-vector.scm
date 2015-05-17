;;!!! Persistent Zipper Vector
;; .author Francesco Bracchi, 2013
;; .source https://github.com/francesco-bracchi/gambit-persistent
;; .version 702cf28b6f60605e874eb036699df0bc20fc9bc2
;;
;; this library provide the same features of persistent-vector, but the uses a
;; zipper to bookmark the last insertion point. This means that it will be faster
;; than normal vector in updating elements in sequential indexes. In some
;; scenario it give advantages on normal persistent vector.
;;
;;     (include "~~persistent/zipper-vector#.scm")
;;     (define zipvect (list->persistent-vector `(0 1 2 3 4 5 6 7 8 9)))
;;     ;; zipvect is a vector with zipper.


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
  id: a88e7834-62d0-4015-8ddb-a508b33de6bf
  (procedure read-only: unprintable:)
  (arguments read-only: unprintable:)
  (arg-num read-only: unprintable:))

(define-persistent-vector-exception persistent-vector-range-exception
  id: 1fa8bfc0-86d2-4e07-bbf7-66a6bbffa005)

(define-persistent-vector-exception persistent-vector-type-exception
  id: 061b558d-a2a3-4f86-bb5e-89b8e487fb33)

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
  (length read-only: )
  (lbf read-only: unprintable:)
;;   (tree read-only: unprintable:)

  ;cache

  (bf read-only:)
  (depth read-only:)

  ;;zipper
  (offset read-only:)
  (focus read-only: )
  (stack read-only: )
  )

(define-macro (macro-calc-persistent-vector-bf pv) `(arithmetic-shift 1 (macro-persistent-vector-lbf ,pv)))

(define-macro (macro-calc-persistent-vector-depth pv)
  `(max (- (ceiling (/ (integer-length (- (macro-persistent-vector-length ,pv) 1))
		       (macro-persistent-vector-lbf ,pv))) 1) 0))

(define (make-vector&init size init)
  (do ((v (make-vector size))
       (j 0 (+ j 1)))
      ((>= j size) v)
    (vector-set! v j (init j))))


(define (unsafe-make size lbf init)
  (initialize (macro-make-persistent-vector size lbf #f #f #f #f #f) init))

;; that's a bad macro, for internal use only!
(define-macro (with-persistent-vector v . b)
  `(let((length (macro-persistent-vector-length ,v))
       (lbf (macro-persistent-vector-lbf ,v))
       (bf (macro-persistent-vector-bf ,v))
       (depth (macro-persistent-vector-depth ,v))
       (offset (macro-persistent-vector-offset ,v))
       (focus (macro-persistent-vector-focus ,v))
       (stack (macro-persistent-vector-stack ,v)))
    ,@b))

(define (make-tree pv init)
  (let make-tree ((d (macro-calc-persistent-vector-depth pv))
		  (m (- (macro-persistent-vector-length pv) 1))
		  (lbf (macro-persistent-vector-lbf pv))
		  (o 0))
    (if (<= d 0) (make-vector&init (+ m 1) (lambda (j) (init (+ j o))))
	(let*((x (* d lbf))
	      (k (extract-bit-field lbf x m))
	      (r (extract-bit-field x 0 m))
	      (s (arithmetic-shift 1 x)))
	  (make-vector&init
	   (+ k 1)
	   (lambda (i) (make-tree (- d 1)
				  (cond
				   ((< i k) (- s 1))
				   (else r))
				  lbf
				  (+ o (* s i)))))))))

(define (initialize pv init)
  (macro-make-persistent-vector (macro-persistent-vector-length pv)
				(macro-persistent-vector-lbf pv)
				(macro-calc-persistent-vector-bf pv)
				(macro-calc-persistent-vector-depth pv)
				0
				(make-tree pv init)
				'()))
(define *hole* #f)

(define (zipper-down pv j)
  (with-persistent-vector
   pv
   (let((v (vector-copy focus)))
     (vector-set! v j *hole*) ;; make a hole for this
     (macro-make-persistent-vector length
				   lbf
				   bf
				   (- depth 1)
				   (+ offset (* j (arithmetic-shift 1 (* depth lbf))))
				   (vector-ref focus j)
				   (cons (cons j v) stack)))))

(define (zipper-up pv)
  (with-persistent-vector
   pv
   (let((j (caar stack))
	(v (vector-copy (cdar stack))))
     (vector-set! v j focus) ;; fill the hole
     (macro-make-persistent-vector length
				   lbf
				   bf
				   (+ depth 1)
				   (- offset (* j (arithmetic-shift 1 (* (+ 1 depth) lbf))))
				   v
				   (cdr stack)))))

(define (zipper-set pv j x)
  (with-persistent-vector
   pv
   (let((v (vector-copy focus)))
     (vector-set! v j x)
     (macro-make-persistent-vector length
				   lbf
				   bf
				   depth
				   offset
				   v
				   stack))))


(define (zipper-ref pv j)
  (vector-ref (macro-persistent-vector-focus pv) j))

(define (zipper-focus pv j)
  (with-persistent-vector
   pv
   (let((delta (- j offset)))
     (cond
      ((< delta 0) (zipper-focus (zipper-up pv) j))
      ((>= delta (arithmetic-shift 1 (* (+ 1 depth) lbf))) (zipper-focus (zipper-up pv) j))
      ((<= depth 0) pv)
      ;; ((>= delta (* (+ 1 depth) bf)) (zipper-focus (zipper-up pv) j))
      (else
	(let*((x (* depth lbf))
	      (k (extract-bit-field lbf x j)))
	  (zipper-focus (zipper-down pv k) j)))))))

(define (vector-map fn t)
  (make-vector&init (vector-length t) (lambda (j) (fn (vector-ref t j)))))

(define (tree-map d fn t)
  (cond
   ((eq? t *hole*) *hole*)
   ((<= d 0) (vector-map fn t))
   (else (vector-map (lambda (e) (tree-map (- d 1) fn e)) t))))

(define (stack-map d fn ss)
  (if (null? ss) '()
      (cons (cons (caar ss) (tree-map (+ d 1) fn (cdar ss)))
	    (stack-map (+ d 1) fn (cdr ss)))))

(define (unsafe-set pv j v)
  (zipper-set (zipper-focus pv j) (extract-bit-field (macro-persistent-vector-lbf pv) 0 j) v))

(define (unsafe-ref pv j)
  (zipper-ref (zipper-focus pv j) (extract-bit-field (macro-persistent-vector-lbf pv) 0 j)))

(define (unsafe-map fn pv)
  (macro-make-persistent-vector
   (macro-persistent-vector-length pv)
   (macro-persistent-vector-lbf pv)
   (macro-persistent-vector-bf pv)
   (macro-persistent-vector-depth pv)
   (macro-persistent-vector-offset pv)
   (tree-map (macro-persistent-vector-depth pv) fn (macro-persistent-vector-focus pv))
   (stack-map (macro-persistent-vector-depth pv) fn (macro-persistent-vector-stack pv))))

(define (unsafe-for-each fn pv)
  (let((length (macro-persistent-vector-length pv)))
    (do ((j 0 (+ j 1)))
	((>= j length))
      (fn (unsafe-ref pv j)))))

(define (unsafe-push pv v)
  (zipper-push
   (zipper-focus pv (- (macro-persistent-vector-length pv) 1))
   v))

(define (zipper-push pv v)
  (let*((focus (macro-persistent-vector-focus pv))
	(bf (macro-persistent-vector-bf pv))
	(flen (vector-length focus)))
    (cond
     ((< flen bf) ;; enough rooms in focus
      (macro-make-persistent-vector
       (+ 1 (macro-persistent-vector-length pv))
       (macro-persistent-vector-lbf pv)
       (macro-persistent-vector-bf pv)
       (macro-persistent-vector-depth pv)
       (macro-persistent-vector-offset pv)
       (make-vector&init (+ flen 1) (lambda (j) (if (< j flen) (vector-ref focus j) v)))
       (macro-persistent-vector-stack pv)))

     ((null? (macro-persistent-vector-stack pv)) ;; top, no more up is possibleo
      (macro-make-persistent-vector
       (+ 1 (macro-persistent-vector-length pv))
       (macro-persistent-vector-lbf pv)
       (macro-persistent-vector-bf pv)
       (+ 1 (macro-persistent-vector-depth pv))
       0
       (vector (macro-persistent-vector-focus pv) (vector v))
       '()))
     (else
      (zipper-push (zipper-up pv) (vector v))))))

(define (unsafe-reduce fn i pv)
  (let((plen (macro-persistent-vector-length pv)))
    (let reduce ((i i)
		 (j 0)
		 (pv pv))
      (if (>= j plen) i
	  (let((pv (zipper-focus pv j)))
	    (reduce (fn i (unsafe-ref pv j))
		    (+ j 1)
		    pv))))))

;; external interface

;; predicate
(define (persistent-vector? pv) (macro-persistent-vector? pv))

;; creation
(define (make-persistent-vector size #!key (lbf 3) (init 0))
  (if (not (procedure? init)) (let ((i0 init))  (set! init (lambda (_) i0))))
  (cond
   ((not (integer? size)) (type-error make-persistent-vector (list size lbf init) 0))
   ((< size 0) (range-error make-persistent-vector (list size lbf init) 0))
   ((not (integer? lbf)) (type-error make-persistent-vector (list size lbf init) 1))
   ((<= lbf 0) (range-error make-persistent-vector (list size lbf init) 1))
   (else (unsafe-make size lbf (if (procedure? init) init (lambda (_) init))))))

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
   ((< j 0) (range-error persistent-vector-set (list pv j v) 1))
   ((>= j (macro-persistent-vector-length pv)) (range-error persistent-vector-set (list pv j v) 1))
   (else (unsafe-set pv j v))))

;; map
(define (persistent-vector-map fn pv)
  (cond
   ((not (procedure? fn)) (type-error persistent-vector-map (list fn pv) 0))
   ((not (macro-persistent-vector? pv)) (type-error persistent-vector-map (list fn pv) 1))
   (else (unsafe-map fn pv))))

;; for-each
(define (persistent-vector-for-each fn pv)
  (cond
   ((not (procedure? fn)) (type-error persistent-vector-for-each (list fn pv) 0))
   ((not (macro-persistent-vector? pv)) (type-error persistent-vector-for-each (list fn pv) 1))
   (else (unsafe-for-each fn pv))))

;; push
(define (persistent-vector-push pv v)
  (cond
   ((not (macro-persistent-vector? pv)) (type-error persistent-vector-push (list pv v) 0))
   (else (unsafe-push pv v))))

;; reduce
(define (persistent-vector-reduce fn i pv)
  (if (not (macro-persistent-vector? pv)) (type-error persistent-vector-reduce (list fn i pv) 2)
      (unsafe-reduce fn i pv)))

;; conversion
(define (vector->persistent-vector v #!key (lbf 3))
  (cond
   ((not (vector? v)) (type-error vector->persistent-vector (list v lbf: lbf) 0))
   ((not (integer? lbf)) (type-error vector->persistent-vector (list v lbf: lbf) 2))
   (else (make-persistent-vector
	  (vector-length v)
	  lbf: lbf
	  init: (lambda (j) (vector-ref v j))))))

(define (persistent-vector->vector pv)
  (if (not (macro-persistent-vector? pv))
      (type-error persistent-vector->vector (list pv) 0)
      (do ((v (make-vector (macro-persistent-vector-length pv)))
	   (j 0 (+ j 1)))
	  ((>= j (vector-length v)) v)
	(vector-set! v j (persistent-vector-ref pv j)))))

(define (list->persistent-vector l)
  (vector->persistent-vector (list->vector l)))

(define (persistent-vector->list pv)
  (vector->list (persistent-vector->vector pv)))

(define (persistent-vector->string pv)
  (list->string (persistent-vector->list pv)))

(define (string->persistent-vector pv)
  (list->persistent-vector (string->list pv)))
