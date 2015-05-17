;; .author Francesco Bracchi, 2013
;; .source https://github.com/francesco-bracchi/gambit-persistent
;; .version 702cf28b6f60605e874eb036699df0bc20fc9bc2
;;
;; this is the standard, without focus or last optimizations, just plain persistent map in log(n) / log (lbf) access time

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

(define-type persistent-map-exception
  extender: define-persistent-map-exception
  id: 1fc40b57-3c90-4464-a518-7a27cdf6461e
  (procedure read-only: unprintable:))

(define-persistent-map-exception persistent-map-key-not-found-exception
  id: f494ed52-42f8-4edf-ae82-35c61e07ec05
  (arguments read-only: unprintable:))

(define-persistent-map-exception persistent-map-type-exception
  id: ad201dc5-8cb1-4127-978b-f64b8bfbd10a
  (arguments read-only: unprintable:)
  (arg-num read-only: unprintable:))

(define
  persistent-map-type-exception-procedure
  persistent-map-exception-procedure)

(define
  persistent-map-key-not-found-exception-procedure
  persistent-map-exception-procedure)

(define-macro (key-not-found-error p a)
  `(raise (make-persistent-map-key-not-found-exception ,p ,a)))

(define-macro (type-error p a n)
  `(raise (make-persistent-map-type-exception ,p ,a ,n)))

(define-type persistent-map
  prefix: macro-
  constructor: macro-make-persistent-map
  id: 0a731ff2-dfd8-4869-927f-4b6bfc49d19b
  predicate: macro-persistent-map?
  macros:
  (lbf read-only: unprintable:)
  (hash read-only: unprintable:)
  (eq read-only: unprintable:)
  (length read-only:)
  ;;   (tree read-only:)
  (tree read-only: unprintable:)
                                        ;cache
  ;;  (bf read-only: unprintable)
  )

(define-type node
  prefix: macro-
  constructor: macro-make-node
  predicate: macro-node?
  macros:
  (bitmap read-only:)
  (vector read-only:))

(define (make-vector&init size init)
  (do ((v (make-vector size))
       (j 0 (+ j 1)))
      ((>= j size) v)
    (vector-set! v j (init j))))

(define *absent* (list 'absent))

(define-macro (macro-bit-set j n)
  `(bitwise-ior ,n (arithmetic-shift 1 ,j)))

(define-macro (macro-bit-unset j n)
  `(clear-bit-field 1 ,j ,n))

;; (define-macro (macro-node-set n)
;;   `(let((v (macro-node-vector ,n))
;; 	(b (macro-node-bitmap ,n)))
;;      (if (bit-set? h0 ,b)
;; 	 (macro-node-current ,n)
;; 	 (macro-node-deeper ,n))))

(define (unsafe-set pm k val)
  (let*((make-hash (macro-persistent-map-hash pm))
	(eq (macro-persistent-map-eq pm))
	(lbf (macro-persistent-map-lbf pm))
	(hash (make-hash k))
	(length (macro-persistent-map-length pm))
	(delta 0)
	(tree (let tree-set ((depth 0) (n (macro-persistent-map-tree pm)))
		(let ((h0 (extract-bit-field lbf (* depth lbf) hash)))
		  (cond
		   ((macro-node? n)
		    (let*((v (macro-node-vector n))
			  (b (macro-node-bitmap n))
			  (p (bit-count (extract-bit-field h0 0 b))))
		      (if (bit-set? h0 b)
			  (macro-make-node
			   b
			   (make-vector&init
			    (vector-length v)
			    (lambda (j) (if (= j p)
                                       (tree-set (+ 1 depth) (vector-ref v j))
                                       (vector-ref v j)))))
			  (begin
			    (set! delta 1)
			    (macro-make-node
			     (macro-bit-set h0 b)
			     (make-vector&init
			      (+ 1 (vector-length v))
			      (lambda (j) (cond
                                      ((< j p) (vector-ref v j))
                                      ((= j p) (list (cons k val)))
                                      ((> j p) (vector-ref v (- j 1)))))))))))
		   ((pair? n)
		    (let assoc ((ps n) (rs '()))
		      (if (null? ps)
			  (let*((h1 (extract-bit-field lbf (* depth lbf) (make-hash (caar ps))))
				(n1 (macro-make-node (arithmetic-shift 1 h1) (vector n))))
			    (tree-set depth n1))
			  (let*((p (car ps)) (k0 (car p)))
			    (cond
			     ((eq k k0)
			      (append (cdr ps) (cons (cons k val) rs)))
			     ((= hash (make-hash k0))
			      (set! delta 1)
			      (append (cdr ps) (cons (cons k val) (cons p rs))))
			     (else
			      (assoc (cdr ps) (cons p rs))))))))
		   (else
		    (set! delta (+ delta 1))
		    (list (cons k val))))))))

    (macro-make-persistent-map
     lbf
     make-hash
     eq
     (+ length delta)
     tree)))

(define (unsafe-unset pm k)
  (let*((make-hash (macro-persistent-map-hash pm))
	(eq (macro-persistent-map-eq pm))
	(lbf (macro-persistent-map-lbf pm))
	(hash (make-hash k))
	(length (macro-persistent-map-length pm))
	(delta 0)
	(tree (let tree-unset ((depth 0) (n (macro-persistent-map-tree pm)))
		(let ((h0 (extract-bit-field lbf (* depth lbf) hash)))
		  (cond
		   ((macro-node? n)
		    (let*((v (macro-node-vector n))
			  (b (macro-node-bitmap n))
			  (p (bit-count (extract-bit-field h0 0 b))))
		      (if (bit-set? h0 b)
			  (let ((child (tree-unset (+ depth 1) (vector-ref v p))))
			    (cond
			     ((= delta 0) n)
			     ((null? child)
			      (if (<= (vector-length v) 2)
				  (vector-ref v (if (= p 0) 1 0))
				  (let ((b1 (macro-bit-unset h0 b)))
				    (macro-make-node
				     b1
				     (make-vector&init
				      (- (vector-length v) 1)
				      (lambda (j)
					(cond
					 ((< j p) (vector-ref v j))
					 ((>= j p) (vector-ref v (+ j 1))))))))))
			     (else
			      (macro-make-node
			       b
			       (make-vector&init
				(vector-length v)
				(lambda (j)
				  (cond
				   ((< j p) (vector-ref v j))
				   ((= j p) child)
				   ((> j p) (vector-ref v j))))))))))))
		   ((pair? n)
		    (let dissoc ((ps n) (rs '()))
		      (if (null? ps) n
			  (let*((p (car ps)) (k0 (car p)))
			    (if (eq k k0)
				(begin (set! delta -1) (append (cdr ps) rs))
				(dissoc (cdr ps) (cons p rs)))))))
		   (else n))))))
    (macro-make-persistent-map
     lbf
     make-hash
     eq
     (+ length delta)
     tree)))

(define (unsafe-ref pm k default)
  (let*((make-hash (macro-persistent-map-hash pm))
	(eq (macro-persistent-map-eq pm))
	(lbf (macro-persistent-map-lbf pm))
	(hash (make-hash k))
	(length (macro-persistent-map-length pm)))
    (let ref ((depth 0)
	      (n (macro-persistent-map-tree pm)))
      (let ((h0 (extract-bit-field lbf (* depth lbf) hash)))
	(cond
	 ((macro-node? n)
	  (let*((v (macro-node-vector n))
		(b (macro-node-bitmap n))
		(p (bit-count (extract-bit-field h0 0 b))))
	    (cond
	     ((bit-set? h0 b)
	      (ref (+ depth 1) (vector-ref v p)))
	     ((eq? default *absent*)
	      (key-not-found-error persistent-map-ref (list pm k)))
	     (else default))))
	 ((pair? n)
	  (let ref ((ps n))
	    (cond
	     ((pair? ps) (if (eq (caar ps) k) (cdar ps) (ref (cdr ps))))
	     ((eq? default *absent*)
	      (key-not-found-error persistent-map-ref (list pm k)))
	     (else default))))
	 ((eq? default *absent*)
	  (key-not-found-error persistent-map-ref (list pm k)))
	 (else default))))))

(define (unsafe-reduce fn i pm)
  (let tree-reduce ((i i) (n (macro-persistent-map-tree pm)))
    (cond
     ((macro-node? n)
      (let ((v (macro-node-vector n)))
	(do ((j 0 (+ j 1))
	     (i i (tree-reduce i (vector-ref v j))))
	    ((>= j (vector-length v)) i))))
     ((pair? n)
      (do ((ps n (cdr ps))
	   (i i (fn i (car ps))))
	  ((null? ps) i)))
     (else i))))

(define (unsafe-list->persistent-map ps eq hash lbf)
  (let list-> ((ps ps)
	       (pm (macro-make-persistent-map lbf hash eq 0 #f)))
    (cond
     ((and (pair? ps) (pair? (car ps)))
      (list-> (cdr ps) (unsafe-set pm (caar ps) (cdar ps))))
     ((null? ps) pm)
     (else (type-error list->persistent-map (list ps) 0)))))

(define (unsafe-merge-slow p q)
  (unsafe-reduce (lambda (p1 e) (unsafe-set p1 (car e) (cdr e))) p q))

(define (unsafe-merge-fast p q)
  (error "fast merging not implemented, yet"))

(define (make-persistent-map #!key (eq eq?) (hash eq?-hash) (lbf 5))
  (cond
   ((not (procedure? eq)) (type-error make-persistent-map (list eq: eq) 2))
   ((not (procedure? hash)) (type-error make-persistent-map (list hash: hash) 2))
   ((not (integer? lbf)) (type-error make-persistent-map (list lbf: lbf) 2))
   (else (macro-make-persistent-map lbf hash eq 0 #f))))

(define (persistent-map? pm)
  (macro-persistent-map? pm))

(define (persistent-map-length pm)
  (cond
   ((macro-persistent-map? pm) (macro-persistent-map-length pm))
   (else (type-error persistent-map-length (list pm) 0))))

(define (persistent-map-set pm k #!optional (v *absent*))
  (cond
   ((macro-persistent-map? pm) (if (eq? v *absent*) (unsafe-unset pm k) (unsafe-set pm k v)))
   ((eq? v *absent*) (type-error persistent-map-set (list pm k) 0))
   (else (type-error persistent-map-set (list pm k v) 0))))

(define (persistent-map-ref pm k #!optional (default *absent*))
  (cond
   ((macro-persistent-map? pm) (unsafe-ref pm k default))
   ((eq? default *absent*) (type-error persistent-map-set (list pm k) 0))
   (else (type-error persistent-map-set (list pm k default) 0))))

(define (persistent-map-reduce fn i pm)
  (cond
   ((macro-persistent-map? pm) (unsafe-reduce fn i pm))
   (else (type-error persistent-map-reduce (list fn i pm) 2))))

(define (persistent-map-for-each fn pm)
  (cond
   ((macro-persistent-map? pm) (unsafe-reduce (lambda (i p) (fn (car p) (cdr p)) i) #f pm))
   (else (type-error persistent-map-for-each (list fn pm) 1))))

(define (persistent-map->list pm)
  (cond
   ((macro-persistent-map? pm) (unsafe-reduce (lambda (i p) (cons p i)) '() pm))
   (else (type-error persistent-map->list  (list pm) 0))))

(define (persistent-map-keys pm)
  (cond
   ((macro-persistent-map? pm) (unsafe-reduce (lambda (i p) (cons (car p) i)) '() pm))
   (else (type-error persistent-map-keys (list pm) 0))))

(define (persistent-map-values pm)
  (cond
   ((macro-persistent-map? pm) (unsafe-reduce (lambda (i p) (cons (cdr p) i)) '() pm))
   (else (type-error persistent-map-values (list pm) 0))))

(define (persistent-map-merge p q)
  (cond
   ((not (macro-persistent-map? p)) (type-error persistent-map-merge (list p q) 0))
   ((not (macro-persistent-map? q)) (type-error persistent-map-merge (list p q) 1))
   ((and (eq? (macro-persistent-map-lbf p) (macro-persistent-map-lbf q))
	 (eq? (macro-persistent-map-eq p) (macro-persistent-map-eq q))
	 (eq? (macro-persistent-map-hash p) (macro-persistent-map-hash q)))
    (unsafe-merge-fast p q))
   (else (unsafe-merge-slow p q))))

(define (list->persistent-map ps #!key (eq eq?) (hash eq?-hash) (lbf 5))
  (cond
   ((not (procedure? eq)) (type-error make-persistent-map (list eq: eq) 2))
   ((not (procedure? hash)) (type-error make-persistent-map (list hash: hash) 2))
   ((not (integer? lbf)) (type-error make-persistent-map (list lbf: lbf) 2))
   (else (unsafe-list->persistent-map ps eq hash lbf))))
