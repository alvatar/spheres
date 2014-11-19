;;!!! SRFI-69: Basic hash tables
;; .author Arthur T Smyles. Implemented based on Gambit's tables
;; .author Álvaro Castro-Castilla, 2014 - Ported to SchemeSpheres
;;
;; This SRFI is provided for supporting modules that require this API.
;; Implementation is based upon Gambit's tables, which are more powerful. They are
;; actually the same type, thus being compatible.

;; XXX: This is the proper way of declaring Gambit's tables
;; (define-type table
;;   id: 5917e472-85e5-11d9-a2c0-00039301ba52
;;   type-exhibitor: macro-type-table
;;   constructor: macro-make-table
;;   implementer: implement-type-table
;;   opaque:
;;   macros:
;;   prefix: macro-
;;   (flags unprintable:)
;;   (test  unprintable:)
;;   (hash  unprintable:)
;;   (loads unprintable:)
;;   (gcht  unprintable:)
;;   (init  unprintable:))

;; Minimal hash table declaration, since define-type is still problematic in 

(define ##table-type (##structure-type (make-table)))
(define-macro (macro-table-flags obj)
  (##list '(let () (##declare (extended-bindings)) ##direct-structure-ref)
          obj
          1
          '##table-type
          #f))
(define-macro (macro-table-flags-set! obj val)
  (##list '(let () (##declare (extended-bindings)) ##direct-structure-set!)
          obj
          val
          1
          '##table-type
          #f))
(define-macro (macro-table-gcht-set! obj val)
  (##list '(let () (##declare (extended-bindings)) ##direct-structure-set!)
          obj
          val
          5
          '##table-type
          #f))
(define-macro (macro-table-hash obj)
  (##list '(let () (##declare (extended-bindings)) ##direct-structure-ref)
          obj
          3
          '##table-type
          #f))
(define-macro (macro-table-test obj)
  (##list '(let () (##declare (extended-bindings)) ##direct-structure-ref)
          obj
          2
          '##table-type
          #f))

;;;;

(define-macro (macro-slot index struct . val)
  (if (null? val)
      `(##vector-ref ,struct ,index)
      `(##vector-set! ,struct ,index ,@val)))

(define-macro (macro-gc-hash-table-flags ht)
  `(macro-slot 1 ,ht))
(define-macro (macro-gc-hash-table-flags-set! ht x)
  `(macro-slot 1 ,ht ,x))
(define-macro (macro-gc-hash-table-count ht)
  `(macro-slot 2 ,ht))
(define-macro (macro-gc-hash-table-count-set! ht x)
  `(macro-slot 2 ,ht ,x))
(define-macro (macro-gc-hash-table-min-count ht)
  `(macro-slot 3 ,ht))
(define-macro (macro-gc-hash-table-min-count-set! ht x)
  `(macro-slot 3 ,ht ,x))
(define-macro (macro-gc-hash-table-free ht)
  `(macro-slot 4 ,ht))
(define-macro (macro-gc-hash-table-free-set! ht x)
  `(macro-slot 4 ,ht ,x))

(define-macro (macro-gc-hash-table-key0) 5)
(define-macro (macro-gc-hash-table-val0) 6)

(define-macro (macro-gc-hash-table-flag-weak-keys) 1)
(define-macro (macro-gc-hash-table-flag-weak-vals) 2)
(define-macro (macro-gc-hash-table-flag-key-moved) 4)
(define-macro (macro-gc-hash-table-flag-entry-deleted) 8)
(define-macro (macro-gc-hash-table-flag-mem-alloc-keys) 16)
(define-macro (macro-gc-hash-table-flag-need-rehash) 32)

(define-macro (macro-gc-hash-table-key-ref ht i*2)
  `(##vector-ref ,ht (##fx+ ,i*2 (macro-gc-hash-table-key0))))
(define-macro (macro-gc-hash-table-key-set! ht i*2 x)
  `(##vector-set! ,ht (##fx+ ,i*2 (macro-gc-hash-table-key0)) ,x))

(define-macro (macro-gc-hash-table-val-ref ht i*2)
  `(##vector-ref ,ht (##fx+ ,i*2 (macro-gc-hash-table-val0))))
(define-macro (macro-gc-hash-table-val-set! ht i*2 x)
  `(##vector-set! ,ht (##fx+ ,i*2 (macro-gc-hash-table-val0)) ,x))

;;-------------------------------------------------------------------------------

;;!! Constructors
(define hash-table-type (##structure-type (make-table)))

(define (make-hash-table . rest)
  (let ((equal? (if (null? rest) #f (car rest)))
	(hash (if (or (null? rest) (null? (cdr rest))) #f (cadr  rest)))
	(args (if (or (null? rest) (null? (cdr rest))) '() (cddr rest))))
    (cond
     ((and equal? hash) (apply make-table test: equal? hash: hash args))
     (equal? (make-table test: equal?))
     (else (make-table)))))

(define hash-table? table?)

(define (alist->hash-table alist . rest)
  (let ((equal? (if (null? rest) #f (car rest)))
	(hash (if (or (null? rest) (null? (cdr rest))) #f (cadr  rest)))
	(args (if (or (null? rest) (null? (cdr rest))) '() (cddr rest))))
    (cond
     ((and equal? hash) (apply list->table alist test: equal? hash: hash args))
     (equal? (list->table alist test: equal?))
     (else (list->table alist)))))

;;!! Reflection

(define (hash-table-equivalence-function hash-table) 
  (macro-table-test hash-table))

(define (hash-table-hash-function hash-table)
	(macro-table-hash hash-table))

(define (hash-table-mutable? hash-table) (not (bit-set? 6 (macro-table-flags hash-table))))
(define (hash-table-mutable?-set! hash-table mutable?)
  (macro-table-flags-set! hash-table (bitwise-merge 64 (macro-table-flags hash-table) (if mutable? 0 64))))

;;!! Dealing with single elements

(define (hash-table-ref hash-table key . rest)
  (let ((thunk (if (pair? rest) (car rest) #f)))
    (##table-access hash-table key
                    (lambda (table key gcht probe2 default-value) 
                      (macro-gc-hash-table-val-ref gcht probe2))
                    (lambda (table key gcht probe2 deleted2 default-value)
                      (if default-value (default-value)
                          (##raise-unbound-table-key-exception hash-table-ref table key))) thunk))) 

(define (hash-table-ref/default hash-table key default) 
  (table-ref hash-table key default))

(define (hash-table-set! hash-table key value)
  (cond 
   ((not (hash-table? hash-table)) (assertion-violation 'hash-table-set! "First argument must be a hash-table" hash-table))
   ((hash-table-mutable? hash-table) (table-set! hash-table key value))))

(define (hash-table-delete! hash-table key)
  (if (hash-table-mutable? hash-table)
      (table-set! hash-table key)))

(define (hash-table-exists? hash-table key)
  (##table-access hash-table key (lambda rest #t) (lambda rest #f) #!void))

(define (hash-table-update! hash-table key function . rest)
  (if (hash-table-mutable? hash-table)
      (let ((thunk (if (pair? rest) (car rest) #f)))
        (##table-access
         hash-table
         key
         (lambda (table key gcht probe2 val)
           (macro-gc-hash-table-val-set! gcht probe2 (function (macro-gc-hash-table-val-ref gcht probe2)))
           (##void))
         (lambda (table key gcht probe2 deleted2 val)
           ;; key was not found (search ended at position "probe2" and the
           ;; first deleted entry encountered is at position "deleted2")
           (if val 
               (if deleted2
                   (let ((count (##fixnum.+ (macro-gc-hash-table-count gcht) 1)))
                     (macro-gc-hash-table-count-set! gcht count)
                     (macro-gc-hash-table-key-set! gcht deleted2 key)
                     (macro-gc-hash-table-val-set! gcht deleted2 (function (val)))
                     (##void))
                   (let ((count (##fixnum.+ (macro-gc-hash-table-count gcht) 1))
                         (free (##fixnum.- (macro-gc-hash-table-free gcht) 1)))
                     (macro-gc-hash-table-count-set! gcht count)
                     (macro-gc-hash-table-free-set! gcht free)
                     (macro-gc-hash-table-key-set! gcht probe2 key)
                     (macro-gc-hash-table-val-set! gcht probe2 (function (val)))
                     (if (##fixnum.< free 0)
                         (##table-resize! table)
                         (##void))))
               (##raise-unbound-table-key-exception hash-table-ref table key)))
         thunk))))

(define (hash-table-update!/default hash-table key function default)
  (hash-table-update! hash-table key function (lambda () default)))

;;!! Dealing with the whole contents

(define hash-table-size table-length)
(define (hash-table-keys hash-table)
  (##table-foldl (lambda (base key) (cons key base)) '() (lambda (key value) key) hash-table))

(define (hash-table-values hash-table)
  (##table-foldl (lambda (base value) (cons value base)) '() (lambda (key value) value) hash-table))

(define (hash-table-walk table proc)
  (cond
   ((not (hash-table? table)) (assertion-violation 'hash-table-walk "first argument must be a table" table))
   ((not (procedure? proc)) (assertion-violation 'hash-table-walk "second argument must be a procedure" proc))
   (else  (table-for-each proc table))))

(define (hash-table-fold hash-table f init-value)
  (##table-foldl (lambda (base key-value)
                   (f (car key-value) (cdr key-value) base))
                 init-value
                 cons
                 hash-table))

(define hash-table->alist table->list)
(define hash-table-copy table-copy)
		
(define (hash-table-merge! hash-table-1 hash-table-2)
  (if (hash-table-mutable? hash-table-1) 
      (table-merge! hash-table-1 hash-table-2 #t)
      (table-merge hash-table-1 hash-table-2 #t)))

(define (hash-table-clear! hash-table . rest)
  (if (hash-table-mutable? hash-table)
      (let ((size (if (pair? rest) (car rest) 0)))
	(macro-table-gcht-set! hash-table size))))

;;!! Hashing

(define hash equal?-hash)

(define string-hash string=?-hash)

(define string-ci-hash string-ci=?-hash)

(define hash-by-identity eq?-hash)



;; SRFI reference implementation
;; Copyright (C) Panu Kalliokoski (2005). All Rights Reserved.
;;
;; (define *default-bound* (- (expt 2 29) 3))
;;
;; (define (%string-hash s ch-conv bound)
;;   (let ((hash 31)
;; 	(len (string-length s)))
;;     (do ((index 0 (+ index 1)))
;;         ((>= index len) (modulo hash bound))
;;       (set! hash (modulo (+ (* 37 hash)
;; 			    (char->integer (ch-conv (string-ref s index))))
;; 			 *default-bound*)))))
;;
;; (define (string-hash s . maybe-bound)
;;   (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
;;     (%string-hash s (lambda (x) x) bound)))
;;
;; (define (string-ci-hash s . maybe-bound)
;;   (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
;;     (%string-hash s char-downcase bound)))
;;
;; (define (symbol-hash s . maybe-bound)
;;   (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
;;     (%string-hash (symbol->string s) (lambda (x) x) bound)))
;;
;; (define (hash obj . maybe-bound)
;;   (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
;;     (cond ((integer? obj) (modulo obj bound))
;; 	  ((string? obj) (string-hash obj bound))
;; 	  ((symbol? obj) (symbol-hash obj bound))
;; 	  ((real? obj) (modulo (+ (numerator obj) (denominator obj)) bound))
;; 	  ((number? obj)
;; 	   (modulo (+ (hash (real-part obj)) (* 3 (hash (imag-part obj))))
;; 		   bound))
;; 	  ((char? obj) (modulo (char->integer obj) bound))
;; 	  ((vector? obj) (vector-hash obj bound))
;; 	  ((pair? obj) (modulo (+ (hash (car obj)) (* 3 (hash (cdr obj))))
;; 			       bound))
;; 	  ((null? obj) 0)
;; 	  ((not obj) 0)
;; 	  ((procedure? obj) (error "hash: procedures cannot be hashed" obj))
;; 	  (else 1))))
;;
;; (define hash-by-identity hash)
;;
;; (define (vector-hash v bound)
;;   (let ((hashvalue 571)
;; 	(len (vector-length v)))
;;     (do ((index 0 (+ index 1)))
;;         ((>= index len) (modulo hashvalue bound))
;;       (set! hashvalue (modulo (+ (* 257 hashvalue) (hash (vector-ref v index)))
;; 			      *default-bound*)))))
;;
;; (define %make-hash-node cons)
;; (define %hash-node-set-value! set-cdr!)
;; (define %hash-node-key car)
;; (define %hash-node-value cdr)
;;
;; (define-record-type <srfi-hash-table>
;;   (%make-hash-table size hash compare associate entries)
;;   hash-table?
;;   (size hash-table-size hash-table-set-size!)
;;   (hash hash-table-hash-function)
;;   (compare hash-table-equivalence-function)
;;   (associate hash-table-association-function)
;;   (entries hash-table-entries hash-table-set-entries!))
;;
;; (define *default-table-size* 64)
;;
;; (define (appropriate-hash-function-for comparison)
;;   (or (and (eq? comparison eq?) hash-by-identity)
;;       (and (eq? comparison string=?) string-hash)
;;       (and (eq? comparison string-ci=?) string-ci-hash)
;;       hash))
;;
;; (define (make-hash-table . args)
;;   (let* ((comparison (if (null? args) equal? (car args)))
;; 	 (hash
;;           (if (or (null? args) (null? (cdr args)))
;;               (appropriate-hash-function-for comparison) (cadr args)))
;; 	 (size
;;           (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
;;               *default-table-size* (caddr args)))
;; 	 (association
;;           (or (and (eq? comparison eq?) assq)
;;               (and (eq? comparison eqv?) assv)
;;               (and (eq? comparison equal?) assoc)
;;               (letrec
;;                   ((associate
;; 		    (lambda (val alist)
;; 		      (cond ((null? alist) #f)
;; 			    ((comparison val (caar alist)) (car alist))
;; 			    (else (associate val (cdr alist)))))))
;;                 associate))))
;;     (%make-hash-table 0 hash comparison association (make-vector size '()))))
;;
;; (define (make-hash-table-maker comp hash)
;;   (lambda args (apply make-hash-table (cons comp (cons hash args)))))
;; (define make-symbol-hash-table
;;   (make-hash-table-maker eq? symbol-hash))
;; (define make-string-hash-table
;;   (make-hash-table-maker string=? string-hash))
;; (define make-string-ci-hash-table
;;   (make-hash-table-maker string-ci=? string-ci-hash))
;; (define make-integer-hash-table
;;   (make-hash-table-maker = modulo))
;;
;; (define (%hash-table-hash hash-table key)
;;   ((hash-table-hash-function hash-table)
;;    key (vector-length (hash-table-entries hash-table))))
;;
;; (define (%hash-table-find entries associate hash key)
;;   (associate key (vector-ref entries hash)))
;;
;; (define (%hash-table-add! entries hash key value)
;;   (vector-set! entries hash
;; 	       (cons (%make-hash-node key value)
;; 		     (vector-ref entries hash))))
;;
;; (define (%hash-table-delete! entries compare hash key)
;;   (let ((entrylist (vector-ref entries hash)))
;;     (cond ((null? entrylist) #f)
;; 	  ((compare key (caar entrylist))
;; 	   (vector-set! entries hash (cdr entrylist)) #t)
;; 	  (else
;;            (let loop ((current (cdr entrylist)) (previous entrylist))
;;              (cond ((null? current) #f)
;;                    ((compare key (caar current))
;;                     (set-cdr! previous (cdr current)) #t)
;;                    (else (loop (cdr current) current))))))))
;;
;; (define (%hash-table-walk proc entries)
;;   (do ((index (- (vector-length entries) 1) (- index 1)))
;;     ((< index 0)) (for-each proc (vector-ref entries index))))
;;
;; (define (%hash-table-maybe-resize! hash-table)
;;   (let* ((old-entries (hash-table-entries hash-table))
;; 	 (hash-length (vector-length old-entries)))
;;     (if (> (hash-table-size hash-table) hash-length)
;;         (let* ((new-length (* 2 hash-length))
;;                (new-entries (make-vector new-length '()))
;;                (hash (hash-table-hash-function hash-table)))
;;           (%hash-table-walk
;;            (lambda (node)
;;              (%hash-table-add! new-entries
;;                                (hash (%hash-node-key node) new-length)
;;                                (%hash-node-key node) (%hash-node-value node)))
;;            old-entries)
;;           (hash-table-set-entries! hash-table new-entries)))))
;;
;; (define (hash-table-ref hash-table key . maybe-default)
;;   (cond ((%hash-table-find (hash-table-entries hash-table)
;; 			   (hash-table-association-function hash-table)
;; 			   (%hash-table-hash hash-table key) key)
;; 	 => %hash-node-value)
;; 	((null? maybe-default)
;; 	 (error "hash-table-ref: no value associated with" key))
;; 	(else ((car maybe-default)))))
;;
;; (define (hash-table-ref/default hash-table key default)
;;   (hash-table-ref hash-table key (lambda () default)))
;;
;; (define (hash-table-set! hash-table key value)
;;   (let ((hash (%hash-table-hash hash-table key))
;; 	(entries (hash-table-entries hash-table)))
;;     (cond ((%hash-table-find entries
;; 			     (hash-table-association-function hash-table)
;; 			     hash key)
;; 	   => (lambda (node) (%hash-node-set-value! node value)))
;; 	  (else (%hash-table-add! entries hash key value)
;; 		(hash-table-set-size! hash-table
;;                                       (+ 1 (hash-table-size hash-table)))
;; 		(%hash-table-maybe-resize! hash-table)))))
;;
;; (define (hash-table-update! hash-table key function . maybe-default)
;;   (let ((hash (%hash-table-hash hash-table key))
;; 	(entries (hash-table-entries hash-table)))
;;     (cond ((%hash-table-find entries
;; 			     (hash-table-association-function hash-table)
;; 			     hash key)
;; 	   => (lambda (node)
;; 	        (%hash-node-set-value!
;;                  node (function (%hash-node-value node)))))
;; 	  ((null? maybe-default)
;; 	   (error "hash-table-update!: no value exists for key" key))
;; 	  (else (%hash-table-add! entries hash key
;; 				  (function ((car maybe-default))))
;; 		(hash-table-set-size! hash-table
;;                                       (+ 1 (hash-table-size hash-table)))
;; 		(%hash-table-maybe-resize! hash-table)))))
;;
;; (define (hash-table-update!/default hash-table key function default)
;;   (hash-table-update! hash-table key function (lambda () default)))
;;
;; (define (hash-table-delete! hash-table key)
;;   (if (%hash-table-delete! (hash-table-entries hash-table)
;; 			   (hash-table-equivalence-function hash-table)
;; 			   (%hash-table-hash hash-table key) key)
;;       (hash-table-set-size! hash-table (- (hash-table-size hash-table) 1))))
;;
;; (define (hash-table-exists? hash-table key)
;;   (and (%hash-table-find (hash-table-entries hash-table)
;; 			 (hash-table-association-function hash-table)
;; 			 (%hash-table-hash hash-table key) key) #t))
;;
;; (define (hash-table-walk hash-table proc)
;;   (%hash-table-walk
;;    (lambda (node) (proc (%hash-node-key node) (%hash-node-value node)))
;;    (hash-table-entries hash-table)))
;;
;; (define (hash-table-fold hash-table f acc)
;;   (hash-table-walk hash-table 
;;                    (lambda (key value) (set! acc (f key value acc))))
;;   acc)
;;
;; (define (alist->hash-table alist . args)
;;   (let* ((comparison (if (null? args) equal? (car args)))
;; 	 (hash
;;           (if (or (null? args) (null? (cdr args)))
;;               (appropriate-hash-function-for comparison) (cadr args)))
;; 	 (size
;;           (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
;;               (max *default-table-size* (* 2 (length alist))) (caddr args)))
;; 	 (hash-table (make-hash-table comparison hash size)))
;;     (for-each
;;      (lambda (elem)
;;        (hash-table-update!/default
;;         hash-table (car elem) (lambda (x) x) (cdr elem)))
;;      alist)
;;     hash-table))
;;
;; (define (hash-table->alist hash-table)
;;   (hash-table-fold hash-table
;; 		   (lambda (key val acc) (cons (cons key val) acc)) '()))
;;
;; (define (hash-table-copy hash-table)
;;   (let ((new (make-hash-table (hash-table-equivalence-function hash-table)
;;   			      (hash-table-hash-function hash-table)
;; 			      (max *default-table-size*
;; 				   (* 2 (hash-table-size hash-table))))))
;;     (hash-table-walk hash-table
;; 		     (lambda (key value) (hash-table-set! new key value)))
;;     new))
;;
;; (define (hash-table-merge! hash-table1 hash-table2)
;;   (hash-table-walk
;;    hash-table2
;;    (lambda (key value) (hash-table-set! hash-table1 key value)))
;;   hash-table1)
;;
;; (define (hash-table-keys hash-table)
;;   (hash-table-fold hash-table (lambda (key val acc) (cons key acc)) '()))
;;
;; (define (hash-table-values hash-table)
;;   (hash-table-fold hash-table (lambda (key val acc) (cons val acc)) '()))

