; 			Verifying the treaps package
;
; IMPORT
; appropriate prelude: myenv.scm, myenv-bigloo.scm, myenv-scm.scm
;	depending on your system
; catch-error.scm -- for procedure, for-syntax
; treap.scm
;
; $Id: vtreap.scm,v 1.2 2002/11/14 04:45:26 oleg Exp oleg $

(declare				; Compilation optimization options
 (block)
 (standard-bindings)
 (fixnum)
)

	; Check out that the 'form' has failed indeed
	; (as it was supposed to)
(define-macro (must-have-failed form)
  `(assert (failed?
      (##catch-all
        (lambda (sig args) (error "catching " sig args))
        (lambda () ,form)))))

(cerr nl nl "--> Sorting of a set of numbers via a treap" nl)
(let
  ((min-key -1) (max-key 10)
   (treap (make-treap (lambda (x y) (- x y))))

			; a hard-wired association between a key and a value
   (compute-assoc (lambda (key) (cons key (++ key)))))

  (cerr "\tloading a sequence [" min-key "," max-key "] in ascending order" nl)
  (assert (treap 'empty?))
  (assert (zero? (treap 'size)))
  (assert (zero? (treap 'depth)))
  (do ((i min-key (++ i))) ((> i max-key))
    (assert (not ((treap 'put!) i (cdr (compute-assoc i))))))
  (treap 'debugprint)
  (cerr "\ttreap's depth is " (treap 'depth) nl)
  (assert (= (treap 'size) (++ (- max-key min-key))))
  (assert (not (treap 'empty?)))
  (assert (equal? (treap 'get-min) (compute-assoc min-key)))
  (assert (equal? (treap 'get-max) (compute-assoc max-key)))
  (assert (equal? (treap 'get-min) ((treap 'get) min-key)))
  (assert (equal? (treap 'get-max) ((treap 'get) max-key)))

  (assert (equal? ((treap 'get) (++ min-key))
      (compute-assoc (++ min-key))))
  (assert (equal? ((treap 'get) (++ min-key) #f)
      (compute-assoc (++ min-key))))
		; check looking up of non-existing keys
  (assert (not ((treap 'get) (-- min-key) #f)))
  (assert (= 1 ((treap 'get) (++ max-key) 
        (lambda () 1))))

  (must-have-failed ((treap 'get) (-- min-key)))

  (cerr "\n\tclearing the treap and reloading the same seq in descending order" nl)
  (treap 'clear!)
  (assert (treap 'empty?))
  (assert (zero? (treap 'size)))
  (assert (zero? (treap 'depth)))
  (must-have-failed ((treap 'get) min-key))
  (must-have-failed (treap 'get-min))
  (must-have-failed (treap 'get-max))
  (do ((i max-key (-- i))) ((< i min-key))
    (assert (not ((treap 'put!) i (cdr (compute-assoc i))))))
  (treap 'debugprint)
  (cerr "\ttreap's depth is " (treap 'depth) nl)
  (assert (equal? (treap 'get-min) (compute-assoc min-key)))
  (assert (equal? (treap 'delete-min!) (compute-assoc min-key)))
  (assert (not ((treap 'get) min-key #f)))
  (assert (equal? (treap 'get-min) (compute-assoc (++ min-key))))

  (assert (equal? (treap 'get-max) ((treap 'get) max-key)))
  (assert (equal? (treap 'delete-max!) (compute-assoc max-key)))
  (assert (not ((treap 'get) max-key #f)))
  (assert (equal? (treap 'get-max) ((treap 'get) (-- max-key))))
  (assert (= (treap 'size) (+ -2 (++ (- max-key min-key)))))

  (cerr "\n\tremove the remaining treap elements one by one, from min to max" nl)
  (do ((i (++ min-key) (++ i))) ((> i (-- max-key)))
    (assert (equal? ((treap 'get) i #f) (compute-assoc i)))
    (assert (equal? (treap 'delete-min!) (compute-assoc i)))
    (assert (not ((treap 'get) i #f)))
    )
  (assert (treap 'empty?))
  (assert (zero? (treap 'size)))
  (assert (zero? (treap 'depth)))
  (must-have-failed ((treap 'for-each-ascending) (lambda (association) #t)))
  (must-have-failed ((treap 'for-each-descending) (lambda (association) #t)))

  (cerr "\n\tloading the treap again in a \"random\" order" nl)
  (do ((i min-key) (j max-key) (direction #t (not direction)))
      ((< j i))
      (cond
        (direction
          (assert (not ((treap 'put!) i (cdr (compute-assoc i)))))
          (++! i))
        (else
          (assert (not ((treap 'put!) j (cdr (compute-assoc j)))))
          (--! j))
        ))

  (let* ((a-key (quotient (+ min-key max-key) 2))
         (old-assoc (compute-assoc a-key))
         (new-assoc (cons a-key #\a)))
    (cerr "\n\tchecking putting and deleting of an assoc with key " a-key nl)
    (assert (equal? ((treap 'get) a-key) old-assoc))
    (assert (equal? ((treap 'put!) a-key (cdr new-assoc)) old-assoc))
    (assert (equal? ((treap 'get) a-key) new-assoc))
    (assert (equal? ((treap 'delete!) a-key) new-assoc))
    (assert (not ((treap 'delete!) a-key #f)))
    (assert (equal? ((treap 'put!) a-key (cdr old-assoc)) #f))
    (assert (equal? ((treap 'put!) a-key (cdr old-assoc)) old-assoc))
    (assert (equal? ((treap 'get) a-key) old-assoc))
  )
  (assert (= (treap 'size) (++ (- max-key min-key))))
  (cerr "\ttreap's depth is " (treap 'depth) nl)
  (assert (not (treap 'empty?)))

  (cerr "\tchecking traversing in the ascending order" nl)
  (let ((expected-key min-key))
    ((treap 'for-each-ascending)
      (lambda (association)
        (assert (equal? association (compute-assoc expected-key)))
        (++! expected-key)))
    (assert (= expected-key (++ max-key))))

  (cerr "\tchecking traversing in the descending order" nl)
  (let ((expected-key max-key))
    ((treap 'for-each-descending)
      (lambda (association)
        (assert (equal? association (compute-assoc expected-key)))
        (--! expected-key)))
    (assert (= expected-key (-- min-key))))
)
(cerr nl "Done" nl)


(cerr nl nl "--> Build a treap to map from digit strings to the numbers "
  	    "they represent" nl)
(let
  ((lo 1) (hi 150)
   (treap (make-treap (lambda (x y) (if (string<? x y) -1
           				(if (string>? x y) 1 0)))))
   (min-key #f)		; to be computed later
   (max-key #f)
  )

  (assert (treap 'empty?))
  (assert (zero? (treap 'size)))
  (assert (zero? (treap 'depth)))
  (cerr "\tloading a sequence [" lo "," hi "] in an ascending number order" nl)
  (do ((i lo (++ i))) ((> i hi))
    (let ((key (number->string i)))
      (assert (not ((treap 'put!) key i)))
      (cond
        ((not min-key) (set! min-key key) (set! max-key key))
        ((string<? key min-key) (set! min-key key))
        ((string>? key max-key) (set! max-key key))
        )))
        
  (cerr "\ttreap's depth is " (treap 'depth) nl)
  (assert (= (treap 'size) (++ (- hi lo))))
  (assert (not (treap 'empty?)))
  (assert (equal? (treap 'get-min) (cons min-key (string->number min-key))))
  (assert (equal? (treap 'get-max) (cons max-key (string->number max-key))))
  (assert (equal? (treap 'get-min) ((treap 'get) min-key)))
  (assert (equal? (treap 'get-max) ((treap 'get) max-key)))

  (let* ((a-key-val (quotient (+ lo hi) 2))
         (a-key (number->string a-key-val))
         (old-assoc (cons a-key a-key-val))
         (new-assoc (cons a-key #\a)))
    (cerr "\n\tchecking putting and deleting of an assoc with key " a-key nl)
    (assert (equal? ((treap 'get) a-key) old-assoc))
    (assert (equal? ((treap 'put!) a-key (cdr new-assoc)) old-assoc))
    (assert (equal? ((treap 'get) a-key) new-assoc))
    (assert (equal? ((treap 'delete!) a-key) new-assoc))
    (assert (not ((treap 'delete!) a-key #f)))
    (assert (equal? ((treap 'put!) a-key (cdr old-assoc)) #f))
    (assert (equal? ((treap 'put!) a-key (cdr old-assoc)) old-assoc))
    (assert (equal? ((treap 'get) a-key) old-assoc))
  )
  (assert (= (treap 'size) (++ (- hi lo))))

  (let
    ((values-to-print 15) (printed 0))
    (cerr "\n\tprinting out first " values-to-print " associations in the descending\n"
          "\torder of their string keys (but not the values themselves!)\n")
    ((treap 'for-each-descending)
      (lambda (association)
        (if (zero? printed)
          (assert (equal? association (cons max-key (string->number max-key)))))
        (if (< printed values-to-print)
          (cerr "\t\t" association nl)
          )
        (++! printed)))
    (assert (= printed (treap 'size))))
)
(cerr nl "Done" nl)

(cerr nl nl "All tests passed" nl)
