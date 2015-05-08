;;;; test.scm



(define (main args)
  (let* ((maxi (string->number (car args)))
	 (sv (make-sparse-vector))
	 (indices '())
	 (high 0))
    (print "filling ...")
    (do ((i 0 (+ 1 i)))
	((>= i 10000))
      (let ((j (random-integer maxi)))
	(set! high (max high j))
	(sparse-vector-set! sv j j)
	(set! indices (cons j indices))))
    (print "checking (highest = " high ") ...")
    (do ((i indices (cdr i)))
	((null? i))
      (let ((j (car i)))
      (assert (= j (sparse-vector-ref sv j)))))))

(main (command-line-arguments))
