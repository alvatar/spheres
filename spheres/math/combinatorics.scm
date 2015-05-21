;;!!! Procedures for countable discrete structures and combinatorial problems
;; .author Alvaro Castro-Castilla, 2012-2015

;;! Binomial coefficient
(define (binomial-coefficient n k)
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

;;! Permutations of a list
(define (permutations l)
  (define (insert l n e)
    (if (= 0 n)
        (cons e l)
        (cons (car l)
              (insert (cdr l) (- n 1) e))))
  (define (seq start end)
    (if (= start end)
        (list end)
        (cons start (seq (+ start 1) end))))
  (let permute ((l l))
    (if (null? l)
        '(())
        (apply append (map (lambda (p)
                             (map (lambda (n)
                                    (insert p n (car l)))
                                  (seq 0 (length p))))
                           (permute (cdr l)))))))

(define (permutations-for-each proc set)
  (if (null? set)
      (proc '())
      (for-each
       (lambda (x)
         (permutations-for-each
          (lambda (sub-perm)
            (proc (cons x sub-perm)))
          (delete x set)))
       set)))

;;! Combinations of a list
(define (combinations m lst)
  (let comb ((m m)
             (lst lst))
    (cond ((= m 0) '(()))
          ((null? lst) '())
          (else (append (map (lambda (y) (cons (car lst) y))
                             (comb (- m 1) (cdr lst)))
                        (comb m (cdr lst)))))))

(define (combinations-for-each proc set n)
  (if (zero? n)
      (proc '())
      (let ((n2 (- n 1)))
        (pair-for-each
         (lambda (pr)
           (let ((first (car pr)))
             (combinations-for-each
              (lambda (sub-comb)
                (proc (cons first sub-comb)))
              (cdr pr)
              n2)))
         set))))

;;! Non-continuous sequences found in a list
(define (non-continuous-sequences lst)
  (let recurse ((s 0)
                (lst lst))
    (if (null? lst)
        (if (>= s 3)
            '(())
            '())
        (let ((x (car lst))
              (xs (cdr lst)))
          (if (even? s)
              (append
               (map (lambda (ys) (cons x ys))
                    (recurse (+ s 1) xs))
               (recurse s xs))
              (append
               (map (lambda (ys) (cons x ys))
                    (recurse s xs))
               (recurse (+ s 1) xs)))))))

;;! Power Set: the set of all subsets
(define (power-set set)
  (if (null? set)
      '(())
      (let ((rest (power-set (cdr set))))
        (append (map (lambda (element) (cons (car set) element))
                     rest)
                rest))))

;; orderedx
(define (power-set-for-each proc set)
  (let ((size (length set)))
    (let loop ((i 0))
      (if (> i size)
          '()
          (begin
            (combinations-for-each proc set i)
            (loop (+ i 1)))))))

;;! All subsets of a given size
;; .author Oleg Kiselyov
(define (subsets l n)
  ;; The initialization function. Check the boundary conditions
  (define (loop l ln n accum)
    (cond
     ((<= n 0) (cons '() accum))
     ((< ln n) accum)
     ((= ln n) (cons l accum))
     ((= n 1)
      (let fold ((l l) (accum accum))
	(if (null? l) accum
	    (fold (cdr l) (cons (cons (car l) '()) accum)))))
     (else
      (split l ln n accum))))
  ;; split l in two parts a and b so that (length b) is n
  ;; Invariant: (equal? (append a b) l)
  ;; ln is the length of l
  (define (split l ln n accum)
    (let loop  ((a '()) (b l) (lna 0) (lnb ln))
      (if (= lnb n) (cont a lna b lnb n accum)
	  (loop (cons (car b) a) (cdr b) (+ 1 lna) (- lnb 1)))))
  ;; The main body of the algorithm
  (define (cont a lna b lnb n accum)
    (let* ((accum
	    (loop a lna n accum))
	   (accum              ; this is actually (loop b lnb n accum)
	    (cons b accum))
	   )
      (let inner ((k 1) (accum accum))
	(if (> k (min lna (- n 1))) ; don't loop past meaningful boundaries
	    accum
	    (let ((as (loop a lna k '()))
		  (bs (loop b lnb (- n k) '())))
	      (inner (+ 1 k)
                                        ; compute the cross-product of as and bs
                                        ; and append it to accum
		     (let fold ((bs bs) (accum accum))
		       (if (null? bs) accum
			   (fold (cdr bs)
				 (append
				  (map (lambda (lst) (append lst (car bs))) as)
				  accum))))))))))
  (loop l (length l) n '()))

;;! All combinations of one element from each set)
;; .author Alex Shinn, 2003
(define (cartesian-product lol)
  (if (null? lol)
      (list '())
      (let ((l (car lol))
            (rest (cartesian-product (cdr lol))))
        (append-map
         (lambda (x)
           (map (lambda (sub-prod) (cons x sub-prod)) rest))
         l))))

(define (cartesian-product-for-each proc lol)
  (if (null? lol)
      (proc '())
      (for-each
       (lambda (x)
         (cartesian-product-for-each
          (lambda (sub-prod)
            (proc (cons x sub-prod)))
          (cdr lol)))
       (car lol))))

;; The above is left fixed (it varies elements to the right first).
;; Below is a right fixed product which could be defined with two
;; reverses but is short enough to warrant the performance gain of a
;; separate procedure.

;;(define (cartesian-product-right lol)
;;  (map reverse (cartesian-product (reverse lol))))

(define (cartesian-product-right lol)
  (if (null? lol)
      (list '())
      (let ((l (car lol))
            (rest (cartesian-product (cdr lol))))
        (append-map
         (lambda (sub-prod)
           (map (lambda (x) (cons x sub-prod)) l))
         rest))))

(define (cartesian-product-right-for-each proc lol)
  (if (null? lol)
      (proc '())
      (cartesian-product-right-for-each
       (lambda (sub-prod)
         (for-each (lambda (x) (proc (cons x sub-prod))) (car lol)))
       (cdr lol))))
