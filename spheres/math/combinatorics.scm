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

;;! Combinations of a list
(define (combinations m lst)
  (let comb ((m m)
             (lst lst))
    (cond ((= m 0) '(()))
          ((null? lst) '())
          (else (append (map (lambda (y) (cons (car lst) y))
                             (comb (- m 1) (cdr lst)))
                        (comb m (cdr lst)))))))

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
