;;!!! Prime numbers
;; .author Aubrey Jaffer, Copyright (C) 1991, 1993, 1995, 2001, 2002, 2006
;; .author Alvaro Castro-Castilla, 2015
;;
;; Permission to copy this software, to modify it, to redistribute it,
;; to distribute modified versions, and to use it for any purpose is
;; granted, subject to the following restrictions and understandings.
;;
;; 1.  Any copy made of this software must include this copyright notice
;; in full.
;;
;; 2.  I have made no warranty or representation that the operation of
;; this software will be error-free, and I am under no obligation to
;; provide any services, by way of maintenance, update, or otherwise.
;;
;; 3.  In conjunction with products arising from the use of this
;; material, there shall be no use of my name in any advertising,
;; promotional, or sales literature without prior written consent in
;; each case.


;;! Returns #f if n is composite, #t if n is prime. There is a slight chance
;; (expt 2 (- prime:trials)) that a composite will return #t.
;; The prime test and generation procedures implement the Solovay-Strassen
;; primality test.
;; Robert Solovay and Volker Strassen,
;; A Fast Monte-Carlo Test for Primality,
;; SIAM Journal on Computing, 1977, pp 84-85.
(define (prime? n)
  ;; Solovay-Strassen Prime Test
  ;; checks if n is prime.  Returns #f if not prime. #t if (probably) prime.
  ;;   probability of a mistake = (expt 2 (- prime:trials))
  ;;     choosing prime:trials=30 should be enough
  ;;   if n is prime, then J(a,n) is congruent mod n to a**((n-1)/2)
  (define (solovay-strassen-prime? n)
    ;; Returns the value (+1, -1, or 0) of the Jacobi-Symbol of
    ;; exact non-negative integer @1 and exact positive odd integer @2.
    ;; (modulo p 16) is because we care only about the low order bits.
    ;; The odd? tests are inline of (expt -1 ...)
    (define (jacobi-symbol p q)
      (cond ((zero? p) 0)
            ((= 1 p) 1)
            ((odd? p)
             (if (odd? (quotient (* (- (modulo p 16) 1) (- q 1)) 4))
                 (- (jacobi-symbol (modulo q p) p))
                 (jacobi-symbol (modulo q p) p)))
            (else
             (let ((qq (modulo q 16)))
               (if (odd? (quotient (- (* qq qq) 1) 8))
                   (- (jacobi-symbol (quotient p 2) q))
                   (jacobi-symbol (quotient p 2) q))))))
    ;; the maxinum number of iterations of Solovay-Strassen that will
    ;; be done to test a number for primality.
    (define prime:trials 30)
    (do ((i prime:trials (- i 1))
         (a (+ 2 (random-integer (- n 2)))
            (+ 2 (random-integer (- n 2)))))
        ((not (and (positive? i)
                   (= (gcd a n) 1)
                   (= (modulo (jacobi-symbol a n) n)
                      (modular:expt n a (quotient (- n 1) 2)))))
         (not (positive? i)))))
  (define (primes-gcd? n comps)
    (not (let mapf ((lst comps))
           (or (null? lst) (and (= 1 (gcd n (car lst))) (mapf (cdr lst)))))))
  (define prime:prime-sqr 121)
  (define prime:products '(105))
  (define prime:sieve '#u8(0 0 1 1 0 1 0 1 0 0 0))
  (define most-positive-fixnum #x1fffffff)
  (letrec ((lp (lambda (comp comps primes nexp)
                 (cond ((< comp (quotient most-positive-fixnum nexp))
                        (let ((ncomp (* nexp comp)))
                          (lp ncomp comps
                              (cons nexp primes)
                              (next-prime nexp (cons ncomp comps)))))
                       ((< (quotient comp nexp) (* nexp nexp))
                        (set! prime:prime-sqr (* nexp nexp))
                        (set! prime:sieve (make-u8vector nexp 0))
                        (for-each (lambda (prime)
                                    (u8vector-set! prime:sieve prime 1))
                                  primes)
                        (set! prime:products (reverse (cons comp comps))))
                       (else
                        (lp nexp (cons comp comps)
                            (cons nexp primes)
                            (next-prime nexp (cons comp comps)))))))
           (next-prime (lambda (nexp comps)
                         (set! comps (reverse comps))
                         (do ((nexp (+ 2 nexp) (+ 2 nexp)))
                             ((not (primes-gcd? nexp comps)) nexp)))))
    (lp 3 '() '(2 3) 5))
  (set! n (abs n))
  (cond ((< n (u8vector-length prime:sieve)) (positive? (u8vector-ref prime:sieve n)))
	((even? n) #f)
	((primes-gcd? n prime:products) #f)
	((< n prime:prime-sqr) #t)
	(else (solovay-strassen-prime? n))))

;;! Returns a list of the first prime numbers less than
;; If there are fewer prime numbers than requested, the returned list
;; will have fewer elements
(define (primes< start count)
  (define (prime:prime< start)
    (do ((nbr (+ -1 start) (+ -1 nbr)))
        ((or (negative? nbr) (prime? nbr))
         (if (negative? nbr) #f nbr))))
  (do ((cnt (+ -2 count) (+ -1 cnt))
       (lst '() (cons prime lst))
       (prime (prime:prime< start) (prime:prime< prime)))
      ((or (not prime) (negative? cnt))
       (if prime (cons prime lst) lst))))

;;! Returns a list of the first prime numbers greater than
(define (primes> start count)
  (define (prime> start)
    (do ((nbr (+ 1 start) (+ 1 nbr)))
        ((prime? nbr) nbr)))
  (set! start (max 0 start))
  (do ((cnt (+ -2 count) (+ -1 cnt))
       (lst '() (cons prime lst))
       (prime (prime> start) (prime> prime)))
      ((negative? cnt)
       (reverse (cons prime lst)))))
