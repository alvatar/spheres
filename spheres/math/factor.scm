;;!!! Modular arithmetic
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

(cond-expand
 (gambit (declare (fixnum)))
 (else (void)))

;;! Returns a list of the prime factors of @1.  The order of the
;; factors is unspecified.
;;
;; Lankinen's recursive factoring algorithm:
;; From: ld231782@longs.LANCE.ColoState.EDU (L. Detweiler)
;;
;;                  |  undefined if n<0,
;;                  |  (u,v) if n=0,
;; Let f(u,v,b,n) := | [otherwise]
;;                  |  f(u+b,v,2b,(n-v)/2) or f(u,v+b,2b,(n-u)/2) if n odd
;;                  |  f(u,v,2b,n/2) or f(u+b,v+b,2b,(n-u-v-b)/2) if n even
;;
;; Thm: f(1,1,2,(m-1)/2) = (p,q) iff pq=m for odd m.
;;
;; It may be illuminating to consider the relation of the Lankinen function in
;; a `computational hierarchy' of other factoring functions.*  Assumptions are
;; made herein on the basis of conventional digital (binary) computers.  Also,
;; complexity orders are given for the worst case scenarios (when the number to
;; be factored is prime).  However, all algorithms would probably perform to
;; the same constant multiple of the given orders for complete composite
;; factorizations.
;;
;; Thm: Eratosthenes' Sieve is very roughtly O(ln(n)/n) in time and
;;     O(n*log2(n)) in space.
;; Pf: It works with all prime factors less than n (about ln(n)/n by the prime
;;    number thm), requiring an array of size proportional to n with log2(n)
;;    space for each entry.
;;
;; Thm: `Odd factors' is O((sqrt(n)/2)*log2(n)) in time and O(log2(n)) in
;;     space.
;; Pf: It tests all odd factors less than the square root of n (about
;;    sqrt(n)/2), with log2(n) time for each division.  It requires only
;;    log2(n) space for the number and divisors.
;;
;; Thm: Lankinen's algorithm is O(sqrt(n)/2) in time and O((sqrt(n)/2)*log2(n))
;;     in space.
;; Pf: The algorithm is easily modified to seach only for factors p<q for all
;;    pq=m.  Then the recursive call tree forms a geometric progression
;;    starting at one, and doubling until reaching sqrt(n)/2, or a length of
;;    log2(sqrt(n)/2).  From the formula for a geometric progression, there is
;;    a total of about 2^log2(sqrt(n)/2) = sqrt(n)/2 calls.  Assuming that
;;    addition, subtraction, comparison, and multiplication/division by two
;;    occur in constant time, this implies O(sqrt(n)/2) time and a
;;    O((sqrt(n)/2)*log2(n)) requirement of stack space.
(define (factor k)
  (define (prime:f u v b n)
    (if (<= n 0)
        (cond ((negative? n) #f)
              ((= u 1) #f)
              ((= v 1) #f)
              ;; Do both of these factors need to be factored?
              (else (append (or (prime:f 1 1 2 (quotient (- u 1) 2))
                                (list u))
                            (or (prime:f 1 1 2 (quotient (- v 1) 2))
                                (list v)))))
        (if (even? n)
            (or (prime:f u v (+ b b) (quotient n 2))
                (prime:f (+ u b) (+ v b) (+ b b) (quotient (- n (+ u v b)) 2)))
            (or (prime:f (+ u b) v (+ b b) (quotient (- n v) 2))
                (prime:f u (+ v b) (+ b b) (quotient (- n u) 2))))))
  (define (prime:fo m)
    (let* ((s (gcd m (car prime:products)))
           (r (quotient m s)))
      (if (= 1 s)
          (or (prime:f 1 1 2 (quotient (- m 1) 2)) (list m))
          (append
           (if (= 1 r) '()
               (or (prime:f 1 1 2 (quotient (- r 1) 2)) (list r)))
           (or (prime:f 1 1 2 (quotient (- s 1) 2)) (list s))))))
  (define (prime:fe m)
    (if (even? m)
        (cons 2 (prime:fe (quotient m 2)))
        (if (eqv? 1 m)
            '()
            (prime:fo m))))
  (define (primes-gcd? n comps)
    (not (let mapf ((lst comps))
           (or (null? lst) (and (= 1 (gcd n (car lst))) (mapf (cdr lst)))))))
  (define prime:products '(105))
  (define most-positive-fixnum #x1fffffff)
  (letrec ((lp (lambda (comp comps primes nexp)
                 (cond ((< comp (quotient most-positive-fixnum nexp))
                        (let ((ncomp (* nexp comp)))
                          (lp ncomp comps
                              (cons nexp primes)
                              (next-prime nexp (cons ncomp comps)))))
                       ((< (quotient comp nexp) (* nexp nexp))
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
  (case k
    ((-1 0 1) (list k))
    (else (if (negative? k)
	      (cons -1 (prime:fe (- k)))
	      (prime:fe k)))))
