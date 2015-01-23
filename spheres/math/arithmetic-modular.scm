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
 (gambit (declare (mostly-fixnum)))
 (else (void)))

;;! Returns a list of 3 integers (d x y) such that
;; d = gcd(n1,n2) = n1 * x + n2 * y
(define (modular:extended-euclid x y)
  (define q 0)
  (do ((r0 x r1) (r1 y (remainder r0 r1))
       (u0 1 u1) (u1 0 (- u0 (* q u1)))
       (v0 0 v1) (v1 1 (- v0 (* q v1))))
      ((zero? r1) (list r0 u0 v0))
    (set! q (quotient r0 r1))))

;;! For odd positive integer m, returns an object suitable for passing as the
;; first argument to modular: procedures, directing them to return a symmetric
;; modular number, ie. an @var{n} such that (<= (quotient m -2) n (quotient m 2))
(define (modular:symmetric-modulus m)
  (cond ((or (not (number? m)) (not (positive? m)) (even? m))
	 (error 'modular:symmetric-modulus m))
	(else (quotient (+ -1 m) -2))))

;;! Returns the non-negative integer characteristic of the ring formed when
;; modulus is used with modular: procedures
(define (modular:characteristic m)
  (cond ((negative? m) (- 1 (+ m m)))
	((zero? m) #f)
	(else m)))

;;! Returns the integer (modulo n (modular:characteristic modulus)) in the
;; representation specified by modulus
(define modular:normalize
  (lambda (m k)
    (cond ((positive? m) (modulo k m))
          ((zero? m) k)
          ((<= m k (- m)) k)
          (else
           (let* ((pm (+ 1 (* -2 m)))
                  (s (modulo k pm)))
             (if (<= s (- m)) s (- s pm)))))))

;;!! The rest of these functions assume normalized arguments. For all of these
;; functions, if the first argument (@var{modulus}) is:
;; positive?
;; Integers mod modulus.  The result is between 0 and modulus.
;; zero?
;; The arguments are treated as integers.  An integer is returned.
;;
;; Otherwise, if modulus is a value returned by (symmetric:modulus @var{radix}),
;; then the arguments and result are treated as members of the integers modulo
;; radix, but with symmetric representation.
;; Example
;; (<= (quotient radix 2) n (quotient (- -1 radix) 2)
;; If all the arguments are fixnums the computation will use only fixnums.

;; Returns #t if there exists an integer n such that k * n = 1 mod modulus,
;; and #f otherwise
;; .parameter m modulus
;; .parameter a k
(define (modular:invertable? m a)
  (eqv? 1 (gcd (or (modular:characteristic m) 0) a)))

;;! Returns an integer n such that 1 = (n * n2) mod modulus. If n2 has no
;; inverse mod @var{modulus} an error is signaled.
;; .parameter m modulus
;; .parameter a n2
(define (modular:invert m a)
  (define (err) (error 'modular:invert "can't invert" m a))
  (cond ((eqv? 1 (abs a)) a)		; unit
	(else
	 (let ((pm (modular:characteristic m)))
	   (cond
	    (pm
	     (let ((d (modular:extended-euclid (modular:normalize pm a) pm)))
	       (if (= 1 (car d))
		   (modular:normalize m (cadr d))
		   (err))))
	    (else (err)))))))

;;! Returns -n2 mod modulus
;; .parameter m modulus
;; .parameter a n2
(define (modular:negate m a)
  (cond ((zero? a) 0)
	((negative? m) (- a))
	(else (- m a))))

;; Being careful about overflow here

;;! Returns (n2 + n3) mod modulus
;; .parameter m modulus
;; .parameter a n2
;; .parameter b n3
(define (modular:+ m a b)
  (cond ((positive? m) (modulo (+ (- a m) b) m))
	((zero? m) (+ a b))
	;; m is negative
	((negative? a)
	 (if (negative? b)
	     (let ((s (+ (- a m) b)))
	       (if (negative? s)
		   (- s (+ -1 m))
		   (+ s m)))
	     (+ a b)))
	((negative? b) (+ a b))
	(else (let ((s (+ (+ a m) b)))
		(if (positive? s)
		    (+ s -1 m)
		    (- s m))))))

;;! Returns (n2 - n3) mod modulus
;; .parameter m modulus
;; .parameter a n2
;; .parameter b n3
(define (modular:- m a b)
  (modular:+ m a (modular:negate m b)))

;; See: L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
;; with Splitting Facilities." ACM Transactions on Mathematical
;; Software, 17:98-111 (1991)


;;! Returns (n2 * n3) mod modulus
;; .parameter m modulus
;; .parameter a n2
;; .parameter b n3
(define modular:*
  (lambda (m a b)
    (cond ((zero? m) (* a b))
          ((positive? m) (modulo (* a b) m))
          (else (modular:normalize m (* a b))))))

;;! Returns base ^ xpn mod m
;; .parameter modulus
;; .parameter base
;; .parameter exponent
(define (modular:expt m base xpn)
  (cond ((zero? m) (expt base xpn))
	((= base 1) 1)
	((if (negative? m) (= -1 base) (= (- m 1) base))
	 (if (odd? xpn) base 1))
	((negative? xpn)
	 (modular:expt m (modular:invert m base) (- xpn)))
	((zero? base) 0)
	(else
	 (do ((x base (modular:* m x x))
	      (j xpn (quotient j 2))
	      (acc 1 (if (even? j) acc (modular:* m x acc))))
	     ((<= j 1)
	      (case j
		((0) acc)
		((1) (modular:* m x acc))))))))
