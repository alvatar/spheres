;;; Copyright (c) 2012-2014 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Arithmetic definitions and procedures


;;------------------------------------------------------------------------------
;;!! Constants

(define pi (angle -inf.0))
(define pi2 (fl* #i2 pi))
(define pi/2 (fl/ pi #i2))
(define -pi/2 (fl/ pi #i-2))
(define pi/4 (fl/ pi #i4))
(define -pi/4 (fl/ pi #i-4))
(define pi3/4 (fl* #i3 (fl/ #i4 pi)))

(define e #i2.71828182845904523536)
(define log2e #i1.44269504088896340736)
(define log10e #i0.434294481903251827651)
(define ln2 #i0.693147180559945309417)
(define ln10 #i2.30258509299404568402)
(define sqrt2 #i1.41421356237309504880)
(define sqrt1/2 #i0.707106781186547524401)

(define euler #i0.5772156649)


;;------------------------------------------------------------------------------
;;!! Numeric

;;! Generic conversion to integer number
(define ->integer
  (let ((fixnum-max-as-flonum (##fixnum->flonum ##max-fixnum)))
    (lambda (n)
      (declare (not safe))
      (cond
       ((##fixnum? n) n)
       ((##bignum? n) n)           ; Bignums are integer by definition
       ((##flonum? n) (if (##fl< n fixnum-max-as-flonum)
                          (##flonum->fixnum n)
                          (##flonum->exact-int n)))
       ((##ratnum? n) (##inexact->exact (##floor n)))
       ((##complex? n) (error "complex->integer number conversion not supported"))
       (else (error "Generic ->integer conversion only implemented for numbers"))))))

;;------------------------------------------------------------------------------
;;!! Aritmethics

;;! Computes the sum of all values
(define (sum l) (apply + l))

;;! Computes the product of all values
(define (product l) (apply * l))

;;! Inverse function
(define inverse ##inverse)

;;! Square
(define (square x) (* x x))

;;! Exact random
(define (random-exact)
  (inexact->exact (random-real)))

;;! Exact random from -1 to +1
(define (random-exact/-1/+1)
  (inexact->exact (fl+ -1.0 (fl* (random-real) 2.0))))

;;! Extended-gcd(a,b) = (x,y), such that a*x + b*y = gcd(a,b)
;; Test alternative:
;; (define (extended-gcd a b)
;;   (if (= (modulo a b) 0)
;;       (cons 0 1)
;;       (let* ((x:y (extended-gcd b (modulo a b)))
;;              (x (car x:y))
;;              (y (cdr x:y)))
;;         (cons y (- x (* y (quotient a b)))))))
(define (extended-gcd x y)
  (let loop ((x x) (y y)
             (u1 1) (u2 0)
             (v1 0) (v2 1))
    (if (zero? y)
        (list x u1 v1)
        (let ((q (quotient x y))
              (r (remainder x y)))
          (loop y r
                u2 (- u1 (* q u2))
                v2 (- v1 (* q v2)))))))

;;! Modulo-inverse(a,n) = b, such that a*b = 1 [mod n].
;; Test alternative:
;; (define (modulo-inverse a n)
;;   (modulo (car (extended-gcd a n)) n))
(define (modulo-inverse x b)
  (let* ((x1 (modulo x b))
         (g (extended-gcd x1 b)))
    (if (not (= (car g) 1))
        (error "internal error, numbers are not relatively prime" x b)
        (modulo (cadr g) b))))

;;! Computes x^y mod m
(define (expt-mod x y m)
  (define (expt-mod-aux n e m)
    (cond ((zero? e) 1)
          ((even? e)
           (expt-mod-aux
            (modulo (* n n) m)
            (quotient e 2)
            m))
          (else
           (modulo
            (* n (expt-mod-aux n (- e 1) m))
            m))))
  (expt-mod-aux x y m))

;;! Totient(n) = (p - 1)*(q - 1), 
;;  where pq is the prime factorization of n.
(define (totient p q) (* (- p 1) (- q 1)))

;;! Modulo-power(base,exp,n) = base^exp [mod n]
(define (modulo-power base exp n)
  (if (= exp 0)
      1
      (if (odd? exp)
          (modulo (* base (modulo-power base (- exp 1) n)) n)
          (modulo (square (modulo-power base (/ exp 2) n)) n))))

;;; Factorial
;;; long int fac(unsigned long int n) {
;;;     return lround(exp(lgamma(n+1)));
;;;     }

;;! Generates a random prim in a range
(define* (random-prime start end (show-trace #f))
  (if show-trace
      (begin
        (display ".")
        (force-output)))
  (let* ((product-of-primes
          (lambda (n)
            (let loop ((n (- n 1)) (p 2) (i 3))
              (cond ((= n 0)
                     p)
                    ((= 1 (gcd i p))
                     (loop (- n 1) (* p i) (+ i 2)))
                    (else
                     (loop n p (+ i 2)))))))
         (prod-small-primes (product-of-primes 300)))
    (define (likely-prime? n)
      (and (= 1 (gcd n prod-small-primes))
           (= 1 (expt-mod 2 (- n 1) n))))
    (let loop ((i 1))
      (if show-trace
          (begin
            (if (= 0 (modulo i 10)) (newline))
            (display "+")
            (force-output)))
      (let* ((x (+ start (random-integer (- end start))))
             (n (if (odd? x) x (+ x 1))))
        (if (or (>= n end)
                (not (likely-prime? n)))
            (loop (+ i 1))
            (begin
              (if show-trace (newline))
              n))))))
