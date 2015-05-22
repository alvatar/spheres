;;!!! Numerical methods
;; .author Aubrey Jaffer, 1996-2005
;; .author Lars Arvestad
;; .author Alvaro Castro-Castilla, 2015

;; Newton's and Laguerre's methods for finding roots.
;; .author Aubrey Jaffer, 1996-1997
;; Copyright (C) 1996, 1997 Aubrey Jaffer
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

;; Newton's Method explained in:
;; D. E. Knuth, "The Art of Computer Programming", Vol 2 /
;; Seminumerical Algorithms, Reading Massachusetts, Addison-Wesley
;; Publishing Company, 2nd Edition, p. 510
(define (find-integer-root/newton f df/dx x_0)
  (let loop ((x x_0) (fx (f x_0)))
    (cond
     ((zero? fx) x)
     (else
      (let ((df (df/dx x)))
	(cond
	 ((zero? df) #f)		; stuck at local min/max
	 (else
	  (let* ((delta (quotient (+ fx (quotient df 2)) df))
		 (next-x (cond ((not (zero? delta)) (- x delta))
			       ((positive? fx) (- x 1))
			       (else (- x -1))))
		 (next-fx (f next-x)))
	    (cond ((>= (abs next-fx) (abs fx)) x)
		  (else (loop next-x next-fx)))))))))))

;;(define (integer-sqrt y)
;;  (find-integer-root/newton (lambda (x) (- (* x x) y))
;;			    (lambda (x) (* 2 x))
;;			    (ash 1 (quotient (integer-length y) 2))))

(define (find-root/newton f df/dx x_0 prec)
  (if (and (negative? prec) (integer? prec))
      (let loop ((x x_0) (fx (f x_0)) (count prec))
	(cond ((zero? count) x)
	      (else (let ((df (df/dx x)))
		      (cond ((zero? df) #f) ; stuck at local min/max
			    (else (let* ((next-x (- x (/ fx df)))
					 (next-fx (f next-x)))
				    (cond ((= next-x x) x)
					  ((> (abs next-fx) (abs fx)) #f)
					  (else (loop next-x next-fx
						      (+ 1 count)))))))))))
      (let loop ((x x_0) (fx (f x_0)))
	(cond ((< (abs fx) prec) x)
	      (else (let ((df (df/dx x)))
		      (cond ((zero? df) #f) ; stuck at local min/max
			    (else (let* ((next-x (- x (/ fx df)))
					 (next-fx (f next-x)))
				    (cond ((= next-x x) x)
					  ((> (abs next-fx) (abs fx)) #f)
					  (else (loop next-x next-fx))))))))))))

;;; H. J. Orchard, "The Laguerre Method for Finding the Zeros of
;;; Polynomials", IEEE Transactions on Circuits and Systems, Vol. 36,
;;; No. 11, November 1989, pp 1377-1381.
(define (find-root/laguerre f df/dz ddf/dz^2 z_0 prec)
  (if (and (negative? prec) (integer? prec))
      (let loop ((z z_0) (fz (f z_0)) (count prec))
	(cond ((zero? count) z)
	      (else
	       (let* ((df (df/dz z))
		      (ddf (ddf/dz^2 z))
		      (disc (sqrt (- (* df df) (* fz ddf)))))
		 (if (zero? disc)
		     #f
		     (let* ((next-z
			     (- z (/ fz (if (negative? (+ (* (real-part df)
							     (real-part disc))
							  (* (imag-part df)
							     (imag-part disc))))
					    (- disc) disc))))
			    (next-fz (f next-z)))
		       (cond ((>= (magnitude next-fz) (magnitude fz)) z)
			     (else (loop next-z next-fz (+ 1 count))))))))))
      (let loop ((z z_0) (fz (f z_0)) (delta-z #f))
	(cond ((< (magnitude fz) prec) z)
	      (else
	       (let* ((df (df/dz z))
		      (ddf (ddf/dz^2 z))
		      (disc (sqrt (- (* df df) (* fz ddf)))))
		 ;;(print 'disc disc)
		 (if (zero? disc)
		     #f
		     (let* ((next-z
			     (- z (/ fz (if (negative? (+ (* (real-part df)
							     (real-part disc))
							  (* (imag-part df)
							     (imag-part disc))))
					    (- disc) disc))))
			    (next-delta-z (magnitude (- next-z z))))
		       ;;(print 'next-z next-z )
		       ;;(print '(f next-z) (f next-z))
		       ;;(print 'delta-z delta-z 'next-delta-z next-delta-z)
		       (cond ((zero? next-delta-z) z)
			     ((and delta-z (>= next-delta-z delta-z)) z)
			     (else
			      (loop next-z (f next-z) next-delta-z)))))))))))

(define (find-polynomial-root/laguerre deg f df/dz ddf/dz^2 z_0 prec)
  (if (and (negative? prec) (integer? prec))
      (let loop ((z z_0) (fz (f z_0)) (count prec))
	(cond ((zero? count) z)
	      (else
	       (let* ((df (df/dz z))
		      (ddf (ddf/dz^2 z))
		      (tmp (* (+ deg -1) df))
		      (sqrt-H (sqrt (- (* tmp tmp) (* deg (+ deg -1) fz ddf))))
		      (df+sqrt-H (+ df sqrt-H))
		      (df-sqrt-H (- df sqrt-H))
		      (next-z
		       (- z (/ (* deg fz)
			       (if (>= (magnitude df+sqrt-H)
				       (magnitude df-sqrt-H))
				   df+sqrt-H
				   df-sqrt-H)))))
		 (loop next-z (f next-z) (+ 1 count))))))
      (let loop ((z z_0) (fz (f z_0)))
	(cond ((< (magnitude fz) prec) z)
	      (else
	       (let* ((df (df/dz z))
		      (ddf (ddf/dz^2 z))
		      (tmp (* (+ deg -1) df))
		      (sqrt-H (sqrt (- (* tmp tmp) (* deg (+ deg -1) fz ddf))))
		      (df+sqrt-H (+ df sqrt-H))
		      (df-sqrt-H (- df sqrt-H))
		      (next-z
		       (- z (/ (* deg fz)
			       (if (>= (magnitude df+sqrt-H)
				       (magnitude df-sqrt-H))
				   df+sqrt-H
				   df-sqrt-H)))))
		 (loop next-z (f next-z))))))))

(define (find-root-1/secant f x0 x1 prec must-bracket?)
  (letrec ((stop?
	    (cond ((procedure? prec) prec)
		  ((and (integer? prec) (negative? prec))
		   (lambda (x0 f0 x1 f1 count)
		     (>= count (- prec))))
		  (else
		   (lambda (x0 f0 x1 f1 count)
		     (and (< (abs f0) prec)
			  (< (abs f1) prec))))))
	   (bracket-iter
	    (lambda (xlo flo glo xhi fhi ghi count)
	      (define (step xnew fnew)
		(cond ((or (= xnew xlo)
			   (= xnew xhi))
		       (let ((xmid (+ xlo (* 1/2 (- xhi xlo)))))
			 (if (= xnew xmid)
			     xmid
			     (step xmid (f xmid)))))
		      ((positive? fnew)
		       (bracket-iter xlo flo (if glo (* 0.5 glo) 1)
				     xnew fnew #f
				     (+ count 1)))
		      (else
		       (bracket-iter xnew fnew #f
				     xhi fhi (if ghi (* 0.5 ghi) 1)
				     (+ count 1)))))
	      (if (stop? xlo flo xhi fhi count)
		  (if (> (abs flo) (abs fhi)) xhi xlo)
		  (let* ((fflo (if glo (* glo flo) flo))
			 (ffhi (if ghi (* ghi fhi) fhi))
			 (del (- (/ fflo (- ffhi fflo))))
			 (xnew (+ xlo (* del (- xhi xlo))))
			 (fnew (f xnew)))
		    (step xnew fnew))))))
    (let ((f0 (f x0))
	  (f1 (f x1)))
      (cond ((<= f0 0 f1)
	     (bracket-iter x0 f0 #f x1 f1 #f 0))
	    ((<= f1 0 f0)
	     (bracket-iter x1 f1 #f x0 f0 #f 0))
	    (must-bracket? #f)
	    (else
	     (let secant-iter ((x0 x0)
			       (f0 f0)
			       (x1 x1)
			       (f1 f1)
			       (count 0))
	       (cond ((stop? x0 f0 x1 f1 count)
		      (if (> (abs f0) (abs f1)) x1 x0))
		     ((<= f0 0 f1)
		      (bracket-iter x0 f0 #f x1 f1 #f count))
		     ((>= f0 0 f1)
		      (bracket-iter x1 f1 #f x0 f0 #f count))
		     ((= f0 f1) #f)
		     (else
		      (let ((xnew (+ x0 (* (- (/ f0 (- f1 f0))) (- x1 x0)))))
			(secant-iter x1 f1 xnew (f xnew) (+ count 1)))))))))))

(define (find-root/secant f x0 x1 prec)
  (find-root-1/secant f x0 x1 prec #f))

(define (find-bracketed-root/secant f x0 x1 prec)
  (find-root-1/secant f x0 x1 prec #t))


;;-------------------------------------------------------------------------------
;;!! One-side limit algorithm.
;; .author Aubrey Jaffer, 2005
;;
;; Copyright 2005 Aubrey Jaffer
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

(define (finite? val)
  (and (not (= val (+ 1 val val)))
       (= val val)))

(define (inv-root f1 f2 f3 prec)
  (define f1^2 (* f1 f1))
  (define f2^2 (* f2 f2))
  (define f3^2 (expt f3 2))
  (find-root/newton (lambda (f0)
		      (+ (- (* (expt f0 2) f1))
			 (* f0 f1^2)
			 (* (- (* 2 (expt f0 2)) (* 3 f1^2)) f2)
			 (* (+ (- (* 2 f0)) (* 3 f1)) f2^2)
			 (* (- (+ (- (expt f0 2)) (* 2 f1^2)) f2^2)
			    f3)
			 (* (+ (- f0 (* 2 f1)) f2) f3^2)))
		    (lambda (f0)
		      (+ (- (+ (* -2 f0 f1) f1^2 (* 4 f0 f2))
			    (* 2 f2^2)
			    (* 2 f0 f3))
			 f3^2))
		    f1
		    prec))

(define (invintp f1 f2 f3)
  (define f1^2 (* f1 f1))
  (define f2^2 (* f2 f2))
  (define f3^2 (expt f3 2))
  (let ((c (+ (* -3 f1^2 f2)
	      (* 3 f1 f2^2)
	      (* (- (* 2 f1^2) f2^2) f3)
	      (* (- f2 (* 2 f1)) f3^2)))
	(b (+ (- f1^2 (* 2 f2^2)) f3^2))
	(a (- (* 2 f2) f1 f3)))
    (define disc (- (* b b) (* 4 a c)))
    ;;(printf "discriminant: %g\n" disc)
        (if (negative? (real-part disc))
	(/ b -2 a)
	(let ((sqrt-disc (sqrt disc)))
	  (define root+ (/ (- sqrt-disc b) 2 a))
	  (define root- (/ (+ sqrt-disc b) -2 a))
	  (if (< (magnitude (- root+ f1)) (magnitude (- root- f1)))
	      root+
	      root-)))))

(define (extrapolate-0 fs)
  (define n (length fs))
  (define (choose n k)
    (do ((kdx 1 (+ 1 kdx))
	 (prd 1 (/ (* (- n kdx -1) prd) kdx)))
	((> kdx k) prd)))
  (do ((k 1 (+ 1 k))
       (lst fs (cdr lst))
       (L 0 (+ (* -1 (expt -1 k) (choose n k) (car lst)) L)))
      ((null? lst) L)))

(define (sequence->limit proc sequence)
  (define lval (proc (car sequence)))
  (if (finite? lval)
      (let ((val (proc (cadr sequence))))
	(define h_n*nsamps (* (length sequence) (magnitude (- val lval))))
	(if (finite? val)
	    (let loop ((sequence (cddr sequence))
		       (fxs (list val lval))
		       (trend #f)
		       (ldelta (- val lval))
		       (jdx (+ -1 (length sequence))))
	      (cond ((null? sequence)
		     (case trend
		       ((diverging) (and (real? val) (/ ldelta 0.0)))
		       ((bounded) (invintp val lval (caddr fxs)))
		       (else (cond ((zero? ldelta) val)
				   ((not (real? val)) #f)
				   (else (extrapolate-0 fxs))))))
		    (else
		     (set! lval val)
		     (set! val (proc (car sequence)))
		     ;;(printf "f(%12g)=%12g; delta=%12g hyp=%12g j=%3d %s\n" (car sequence) val (- val lval) (/ h_n*nsamps jdx) jdx (or trend ""))
		     (if (finite? val)
			 (let ((delta (- val lval)))
			   (define h_j (/ h_n*nsamps jdx))
			   (cond ((case trend
				    ((converging) (<= (magnitude delta) h_j))
				    ((bounded)    (<= (magnitude ldelta) (magnitude delta)))
				    ((diverging)  (>= (magnitude delta) h_j))
				    (else #f))
				  (loop (cdr sequence) (cons val fxs) trend delta (+ -1 jdx)))
				 (trend #f)
				 (else
				  (loop (cdr sequence) (cons val fxs)
					(cond ((> (magnitude delta) h_j) 'diverging)
					      ((< (magnitude ldelta) (magnitude delta)) 'bounded)
					      (else 'converging))
					delta (+ -1 jdx)))))
			 (and (eq? trend 'diverging) val)))))
	    (and (real? val) val)))
      (and (real? lval) lval)))

(define (limit proc x1 x2 . k)
  (set! k (if (null? k) 8 (car k)))
  (cond ((not (finite? x2)) (error 'limit 'infinite 'x2 x2))
	((not (finite? x1))
	 (or (positive? (* x1 x2)) (error 'limit 'start 'mismatch x1 x2))
	 (limit (lambda (x) (proc (/ x))) 0.0 (/ x2) k))
	((= x1 (+ x1 x2)) (error 'limit 'null 'range x1 (+ x1 x2)))
	(else (let ((dec (/ x2 k)))
		(do ((x (+ x1 x2 0.0) (- x dec))
		     (cnt (+ -1 k) (+ -1 cnt))
		     (lst '() (cons x lst)))
		    ((negative? cnt)
		     (sequence->limit proc (reverse lst))))))))


;;-------------------------------------------------------------------------------
;; Minimum finding f(x) for x0 <= x <= x1.
;; Author: Lars Arvestad
;;
;; This code is in the public domain.


;;! The Golden Section Search
;; David Kahaner, Cleve Moler, and Stephen Nash
;; Numerical Methods and Software Prentice-Hall, 1989, ISBN 0-13-627258-4
;; The algorithm finds minima of functions which are expensive to compute or for
;; which derivatives are not available. Although optimum for the general case,
;; convergence is slow, requiring nearly 100 iterations for the example
;; (x^3-2x-5).
;;
;; If the derivative is available, Newton-Raphson is probably a better
;; choice.  If the function is inexpensive to compute, consider
;; approximating the derivative.
;;
;; x_0 are x_1 real numbers.  The (single argument) procedure f is unimodal over
;; the open interval (x_0, x_1). That is, there is exactly one point in the
;; interval for which the derivative of f is zero.
;;
;; returns a pair (x . f(x)) where f(x) is the minimum.  The prec parameter is
;; the stop criterion. If prec is a positive number, then the iteration continues
;; until x is within prec from the true value.  If prec is a negative integer,
;; then the procedure will iterate -prec times or until convergence. If prec is
;; a procedure of seven arguments, x0, x1, a, b, fa, fb, and count, then the
;; iterations will stop when the procedurereturns #t.
;;
;; Analytically, the minimum of x^3-2x-5 is 0.816497.
;; Examples:
;; (define func (lambda (x) (+ (* x (+ (* x x) -2)) -5)))
;; (golden-section-search func 0 1 (/ 10000))
;;      ==> (816.4883855245578e-3 . -6.0886621077391165)
;; (golden-section-search func 0 1 -5)
;;      ==> (819.6601125010515e-3 . -6.088637561916407)
;; (golden-section-search func 0 1
;;                       (lambda (a b c d e f g ) (= g 500)))
;;      ==> (816.4965933140557e-3 . -6.088662107903635)
(define golden-section-search
  (let ((gss 'golden-section-search:)
	(r (/ (- (sqrt 5) 1) 2)))	; 1 / golden-section
    (lambda (f x0 x1 prec)
      (cond ((not (procedure? f)) (error gss 'procedure? f))
	    ((not (number? x0)) (error gss 'number? x0))
	    ((not (number? x1)) (error gss 'number? x1))
	    ((>= x0 x1) (error gss x0 'not '< x1)))
      (let ((stop?
	     (cond
	      ((procedure? prec) prec)
	      ((number? prec)
	       (if (>= prec 0)
		   (lambda (x0 x1 a b fa fb count) (<= (abs (- x1 x0)) prec))
		   (if (integer? prec)
		       (lambda (x0 x1 a b fa fb count) (>= count (- prec)))
		       (error gss 'integer? prec))))
	      (else (error gss 'procedure? prec))))
	    (a0 (+ x0 (* (- x1 x0) (- 1 r))))
	    (b0 (+ x0 (* (- x1 x0) r)))
	    (delta #f)
	    (fmax #f)
	    (fmin #f))
	(let loop ((left x0)
		   (right x1)
		   (a a0)
		   (b b0)
		   (fa (f a0))
		   (fb (f b0))
		   (count 1))
	  (define finish
	    (lambda (x fx)
	      (if (> fx fmin) (println gss fx 'not 'min (list '> fmin)))
	      (if (and (> count 9) (or (eqv? x0 left) (eqv? x1 right)))
		  (println gss 'min 'not 'found))
	      (cons x fx)))
	  (case count
	    ((1)
	     (set! fmax (max fa fb))
	     (set! fmin (min fa fb)))
	    ((2)
	     (set! fmin (min fmin fa fb))
	     (if (= fmax fa fb) (error gss 'flat? fmax)))
	    (else
	     (set! fmin (min fmin fa fb))))
	  (cond ((stop? left right a b fa fb count)
		 (if (< fa fb)
		     (finish a fa)
		     (finish b fb)))
		((< fa fb)
		 (let ((a-next (+ left (* (- b left) (- 1 r)))))
		   (cond ((and delta (< delta (- b a)))
			  (finish a fa))
			 (else (set! delta (- b a))
			       (loop left b a-next a (f a-next) fa
				     (+ 1 count))))))
		(else
		 (let ((b-next (+ a (* (- right a) r))))
		   (cond ((and delta (< delta (- b a)))
			  (finish b fb))
			 (else (set! delta (- b a))
			       (loop a right b b-next fb (f b-next)
				     (+ 1 count))))))))))))
