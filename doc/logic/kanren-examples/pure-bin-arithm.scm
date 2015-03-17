;	     Pure, declarative, and constructive binary arithmetics
;
; aka: Addition, Multiplication, Division with the remainder, Discrete
; Logarithm as sound and complete, pure and declarative relations that
; can be used in any mode whatsoever and that recursively enumerate
; their domains.  The relations define arithmetics over base-2
; non-negative numerals of *arbitrary* size.
;
; In particular, we define the function divo such that (divo n m q r) 
; succeeds if and only if n = m*q + r and 0<=r<m. We run the following
; Kanren queries:
;   -- (divo (build 1) (build 0) q _)
;	It fails and does not try to enumerate all natural numbers.
;   -- (divo (build 5) M (build 1) _)
;	It finds all such M that divide (perhaps unevenly)
;       5 with the quotient of 1. The answer is the set (5 4 3).
; (divo (build 5) m (build 7) _) simply fails and does not loop forever.
; We can use our (**o X Y Z) relation either to multiply two numbers
; X and Y -- or to find all factorizations of Z. See the test below.
; Furthermore, we can evaluate (++o X 1 Y) and get the stream
; of answers, among which is ((0 _.0 . _.1) (1 _.0 . _.1))
; which essentially says that 2*x and 2*x +1 are successors, for all x>0!
;
; The relation 'divo' encompasses all four operations of arithmetics:
; we can use (divo x y z zero) to multiply and divide and
; (divo x y one r) to add and subtract.
;
; Our logo relation has the property that (logo n b q r) succeeds if
; and only if n = b^q + r where 0 <= r and q is the largest such integer.
; We can use logo to exponentiate, to find discrete logarithms, or
; to find the logarithm base.
;
; See pure-arithm.scm in this directory for Peano arithmetics.
;
; We give two implementations of addition and multiplication
; relations, `++o' and `**o'. Both versions have the properties of
; soundness and neatly refutational completeness. The first version of `++o'
; is faster, but it does not always recursively enumerate its domain
; if that domain is infinite.  This is the case when, e.g., (**o x y
; z) is invoked when all three x, y, and z are uninstantiated
; variables. The relation in that case has the infinite number of
; solutions, as expected. Alas, those solutions look as follows:
;	x = 2,  y = 3, z = 6
;	x = 4,  y = 3, z = 12
;	x = 8,  y = 3, z = 24
;	x = 16, y = 3, z = 48
; That is, (**o x y z) keeps generating solutions where x is a power of
; two. Therefore, when the answerset of the relation `**o' is infinite, it
; truly produces an infinite set of solutions -- but only the subset of
; all possible solutions. In other words, `**o' does not recursively
; enumerate the set of all numbers such that x*y=z if that set is infinite.
;
; Therefore, 
;   (all (== x '(1 1)) (== y '(1 1)) (**o x y z))
;   (all (**o x y z)   (== x '(1 1)) (== y '(1 1)))
; work differently. The former terminates and binds z to the representation
; of 9 (the product of 3 and 3). The latter fails to terminate.
; This is not generally surprising as `all', like 'commas' in Prolog, 
; is not truly a conjunction: they are not commutative. Still, 
; we would like our `++o' and `**o' to have the algebraic properties
; expected of addition and multiplication.
;
; The second version of `++o' and `**o' completely fixes the
; problem without losing any performance.  The addition and
; multiplication relations completely enumerate their domain, even if
; it is infinite. Furthermore, ++o and **o now generate the numbers
; _in sequence_, which is quite pleasing. We achieve the
; property of recursive enumerability without giving up neither
; completeness nor refutational completeness. As before, if 'z' is
; instantiated but 'x' and 'y' are not, (++o x y z) delivers *all*
; non-negative numbers that add to z and (**o x y z) computes *all*
; factorizations of z.
;
; Such relations are easy to implement in an impure system such as Prolog,
; with the help of a predicate 'var'. The latter can tell if its argument
; is an uninstantiated variable. However, 'var' is impure. The present
; file shows the implementation of arithmetic relations in a _pure_
; logic system.
;
; The present approach places the correct upper bounds on the
; generated numbers to make sure the search process will terminate.
; Therefore, our arithmetic relations are not only sound
; (e.g., if (**o X Y Z) holds then it is indeed X*Y=Z) but also
; complete (if X*Y=Z is true then (**o X Y Z) holds) and
; nearly refutationally complete (if X*Y=Z is false and X, Y, and Z
; are either fully instantiated, or not instantiated, then (**o X Y Z) fails,
; in finite time). The refutational completeness
; claim is limited to the case when all terms passed to arithmetical
; functions do not share variables, are either fully instantiated or not
; instantiated at all. Indeed, sharing of variables or partial
; instantiation essentially imposes the constraint: e.g.,
;   (solution (q) (**o `(1 . ,q) `(1 1) `(1 . ,q)))
; is tantamount to
; (solution (q) (exist (q1)
;         (all (**o `(1 . ,q) `(1 1) `(1 . ,q1)) (== q q1))))
; That conjunction will never succeed. See the corresponding Prolog
; code for justification and relation to the 10th Hilbert problem.
;
; The numerals are represented in the binary little-endian
; (least-significant bit first) notation. The higher-order bit must be 1.
; ()  represents 0
; (1) represents 1
; (0 1) represents 2
; (1 1) represents 3
; (0 0 1) represents 4
; etc.
;


; There is a Prolog version of this code, which has termination proofs.
;
; $Id: pure-bin-arithm.scm,v 4.52 2009/03/11 04:12:37 oleg Exp $

; Auxiliary functions to build and show binary numerals
;
(define (build n)
  (if (zero? n) '() (cons (if (even? n) 0 1) (build (quotient n 2)))))

(define (trans n)
  (if (null? n) 0 (+ (car n) (* 2 (trans (cdr n))))))


; (zeroo x) holds if x is zero numeral
(define zeroo
  (fact () '()))

; Not a zero
(define pos
  (fact () `(,_ . ,_)))

; At least two
(define gt1
  (fact () `(,_ ,_ . ,_)))

; compare the lengths of two numerals
; (<ol a b) 
; holds if a=0 and b>0, or if (floor (log2 a)) < (floor (log2 b))
; That is, we compare the length (logarithms) of two numerals
; For a positive numeral, its bitlength = (floor (log2 n)) + 1
; We also make sure that 'n' is a well-formed number.
(define <ol
  (extend-relation (n m)
    (fact () '() `(,_ . ,_))
    (fact () '(1) `(,_ ,_ . ,_))
    (relation (x y x1 y1) (to-show `(,_ ,x1 . ,x) `(,_ ,y1 . ,y))
      (<ol `(,x1 . ,x) `(,y1 . ,y)))))

; holds if both a and b have the same number of bits, i.e., they are zero
; or if (floor (log2 a)) = (floor (log2 b))
(define =ol
  (extend-relation (n m)
    (fact () '() '())
    (fact () '(1) '(1))
    (relation (x y x1 y1) (to-show `(,_ ,x1 . ,x) `(,_ ,y1 . ,y))
      (=ol `(,x1 . ,x) `(,y1 . ,y)))))

; (<ol3 p1 p n m) holds iff
; p1 = 0 and p > 0 or
; length(p1) < min(length(p), length(n) + length(m) + 1)
(define <ol3
  (relation (head-let p1 p n m)
    (any
      (all (== p1 '()) (pos p))
      (exists (p1r pr)
	(all
	  (== p1 `(,_ . ,p1r))
	  (== p  `(,_ . ,pr))
	  (any-interleave
	    (exists (mr)
	      (all (== n '()) (== m  `(,_ . ,mr)) 
		(<ol3 p1r pr n mr)))
	    (exists (nr)
	      (all (== n  `(,_ . ,nr)) 
		(<ol3 p1r pr nr m)))
	    ))))))

; (<ol2 p n m) holds iff
; length(n) + length(m) -1 <= length(p) <= length(n) + length(m)
; This predicate has nice properties: see the corresponding Prolog
; code for proofs.
(define <ol2
  (relation (head-let p n m)
    (any-interleave
      (all (== p '()) (== n '()) (== m '()))
      (all (== p '()) (== n '()) (== m '(1)))
      (all (== p '()) (== n '(1)) (== m '()))
      (exists (pr mr)
	(all
	  (== p `(,_ . ,pr)) (== n '()) (== m `(,_ . ,mr))
	  (<ol2 pr '() mr)))
      (exists (pr nr)
	(all
	  (== p `(,_ . ,pr)) (== n `(,_ . ,nr))
	  (<ol2 pr nr m)))
      )))


; Half-adder: carry-in a b r carry-out
; The relation holds if
; carry-in + a + b = r + 2*carry-out
; where carry-in a b r carry-out are all either 0 or 1.

(define half-adder
  (extend-relation (carry-in a b r carry-out)
    (fact () 0 0 0 0 0)
    (fact () 0 1 0 1 0)
    (fact () 0 0 1 1 0)
    (fact () 0 1 1 0 1)

    (fact () 1 0 0 1 0)
    (fact () 1 1 0 0 1)
    (fact () 1 0 1 0 1)
    (fact () 1 1 1 1 1)
))

; full-adder: carry-in a b r
; holds if carry-in + a + b = r
; where a, b, and r are binary numerals and carry-in is either 0 or 1

; We do the addition bit-by-bit starting from the least-significant
; one. So, we have already two cases to consider per each number: The
; number has no bits, and the number has some bits.

(define full-adder
  (extend-relation (carry-in a b r)
    (fact (a) 0 a '() a) 		; 0 + a + 0 = a
    (relation (b)			; 0 + 0 + b = b
      (to-show 0 '() b b)
      (pos b))
    (relation (head-let '1 a '() r)	; 1 + a + 0 = 0 + a + 1
      (full-adder 0 a '(1) r))
    (relation (head-let '1 '() b r)	; 1 + 0 + b = 0 + 1 + b
      (all (pos b)
	(full-adder 0 '(1) b r)))

    ; The following three relations are needed
    ; to make all numbers well-formed by construction,
    ; that is, to make sure the higher-order bit is one.
    (relation (head-let carry-in '(1) '(1) r)	; c + 1 + 1 >= 2
      (exists (r1 r2)
	(all (== r `(,r1 ,r2))
	     (half-adder carry-in 1 1 r1 r2))))

    ; cin + 1 + (2*br + bb) = (2*rr + rb) where br > 0 and so is rr > 0
    (relation (carry-in bb br rb rr)
      (to-show carry-in '(1) `(,bb . ,br) `(,rb . ,rr))
      (all
	(pos br) (pos rr)
	(exists (carry-out)
	  (all
	    (half-adder carry-in 1 bb rb carry-out)
	    (full-adder carry-out '() br rr)))))

    ; symmetric case for the above
    (relation (head-let carry-in a '(1) r)
      (all
	(gt1 a) (gt1 r)
	(full-adder carry-in '(1) a r)))

    ; carry-in + (2*ar + ab) + (2*br + bb) 
    ; = (carry-in + ab + bb) (mod 2)
    ; + 2*(ar + br + (carry-in + ab + bb)/2)
    ; The cases of ar= 0 or br = 0 have already been handled.
    ; So, now we require ar >0 and br>0. That implies that rr>0.
    (relation (carry-in ab ar bb br rb rr)
      (to-show carry-in `(,ab . ,ar) `(,bb . ,br) `(,rb . ,rr))
      (all
	(pos ar) (pos br) (pos rr)
	(exists (carry-out)
	  (all
	    (half-adder carry-in ab bb rb carry-out)
	    (full-adder carry-out ar br rr))))
    )))

; After we have checked that  both summands have some bits, and so we
; can decompose them the least-significant bit and the other ones, it appears
; we only need to consider the general case, the last relation in
; the code above.
; But that is not sufficient. Let's consider
;	(full-adder 0 (1 . ()) (1 0 . ()) (0 1 . ()))
; It would then hold. But it shouldn't, because (1 0 . ()) is a bad
; number (with the most-significant bit 0). One can say why we should
; care about user supplying bad numbers. But we do: we don't know which
; arguments of full-adder are definite numbers and which are
; uninstantiated variables. We don't know which are the input and which
; are the output. So, if we keep only the last relation for the
; case of positive summands, and try to
;	(exists (x) (full-adder 0 (1 . ()) x (0 1 . ())))
; we will see x bound to (1 0) -- an invalid number. So, our adder, when
; asked to subtract numbers, gave a bad number. And it would give us
; a bad number in all the cases when we use it to subtract numbers and
; the result has fewer bits than the number to subtract from. 
;
; To guard against such a behavior (i.e., to transparently normalize
; the numbers when the full-adder is used in the ``subtraction'' mode)
; we have to specifically distinguish cases of 
; "bit0 + 2*bit_others" where bit_others>0, and the
; terminal case "1" (that is, the most significant bit 1 and no other
; bits).
; The various (pos ...) conditions in the code are to guarantee that all
; cases are disjoin. At any time, only one case can match. Incidentally,
; the lack of overlap guarantees the optimality of the code.


; The full-adder above is not recursively enumerating however.
; Indeed, (solve 10 (x y z) (full-adder '0 x y z))
; gives solutions with x = 1.
; We now convert the adder into a recursively enumerable form.
; We lose some performance however (but see below!)
;
; The general principles are:
; Convert the relation into a disjunctive normal form, that is
;  (any (all a b c) (all c d e) ...)
; and then replace the single, top-level any with any-interleave.
; The conversion may be too invasive. We, therefore, use an effective
; conversion: if we have a relation
; (all (any a b) (any c d))
; then rather than re-writing it into
; (any (all a c) (all a d) (all b c) (all b d))
; to push disjunctions out and conjunctions in, we do
; (all gen (all (any a b) (any c d)))
; where gen is a relation whose answer set is precisely such
; that each answer in gen makes (all (any a b) (any c d))
; semi-deterministic. That is, with the generator gen, we
; make all the further choices determined.
;
; In the code below we use a different kind of generator, whose full
; justification (with proofs) appears in the Prolog version of the code.
; Please see the predicate `enum' in that Prolog code.
;
; The price to pay is slow-down.
; Note, if we had all-interleave, then we would generally have
; breadth-first search and so the changes to the recursively enumerable
; version would be minimal and without loss of speed.

; The following full-adder* is almost the same as full-adder above.
(define full-adder*
  (extend-relation (carry-in a b r)
;     (fact (a) 0 a '() a) 		; 0 + a + 0 = a
;     (relation (b)			; 0 + 0 + b = b
;       (to-show 0 '() b b)
;       (pos b))
;     (relation (head-let '1 a '() r)	; 1 + a + 0 = 0 + a + 1
;       (full-adder 0 a '(1) r))
;     (relation (head-let '1 '() b r)	; 1 + 0 + b = 0 + 1 + b
;       (all (pos b)
; 	(full-adder 0 '(1) b r)))

    ; The following three relations are needed
    ; to make all numbers well-formed by construction,
    ; that is, to make sure the higher-order bit is one.
    (relation (head-let carry-in '(1) '(1) r)	; c + 1 + 1 >= 2
      (exists (r1 r2)
	(all (== r `(,r1 ,r2))
	     (half-adder carry-in 1 1 r1 r2))))

    ; cin + 1 + (2*br + bb) = (2*rr + rb) where br > 0 and so is rr > 0
    (relation (carry-in bb br rb rr)
      (to-show carry-in '(1) `(,bb . ,br) `(,rb . ,rr))
      (all
	(pos br) (pos rr)
	(exists (carry-out)
	  (all
	    (half-adder carry-in 1 bb rb carry-out)
	    (full-adder carry-out '() br rr)))))

    ; symmetric case for the above
    (relation (head-let carry-in a '(1) r)
      (all
	(gt1 a) (gt1 r)
	(full-adder* carry-in '(1) a r)))

    ; carry-in + (2*ar + ab) + (2*br + bb) 
    ; = (carry-in + ab + bb) (mod 2)
    ; + 2*(ar + br + (carry-in + ab + bb)/2)
    ; The cases of ar= 0 or br = 0 have already been handled.
    ; So, now we require ar >0 and br>0. That implies that rr>0.
    (relation (carry-in ab ar bb br rb rr)
      (to-show carry-in `(,ab . ,ar) `(,bb . ,br) `(,rb . ,rr))
      (all
	(pos ar) (pos br) (pos rr)
	(exists (carry-out)
	  (all
	    (half-adder carry-in ab bb rb carry-out)
	    (full-adder* carry-out ar br rr))))
    )))

; This driver handles the trivial cases and then invokes full-adder*
; coupled with the recursively enumerating generator.

(define full-adder
  (extend-relation (carry-in a b r)
    (fact (a) 0 a '() a) 		; 0 + a + 0 = a
    (relation (b)			; 0 + 0 + b = b
      (to-show 0 '() b b)
      (pos b))
    (relation (head-let '1 a '() r)	; 1 + a + 0 = 0 + a + 1
      (full-adder 0 a '(1) r))
    (relation (head-let '1 '() b r)	; 1 + 0 + b = 0 + 1 + b
      (all (pos b)
	(full-adder 0 '(1) b r)))
    (relation (head-let carry-in a b r)
      (any-interleave
	; Note that we take advantage of the fact that if
	; a + b = r and length(b) <= length(a) then length(a) <= length(r)
	(all (<ol a `(,_ . ,r))		; or, length(a) < length(2*r)
	  (any (<ol b a) (=ol b a))
	  (full-adder* carry-in a b r))
	; commutative case, length(a) < length(b)
	(all (<ol b `(,_ . ,r))
	  (<ol a b)
	  (full-adder* carry-in a b r))
	))))

; There is the third way of doing the addition, using
; all-interleave and any-interleave.
; Note that the code below is almost identical to the very first,
; non-recursively enumerating full-adder, only
; extend-relation is replaced with extend-relation-interleave
; and all is replaced with all-interleave in two places.
; The results are amazing, as the tests below show.
; For example, the test "print a few numbers that are greater than 4"
; shows that the numbers are generated _in sequence_, despite
; our addition being binary (and so one would expect the numbers
; being generated in Gray code or so).
; Also, tests multiplication-all-3 and multiplication-all-4
; show that (**o (build 3) y z) and (**o y (build 3) z)
; generates the _same_ answerlist, and in that answerlist, 'y' appears
; in sequence: 0,1,2....


(define-rel-lifted-comb extend-relation-interleave any-interleave)

(define full-adder
  (extend-relation-interleave (carry-in a b r)
    (fact (a) 0 a '() a) 		; 0 + a + 0 = a
    (relation (b)			; 0 + 0 + b = b
      (to-show 0 '() b b)
      (pos b))
    (relation (head-let '1 a '() r)	; 1 + a + 0 = 0 + a + 1
      (full-adder 0 a '(1) r))
    (relation (head-let '1 '() b r)	; 1 + 0 + b = 0 + 1 + b
      (all (pos b)
	(full-adder 0 '(1) b r)))

    ; The following three relations are needed
    ; to make all numbers well-formed by construction,
    ; that is, to make sure the higher-order bit is one.
    (relation (head-let carry-in '(1) '(1) r)	; c + 1 + 1 >= 2
      (exists (r1 r2)
	(all (== r `(,r1 ,r2))
	     (half-adder carry-in 1 1 r1 r2))))

    ; cin + 1 + (2*br + bb) = (2*rr + rb) where br > 0 and so is rr > 0
    (relation (carry-in bb br rb rr)
      (to-show carry-in '(1) `(,bb . ,br) `(,rb . ,rr))
      (all
	(pos br) (pos rr)
	(exists (carry-out)
	  (all-interleave
	    (half-adder carry-in 1 bb rb carry-out)
	    (full-adder carry-out '() br rr)))))

    ; symmetric case for the above
    (relation (head-let carry-in a '(1) r)
      (all
	(gt1 a) (gt1 r)
	(full-adder carry-in '(1) a r)))

    ; carry-in + (2*ar + ab) + (2*br + bb) 
    ; = (carry-in + ab + bb) (mod 2)
    ; + 2*(ar + br + (carry-in + ab + bb)/2)
    ; The cases of ar= 0 or br = 0 have already been handled.
    ; So, now we require ar >0 and br>0. That implies that rr>0.
    (relation (carry-in ab ar bb br rb rr)
      (to-show carry-in `(,ab . ,ar) `(,bb . ,br) `(,rb . ,rr))
      (all
	(pos ar) (pos br) (pos rr)
	(exists (carry-out)
	  (all-interleave
	    (half-adder carry-in ab bb rb carry-out)
	    (full-adder carry-out ar br rr))))
    )))

; a + b = c
(define ++o
  (relation (head-let a b c)
    (full-adder 0 a b c)))

; a - b = c
(define --o
  (lambda (x y out)
    (++o y out x)))

 
(define <o  ; n < m iff exists x >0 such that n + x = m
  (relation (head-let n m)
    (exists (x) (all (pos x) (++o n x m)))))

; The following is an optimization: it is easier to test for the
; length of two numbers. If one number has fewer bits than the other number,
; the former is clearly shorter (provided that the numbers are well-formed,
; that is, the higher-order bit is one). So we don't need to go through
; the trouble of subtracting them.
(define <o  ; n < m iff exists x >0 such that n + x = m
  (relation (head-let n m)
    (any-interleave
      (<ol n m)
      (all (=ol n m)
	(exists (x) (all (pos x) (++o n x m)))))))


; n * m = p
(define **o
  (relation (head-let n m p)
    (any-interleave
      (all (zeroo n) (== p '()))		; 0 * m = 0
      (all (zeroo m) (pos n) (== p '()))	; n * 0 = 0
      (all (== n '(1)) (pos m) (== m p))        ; 1 * m = m
      (all (== m '(1)) (gt1 n) (== n p))        ; n * 1 = n, n>1

      ; (2*nr) * m = 2*(nr*m), m>0 (the case of m=0 is taken care of already)
      ; nr > 0, otherwise the number is ill-formed
      (exists (nr pr)
	(all
	  (gt1 m)
	  (== n `(0 . ,nr))
	  (== p `(0 . ,pr))
	  (pos nr) (pos pr)
	  (**o nr m pr)))

      ; The symmetric case to the above: m is even, n is odd
      (exists (mr pr)
	(all
	  (== n `(1 ,_ . ,_))		; n is odd and n > 1
	  (== m `(0 . ,mr))
	  (== p `(0 . ,pr))
	  (pos mr) (pos pr)
	  (**o n mr pr)))

      ; (2*nr+1) * m = 2*(n*m) + m
      ; m > 0; also nr>0 for well-formedness
      ; the result is certainly greater than 1.
      ; we note that m > 0 and so 2*(nr*m) < 2*(nr*m) + m
      ; and (floor (log2 (nr*m))) < (floor (log2 (2*(nr*m) + m)))
      (exists (nr p1)
	(all
	  (== m `(1 ,_ . ,_))		; m is odd and n > 1
	  (== n `(1 . ,nr))
	  (pos nr) (gt1 p)
	  (<ol3 p1 p n m)
	  (**o nr m p1)
	  (++o `(0 . ,p1) m p)))
)))

; n = q*m + r
; where 0<=r<m

; This is divo from pure-arithm.scm
; it still works -- but very slow for some operations
; because <o takes linear time...
(define divo
  (relation (head-let n m q r)
    (any-interleave
      (all (== q '()) (== r n) (<o n m))      ; if n < m, then q=0, n=r
      (all (== n m) (== q '(1)) (== r '()))  ; n = 1*n + 0
      (exists (p)
	(all (<o m n) (<o r m)  (++o p r n) ;(trace-vars 1 (p r n))
	  (**o q m p))))))

; A faster divo algorithm
(define divo
  (relation (head-let n m q r)
    (any-interleave
      (all (== r n) (== q '()) (<ol n m) (<o n m)) ; m has more digits than n: q=0,n=r
      (all
	(<ol m n)			; n has mode digits than m
					; q is not zero, n>0, so q*m <= n,
	(exists (p)			; definitely q*m < 2*n
	  (all (<o r m) (<ol p `(0 . ,n))
	    (++o p r n) ;(trace-vars 1 (p r n))
	    (**o q m p)))
	)
      ; n has the same number of digits than m
      (all (== q '(1)) (=ol n m) (++o r m n) (<o r m))
      (all (== q '()) (== r n) (=ol n m) (<o n m))  ; if n < m, then q=0, n=r
      )))
; 	(any-interleave
; 	  (all (== m '(1)) (== r '()) (== n q)) ; n = n*1 + 0
; 	  ; For even divisors:
; 	  ; n = (2*m)*q + r => (n - r) is even and (n-r)/2 = m*q
; 	  (exists (p m1)
; 	    (all (== m `(0 . ,m1))
; 	         (== m1 `(_, . ,_))
; 	         (**o m1 q p)
; 	         (--o n r `(0 . ,p))))


; A faster and a more refutationally complete divo algorithm
; Again, divo n m q r 
; holds iff n = m*q + r
; Let l be the bit-length of r (if r=0, l=0).
; Let n = 2^(l+1) * n1 + n2
;     q = 2^(l+1) * q1 + q2
; Note that n1 or q1 may be zero.
; We obtain that
;    n = m*q + r
; is equivalent to the conjunction of the following two relations
;    q2*m + r - n2 is divisible by 2^(l+1)
;    n1 = q1*m + (q2*m + r - n2)/2^(l+1)
; We note that by construction (see the mentioning of (<ol m n) below)
; all numbers in 'q2*m + r - n2' are length-limited. Therefore, we can
; obtain the success or failure of 'q2*m + r - n2' in finite time.
; This fact let us fail the 'divo' relation, in finite time,
; when it obviously does not hold, as in
;	(divo `(0 . ,x) (build 2) q '(1))
; (because no even number can give the remainder 1 upon the 
; division by two).
;
; We should note that if n1=0, we obtain that q1 must be zero
; (because m>0) and q2*m + r = n2. The latter can be solved in finite
; time.
; We also note that (q2*m + r - n2)/2^(l+1) < m
; because r - n2 < (2^(l+1) - q2)* m
; because 2^(l+1) - q2 >=1 and m > r by construction. Therefore, to
; solve the relation n1 = q1*m + (q2*m + r - n2)/2^(l+1) we use
; divo itself: (divo n1 m q1 (q2*m + r - n2)/2^(l+1))
; Thus our division algorithm is recursive. On each stage we determine at
; least one bit of the quotient (if r=0, l=0 and q2 is either 0 or 1),
; in finite time.
; Chung-chieh Shan has pointed out that the present algorithm is
; akin to the elementary school long-division algorithm, only
; done right-to-left.

(define divo
  (relation (head-let n m q r)
    (any-interleave
      (all (== r n) (== q '()) (<o n m)) ; if n < m, then q=0, n=r
      ; n is at least m and has the same number of digits as m
      (all (== q '(1)) (=ol n m) (++o r m n) (<o r m))
      (all-interleave
	(<ol m n)			; n has more digits than m
					; Note that m is L-instantiated here
	(<o r m)			; r is L-instantiated
	(pos q)				; q must be positive then
	(exists (n1 n2 q1 q2 q2m q2mr rr r1)
	  (all-interleave
	    (split n r n1 n2)
	    (split q r q1 q2)
	    (any
	      (all
		(== n1 '())
		(== q1 '())
		(--o n2 r q2m)
		(**o q2 m q2m))			; provably terminates
	      (all-interleave (pos n1) 
		(**o q2 m q2m)
		(++o q2m r q2mr)
		(--o q2mr n2 rr)		; rr = q2*m + r - n2
		(split rr r r1 '())		; r1 = rr/2^(l+1), evenly
		(divo n1 m q1 r1)))))
	)
      )))

; split n r n1 n2
; holds if n = 2^(l+1)*n1 + n2 where l = bitlength(r)
; This relation makes sense to use only when 'r' is L-instantiated
; (see the Prolog code file for the definition of L-instantiated).
; In that case, the relation has only the finite number of answers, in
; all of which n2 is L-instatantiated.
; We take trouble to assure that we produce only well-formed numbers:
; the major bit must be one.

(define split
  (extend-relation-interleave (n r n1 n2)
    (fact () '() _ '() '())
    (fact (b n) `(0 ,b . ,n) '() `(,b . ,n) '())
    (fact (n) `(1 . ,n) '() n '(1))
    (relation (b n r n1)
      (to-show `(0 ,b . ,n) `(,_ . ,r) n1 '())
      (split `(,b . ,n) r n1 '()))
    (relation (n r n1)
      (to-show `(1 . ,n) `(,_ . ,r) n1 '(1))
      (split n r n1 '()))
    (relation (b n r n1 n2)
      (to-show `(,b . ,n) `(,_ . ,r) n1 `(,b . ,n2))
      (all (pos n2)
	(split n r n1 n2)))
))


; Exponentiation and discrete logarithm
; n = b^q + r, where 0 <= r and q is the largest such integer
;
; From the above condition we obtain the upper bound on r:
; n >= b^q, n < b^(q+1) = b^q * b = (n-r)* b 
; r*b < n*(b-1)
;
; We can also obtain the bounds on q:
; if |b| is the bitwidth of b and |n| is the bitwidth of n,
; we have, by the definition of the bitwidth:
;  (1) 2^(|b|-1) <= b < 2^|b|
;  (2) 2^(|n|-1) <= n < 2^|n|
; Raising (1) to the power of q:
;      2^((|b|-1)*q) <= b^q
; OTH, b^q <= n, and n < 2^|n|. So we obtain
;  (3)   (|b|-1)*q < |n|
; which defines the upper bound on |q|.
; OTH, raising (1) to the power of (q+1):
;    b^(q+1) < 2^(|b|*(q+1))
; But n < b^(q+1) by definition of exponentiation, and keeping in mind (1)
; (4) |n|-1 < |b|*(q+1)
; which is the lower bound on q.

; When b = 2, exponentiation and discrete logarithm are easier to obtain
; n = 2^q + r, 0<= 2*r < n
; Here, we just relate n and q.
;    exp2 n b q
; holds if: n = (|b|+1)^q + r, q is the largest such number, and
; (|b|+1) is a power of two.
; Side condition: (|b|+1) is a power of two and b is L-instantiated.
; To obtain the binary exp/log relation, invoke the relation as
;  (exp2 n '() q)
; Properties: if n is L-instantiated, one answer, q is fully instantiated.
; If q is fully instantiated: one answer, n is L-instantiated.
; In any event, q is always fully instantiated in any answer
; and n is L-instantiated.
; We depend on the properties of split.

(define exp2
  (letrec
    ((r-append				; relational append
       (extend-relation (a b c)
	 (fact (b) '() b b)
	 (relation (ah ar b cr) (to-show `(,ah . ,ar) b `(,ah . ,cr))
	   (r-append ar b cr)))))
  (relation (head-let n b q)
    (any-interleave
      (all (== n '(1)) (== q '()))  ; 1 = b^0
      (all (gt1 n) (== q '(1)) (split n b '(1) _))
      (exists (q1 b2)			; n = (2^k)^(2*q) + r
	(all-interleave                 ;   = (2^(2*k))^q + r
	     (== q `(0 . ,q1))
	     (pos q1)
	     (<ol b n)
	     (r-append b `(1 . ,b) b2)
	     (exp2 n b2 q1)))
      (exists (q1 n1 b2)		; n = (2^k)^(2*q+1) + r
	(all-interleave 		; n/(2^k) = (2^(2*k))^q + r'
	     (== q `(1 . ,q1))
	     (pos q1)
	     (pos n1)
	     (split n b n1 _)
	     (r-append b `(1 . ,b) b2)
	     (exp2 n1 b2 q1)))
      )))
)


; nq = n^q where n is L-instantiated and q is fully instantiated
(define repeated-mul
  (extend-relation (n q nq)
    (fact () `(,_ . ,_) '() '(1))
    (fact (n) n '(1) n)
    (relation (head-let n q nq)
      (all
	(gt1 q)
	(exists (q1 nq1)
	  (all
	    (++o q1 '(1) q)
	    (repeated-mul n q1 nq1)
	    (**o nq1 n nq)))))))

; We call this predicate logo rather than expo due to its close similarity
; to divo. As the tests at the end show, logo can be used for determining
; the exact discrete logarithm, logarithm with a residual, exponentiation,
; and even operations (such as determining the base) that are not
; commonly discussed in high school.
(define logo
  (relation (head-let n b q r)
    (any-interleave
      (all (== n '(1)) (pos b) (== q '()) (== r '())) ; 1 = b^0 + 0, b >0
      (all (== q '())  (<o n b) (pos r) (++o r '(1) n)) ; n = b^0 + (n-1)
	; n = b + r, n and b the same sz
      (all (== q '(1)) (gt1 b) (=ol n b) (++o r b n))
      (all (== b '(1)) (pos q) (++o r '(1) n))  ; n = 1^q + (n-1), q>0
      (all (== b '()) (pos q) (== r n))         ; n = 0^q + n, q>0
      ; in the rest, n is longer than b
      (all (== b '(0 1))		; b = 2
	   (exists (n1)
	     (all
	       (pos n1)
	       (== n `(,_ ,_ . ,n1))    ; n is at least 4
	       (exp2 n '() q)		; that will L-instantiate n and n1
	       (split n n1 _ r))))
      ; the general case
      (all
	(any (== b '(1 1)) (== b `(,_ ,_ ,_ . ,_))) ; b >= 3
	(<ol b n)			; b becomes L-instantiated
	                                ; If b was L-instantiated, the previous
					; goal had only *one* answer
	(exists (bw nw nw1 bw1 ql1 ql qh qdh qd bql bqd bq bq1)
	 (all
	  (exp2 b '() bw1)
	  (++o bw1 '(1) bw)
	  (<ol q n)			; A _very_ lose bound, but makes
					; sure q will be L-instatiated
					; Now, we can use b and q to bound n
					; |n|-1 < |b|*(q+1)
	  (exists (q1 bwq1)
	    (all
	      (++o q '(1) q1)
	      (**o bw q1 bwq1)		; |b|*(q+1)
	      (<o nw1 bwq1)))
	  (exp2 n '() nw1)		; n becomes L-instantiated
					; Now we have only finite number of ans
	  (++o nw1 '(1) nw)
	  (divo nw bw ql1 _)		; low boundary on q:
	  (++o ql '(1) ql1)		; |n| = |b|(ql+1) + c
	  (any (=ol q ql) (<ol ql q))	; Tighten the estimate for q
	  (repeated-mul b ql bql)	; bql = b^ql
	  (divo nw bw1 qh _)		; upper boundary on q-1
	  (++o ql qdh qh)
	  (++o ql qd q)
	  (any (== qd qdh) (<o qd qdh)) ; qd is bounded
	  (repeated-mul b qd bqd)	; b^qd
	  (**o bql bqd bq)		; b^q
	  (**o b   bq  bq1)		; b^(q+1)
	  (++o bq r n)
	  (<o n bq1)			; check the r condition
	  ))))))

	


;------------------------------------------------------------------------
;				Tests


(define-syntax test
  (syntax-rules ()
    ((_ (x) gl)
      (query (redok subst x) gl
	(display (trans (subst-in x subst)))
	(newline)))))

(define (subset? l1 l2)
  (or (null? l1)
    (and (member (car l1) l2) (subset? (cdr l1) l2))))
(define (set-equal? l1 l2) (or (subset? l1 l2) (subset? l2 l1)))
  
(cout nl "addition" nl)
(test (x) (++o (build 29) (build 3) x))
(test (x) (++o (build 3) x (build 29)))
(test (x) (++o x (build 3) (build 29)))
(test-check "all numbers that sum to 4"
  (solve 10 (w)
    (exists (y z)
      (all (++o y z (build 4))
	(project (y z) (== `(,(trans y) ,(trans z)) w)))))
   '(((w.0 (4 0)))
     ((w.0 (0 4)))
     ((w.0 (1 3)))
     ((w.0 (3 1)))
     ((w.0 (2 2)))
     )
  )
(test-check "print a few numbers such as X + 1 = Y"
  (solve 5 (x y) (++o x (build 1) y))
   '(((x.0 ()) (y.0 (1))) ; 0 + 1 = 1
     ((x.0 (1)) (y.0 (0 1))) ; 1 + 1 = 2
       ; 2*x and 2*x+1 are successors, for all x>0!
      ((x.0 (0 _.0 . _.1)) (y.0 (1 _.0 . _.1)))
      ((x.0 (1 1)) (y.0 (0 0 1)))
      ((x.0 (1 0 _.0 . _.1)) (y.0 (0 1 _.0 . _.1))))
)

; check that add(X,Y,Z) recursively enumerates all
; numbers such as X+Y=Z
;
(cout "Test recursive enumerability of addition" nl)
(let ((n 7))
  (do ((i 0 (+ 1 i))) ((> i n))
    (do ((j 0 (+ 1 j))) ((> j n))
      (let ((p (+ i j)))
	(test-check
	  (string-append "enumerability: " (number->string i)
	    "+" (number->string j) "=" (number->string p))
	  (solve 1 (x y z) 
	    (all (++o x y z)
	      (== x (build i)) (== y (build j)) (== z (build p))))
	  `(((x.0 ,(build i)) (y.0 ,(build j))
	      (z.0 ,(build p)))))))))

(test-check "strong commutativity"
  (solve 5 (a b c)
    (all (++o a b c)
    (exists (x y z)
      (all!
	(++o x y z)
	(== x b)
	(== y a)
	(== z c)
	))))
  '(((a.0 ()) (b.0 ()) (c.0 ()))
    ((a.0 ()) (b.0 (_.0 . _.1)) (c.0 (_.0 . _.1)))
    ((a.0 (1)) (b.0 (1)) (c.0 (0 1)))
    ((a.0 (1)) (b.0 (0 _.0 . _.1)) (c.0 (1 _.0 . _.1)))
    ((a.0 (0 _.0 . _.1)) (b.0 (1)) (c.0 (1 _.0 . _.1))))
)


(cout nl "subtraction" nl)
(test (x) (--o (build 29) (build 3) x))
(test (x) (--o (build 29) x (build 3)))
(test (x) (--o x (build 3) (build 26)))
(test (x) (--o (build 29) (build 29) x))
(test (x) (--o (build 29) (build 30) x))
(test-check "print a few numbers such as Y - Z = 4"
  (solve 11 (y z) (--o y z (build 4)))
  '(((y.0 (0 0 1)) (z.0 ()))    ; 4 - 0 = 4
    ((y.0 (1 0 1)) (z.0 (1)))   ; 5 - 1 = 4
    ((y.0 (0 1 1)) (z.0 (0 1))) ; 6 - 2 = 4
    ((y.0 (1 1 1)) (z.0 (1 1))) ; 7 - 3 = 4
    ((y.0 (0 0 0 1)) (z.0 (0 0 1))) ; 8 - 4 = 4
    ((y.0 (1 0 0 1)) (z.0 (1 0 1)))  ; 9 - 5 = 4
    ((y.0 (0 1 0 1)) (z.0 (0 1 1)))  ; 10 - 6 = 4
    ((y.0 (1 1 0 1)) (z.0 (1 1 1)))  ; 11 - 7 = 4
     ; 8*k + 4 - 8*k = 4 forall k> 0!!
    ((y.0 (0 0 1 _.0 . _.1)) (z.0 (0 0 0 _.0 . _.1)))
    ((y.0 (1 0 1 _.0 . _.1)) (z.0 (1 0 0 _.0 . _.1)))
    ((y.0 (0 1 1 _.0 . _.1)) (z.0 (0 1 0 _.0 . _.1))))
)

(test-check "print a few numbers such as X - Y = Z"
  (solve 5 (x y z) (--o x y z))
  '(((x.0 _.0) (y.0 _.0) (z.0 ())) ; 0 - 0 = 0
    ((x.0 (_.0 . _.1)) (y.0 ()) (z.0 (_.0 . _.1))) ; a - 0 = a
    ((x.0 (0 1)) (y.0 (1)) (z.0 (1)))
    ((x.0 (1 _.0 . _.1)) (y.0 (1)) (z.0 (0 _.0 . _.1)))
    ((x.0 (1 _.0 . _.1)) (y.0 (0 _.0 . _.1)) (z.0 (1))))
)


(cout nl "comparisons" nl)
(test (x) (<o x (build 4)))
(test (x) (all (== x (build 3)) (<o x (build 4))))
(test (x) (all (== x (build 4)) (<o x (build 3))))
(test-check "print all numbers that are less than 6"
  (solve 10 (x) (<o x (build 6)))
  '(((x.0 ())) ((x.0 (1 0 1))) ((x.0 (1))) 
     ((x.0 (0 0 1))) ((x.0 (_.0 1))))
  )

(test-check "print *all* numbers that are greater than 4"
  (solve 10 (x) (<o (build 4) x))
  '(((x.0 (_.0 _.1 _.2 _.3 . _.4)))
    ((x.0 (1 0 1))) ((x.0 (0 1 1))) ((x.0 (1 1 1))))
)



(cout nl "multiplication" nl)
(test (x) (**o (build 2) (build 3) x))
(test (x) (**o (build 3) x (build 12)))
(test (x) (**o x (build 3) (build 12)))
(test (x) (**o x (build 5) (build 12)))
(test (x) (all (== x (build 2)) (**o x (build 2) (build 4))))
(test-check 'multiplication-fail-1
  (test (x) (all (== x (build 3)) (**o x (build 2) (build 4))))
  '())
(test-check 'multiplication-all-1
  (solve 7 (w) 
    (exists (y z) (all (**o y z (build 6))
		    (project (y z) (== `(,(trans y) ,(trans z)) w)))))
  '(((w.0 (1 6))) ((w.0 (6 1))) ((w.0 (2 3)))  ((w.0 (3 2)))))

; Only one answer
(test-check 'multiplication-all-2
  (solve 7 (w) 
    (exists (x)
      (all (**o (build 3) (build 2) x)
	(project (x) (== (trans x) w)))))
  '(((w.0 6))))

(test-check 'multiplication-all-3
  (solve 7 (y z) (**o (build 3) y z))
  '(((y.0 ()) (z.0 ()))  ; 0 * 3 = 0
    ((y.0 (1)) (z.0 (1 1))) ; 1 * 3 = 3
    ((y.0 (0 1)) (z.0 (0 1 1))) ; 2 * 3 = 6
    ((y.0 (1 1)) (z.0 (1 0 0 1))) ; 3 * 3 = 9
    ((y.0 (0 0 1)) (z.0 (0 0 1 1))) ; 4 * 3 = 12
    ((y.0 (1 0 1)) (z.0 (1 1 1 1))) ; 5 * 3 = 15
    ((y.0 (0 1 1)) (z.0 (0 1 0 0 1)))) ; 6 * 3 = 18
)

; Just as above
(test-check 'multiplication-all-4
  (solve 7 (y z) (**o y (build 3) z))
  '(((y.0 ()) (z.0 ()))
    ((y.0 (1)) (z.0 (1 1)))
    ((y.0 (0 1)) (z.0 (0 1 1)))
    ((y.0 (1 1)) (z.0 (1 0 0 1)))
    ((y.0 (0 0 1)) (z.0 (0 0 1 1)))
    ((y.0 (1 0 1)) (z.0 (1 1 1 1)))
    ((y.0 (0 1 1)) (z.0 (0 1 0 0 1))))
)

(test-check 'multiplication-all-5
  (solve 7 (x y z) (**o x y z))
  '(((x.0 ()) (y.0 _.0) (z.0 ())) ; 0 * y = 0 for any y whatsoever
    ((x.0 (_.0 . _.1)) (y.0 ()) (z.0 ())) ; x * 0 = 0 for x > 0
     ; 1 * y = y for y > 0
    ((x.0 (1)) (y.0 (_.0 . _.1)) (z.0 (_.0 . _.1)))
    ((x.0 (_.0 _.1 . _.2)) (y.0 (1)) 
      (z.0 (_.0 _.1 . _.2))) ;  x * 1 = x, x > 1
     ; 2 * y = even positive number, for y > 1
    ((x.0 (0 1)) (y.0 (_.0 _.1 . _.2)) 
      (z.0 (0 _.0 _.1 . _.2)))
     ; x * 2 = shifted-left x, for even x>1
    ((x.0 (1 _.0 . _.1)) (y.0 (0 1)) (z.0 (0 1 _.0 . _.1)))
     ; 3 * 3 = 9
    ((x.0 (1 1)) (y.0 (1 1)) (z.0 (1 0 0 1)))
    )
)

(test-check 'multiplication-even-1
  (solve 10 (y z) (**o (build 2) y z))
  '(((y.0 ()) (z.0 ()))
    ((y.0 (1)) (z.0 (0 1))) ; 2 * 1 = 2
     ; 2*y is an even number, for any y > 1!
    ((y.0 (_.0 _.1 . _.2)) (z.0 (0 _.0 _.1 . _.2)))
     )
)

(test-check 'multiplication-even-2
  ; multiplication by an even number cannot yield an odd number
  (solution (q x y u v) (**o '(1 1) `(0 0 1 ,x . ,y) `(1 0 0 ,u . ,v)))
  #f
)

(test-check 'multiplication-even-3
  ; multiplication by an even number cannot yield an odd number
  (solution (q x y z) (**o `(0 0 1 . ,y) `(1 . ,x) `(1 0 . ,z)))
  #f
)

; check that mul(X,Y,Z) recursively enumerates all
; numbers such as X*Y=Z
;
(cout "Test recursive enumerability of multiplication" nl)
(let ((n 7))
  (do ((i 0 (+ 1 i))) ((> i n))
    (do ((j 0 (+ 1 j))) ((> j n))
      (let ((p (* i j)))
	(test-check
	  (string-append "enumerability: " (number->string i)
	    "*" (number->string j) "=" (number->string p))
	  (solve 1 (x y z) 
	    (all (**o x y z)
	      (== x (build i)) (== y (build j)) (== z (build p))))
	  `(((x.0 ,(build i)) (y.0 ,(build j))
	      (z.0 ,(build p)))))))))

(cout nl "split" nl)

(test-check 'split-1
  (solve 5 (x y) (split (build 4) '() x y))
  '(((x.0 (0 1)) (y.0 ()))))
(test-check 'split-2
  (solve 5 (x y) (split (build 4) '(1) x y))
  '(((x.0 (1)) (y.0 ()))))
(test-check 'split-3
  (solve 5 (x y) (split (build 4) '(1 1) x y))
  '(((x.0 ()) (y.0 (0 0 1)))))
(test-check 'split-4
  (solve 5 (x y) (split (build 4) '(1 1 1) x y))
  '(((x.0 ()) (y.0 (0 0 1)))))
(test-check 'split-5
  (solve 5 (x y) (split (build 5) '(1) x y))
  '(((x.0 (1)) (y.0 (1)))))
(test-check 'split-6
  (solve 5 (n) (split n (build 5) '() '(1)))
  '(((n.0 (1)))))

(cout nl "division, general" nl)


(test-check 'divo-1
  (solution (x) (divo (build 4) (build 2) x _))
  '((x.0 (0 1))))
(test-check 'div-fail-1 (test (x) (divo (build 4) (build 0) x _)) '())
(test-check 'divo-2
  (solution (x) (divo (build 4) (build 3) x _))
  '((x.0 (1))))
(test-check 'divo-3
  (solution (x) (divo (build 4) (build 4) x _))
  '((x.0 (1))))
(test-check 'divo-4
  (solution (x y) (divo (build 4) (build 5) x y))
  '((x.0 ()) (y.0 (0 0 1))))


(test-check 'divo-33-1
  (solution (x) (divo (build 33) (build 3) x _))
  `((x.0 ,(build 11))))
(test-check 'divo-33-2
  (solution (x) (divo (build 33) x (build 11) _))
  `((x.0 ,(build 3))))
(test-check 'divo-33-3
  (solution (x) (divo x (build 3) (build 11) _))
  `((x.0 ,(build 33))))
(test-check 'divo-33-5
  (solution (x y) (divo (build 33) (build 5) x y))
  `((x.0 ,(build 6)) (y.0 ,(build 3))))


(test-check 'divo-5-4
  (solve 3 (x y) (divo x (build 5) y (build 4)))
  '(((x.0 (0 0 1)) (y.0 ()))
    ((x.0 (0 0 0 0 0 0 1)) (y.0 (0 0 1 1)))
    ((x.0 (1 0 0 0 1 1)) (y.0 (1 0 0 1))))
)
(test-check 'divo-5-5
  (solve 3 (x y) (divo x (build 5) y (build 5)))
  '())


(test (x) (divo x (build 5) _ (build 4)))
(test (x) (divo x (build 5) (build 3) (build 4)))
(test (x) (divo x _ (build 3) (build 4)))
(test-check 'div-fail-2 (test (x) (divo (build 5) x (build 7) _)) '())

(test-check "all numbers such as 5/Z = 1"
  (solve 7 (w) 
    (exists (z) (all (divo (build 5) z (build 1) _)
		    (project (z) (== `(,(trans z)) w)))))
  '(((w.0 (5))) ((w.0 (3))) ((w.0 (4)))))

(test-check "all inexact factorizations of 12"
  (set-equal?
   (solve 100 (w) 
    (exists (m q r n)
      (all 
	(== n (build 12))
	(<o m n)
	(divo n m q r)
	(project (m q r) (== `(,(trans m) ,(trans q) ,(trans r)) w)))))
  '(((w.0 (1 12 0))) ((w.0 (11 1 1)))
    ((w.0 (2 6 0)))  ((w.0 (10 1 2)))
    ((w.0 (4 3 0)))  ((w.0 (8 1 4)))
    ((w.0 (6 2 0)))  ((w.0 (3 4 0)))
    ((w.0 (9 1 3)))  ((w.0 (7 1 5)))
    ((w.0 (5 2 2)))))
  #t)


(test-check 'div-all-3
  (solve 4 (x y z r) (divo x y z r))
'(((x.0 ()) (y.0 (_.0 . _.1)) (z.0 ()) (r.0 ())) ; 0 = a*0 + 0, a>0
  ((x.0 (1)) (y.0 (1)) (z.0 (1)) (r.0 ())) ; 1 = 1*1 + 0
  ((x.0 (0 1)) (y.0 (1)) (z.0 (0 1)) (r.0 ())) ; 2 = 1*2 + 0
  ((x.0 (0 1)) (y.0 (1 1)) (z.0 ()) (r.0 (0 1))) ; 2 = 3*0 + 2
))

(test-check 'div-even
  (solve 3  (y z r) (divo `(0 . ,y) (build 2) z r))
  '(((y.0 (1)) (z.0 (1)) (r.0 ()))
    ((y.0 (0 1)) (z.0 (0 1)) (r.0 ()))
    ((y.0 (1 1)) (z.0 (1 1)) (r.0 ())))
)

(test-check 'div-even-fail
  (solve 3  (y z r) (divo `(0 . ,y) (build 2) z '(1)))
  '()
)

(test-check 'div-odd
  (solve 3  (y z) (divo `(1 0 . ,y) (build 2) z '(1)))
  '(((y.0 (0 1)) (z.0 (0 0 1))) ; 9 = 2*4 + 1
    ((y.0 (1)) (z.0 (0 1))) ; 5 = 2*2 + 1
    ((y.0 (0 0 1)) (z.0 (0 0 0 1)))) ; 17 = 8*2 + 1
)

(test-check 'div-odd-fail
  (solve 3  (y z r) (divo `(1 0 . ,y) (build 2) z '()))
  '()
)

(test-check 'div-enum-sample
  (solve 1 (n m q r)
    (all (divo n m q r)
      (== n (build 10)) (== m (build 2)) (== q (build 5))
      (== r '())))
  '(((n.0 (0 1 0 1)) (m.0 (0 1)) (q.0 (1 0 1)) (r.0 ())))
)

; the latter takes awfully long time
'(test-check 'div-enum-sample-1
  (solve 1 (n m q r)
    (all (divo n m q r)
      (== n (build 10)) (== m (build 3)) (== q (build 3))
      (== r '(1))))
  '(((n.0 (1 1 1)) (m.0 (0 1)) (q.0 (1 1)) (r.0 (1))))
)

; check that divo(N,M,Q,R) recursively enumerates all
; numbers such as N=M*Q+R, R<M
; It is a slow test...
(cout "Test recursive enumerability of division" nl)
'(let ((n 3))
  (do ((m 1 (+ 1 m))) ((> m n))
    (do ((q 0 (+ 1 q))) ((> q n))
      (do ((r 0 (+ 1 r))) ((>= r m))
	(let ((n (+ (* m q) r)))
	 (test-check
	  (string-append "enumerability: " (number->string n)
	    "=" (number->string m) "*" (number->string q)
	    "+" (number->string r))
	  (solve 1 (n1 m1 q1 r1) 
	    (all (divo n1 m1 q1 r1)
	      (== n1 (build n)) (== m1 (build m))
	      (== q1 (build q)) (== r1 (build r))
	      ))
	  `(((n1.0 ,(build n)) (m1.0 ,(build m))
	     (q1.0 ,(build q)) (r1.0 ,(build r))))))))))


; quite dubious tests. The problem is actually in =ol and <ol
; (solve 1 (q)
;     (poso q)
;     (divo `(0 . ,q) `(1 1 . ,q) '() `(0 . ,q))))
;
; (solve 1 (q)
;     (poso q)
;     (divo `(0 . ,q) `(1 . ,q) '() `(0 . ,q))))


(test-check 'exp2-1
  (solve 10 (q) (exp2 '(1 1 1 1) '() q))
  '(((q.0 (1 1)))))

(test-check 'exp2-2
  (solve 10 (q) (exp2 '(1 0 1 1 1) '()  q))
  '(((q.0 (0 0 1)))))

; These are all answers!
(test-check 'exp2-3
  (solve 100 (n) (exp2 n '() '(1 0 1)))
  '(((n.0 (0 0 0 0 0 1)))
    ((n.0 (1 0 0 0 0 1)))
    ((n.0 (0 1 0 0 0 1)))
    ((n.0 (1 1 0 0 0 1)))
    ((n.0 (0 _.0 1 0 0 1)))
    ((n.0 (1 _.0 1 0 0 1)))
    ((n.0 (0 _.0 _.1 1 0 1)))
    ((n.0 (1 _.0 _.1 1 0 1)))
    ((n.0 (0 _.0 _.1 _.2 1 1)))
    ((n.0 (1 _.0 _.1 _.2 1 1))))
)


(test-check 'exp2-4
  (solve 5 (n q) (exp2 n '() q))
  '(((n.0 (1)) (q.0 ()))
    ((n.0 (0 1)) (q.0 (1)))
    ((n.0 (0 0 1)) (q.0 (0 1)))
    ((n.0 (0 0 0 1)) (q.0 (1 1)))
    ((n.0 (1 1)) (q.0 (1)))))


(test-check 'logo-15-1
  (solve 10 (q r) (logo (build 15) (build 2) q r))
  '(((q.0 (1 1)) (r.0 (1 1 1)))))

(test-check 'logo-15-3
  (solve 10 (q r) (logo (build 15) (build 3) q r))
  '(((q.0 (0 1)) (r.0 (0 1 1)))))

(test-check 'logo-15-4
  (solve 10 (q r) (logo (build 15) (build 4) q r))
  '(((q.0 (1)) (r.0 (1 1 0 1)))))

(test-check 'logo-15-5
  (solve 10 (q r) (logo (build 15) (build 5) q r))
  '(((q.0 (1)) (r.0 (0 1 0 1)))))

(test-check 'logo-15-15
  (solve 10 (q r) (logo (build 15) (build 15) q r))
  '(((q.0 (1)) (r.0 ()))))

(test-check 'logo-15-16
  (solve 10 (q r) (logo (build 15) (build 16) q r))
  '(((q.0 ()) (r.0 (0 1 1 1)))))


(test-check 'logo-15--3
  (solve 10 (b r) (logo (build 15) b (build 3) r))
  '(((b.0 (1)) (r.0 (0 1 1 1)))  ; 15 = 1^3 + 14
    ((b.0 ()) (r.0 (1 1 1 1)))   ; 15 = 0^3 + 15
    ((b.0 (0 1)) (r.0 (1 1 1))))) ; 15 = 2^3 + 7

(test-check 'logo-32--4
  (solve 10 (b r) (logo (build 32) b (build 4) r))
  '(((b.0 (1)) (r.0 (1 1 1 1 1))) ((b.0 ()) (r.0 (0 0 0 0 0 1)))))

(test-check 'logo-33--5
  (solve 10 (b r) (logo (build 33) b (build 5) r))
  '(((b.0 (1)) (r.0 (0 0 0 0 0 1)))
    ((b.0 ()) (r.0 (1 0 0 0 0 1)))
    ((b.0 (0 1)) (r.0 (1)))))


(test-check 'logo-1-2
  (solve 10 (n) (logo n '(1) (build 2) '()))
  '(((n.0 (1)))))

(test-check 'logo-2-5
  (solve 10 (n) (logo n (build 2) (build 5) '(1)))
  '(((n.0 (1 0 0 0 0 1)))))

(test-check 'logo-3-2
  (solve 10 (n) (logo n (build 3) (build 2) '(1)))
  '(((n.0 (0 1 0 1)))))

(test-check 'logo-3-3
  (solve 10 (n) (logo n (build 3) (build 3) '(1)))
  '(((n.0 (0 0 1 1 1)))))

(test-check 'powers-of-3
  (solve 10 (n q r) (logo n (build 3) q r))
  '(((n.0 (1)) (q.0 ()) (r.0 ()))
    ((n.0 (0 1)) (q.0 ()) (r.0 (1)))
    ((n.0 (1 1)) (q.0 (1)) (r.0 ()))
    ((n.0 (0 0 1)) (q.0 (1)) (r.0 (1)))
    ((n.0 (0 0 0 1)) (q.0 (1)) (r.0 (1 0 1)))
    ((n.0 (1 0 1)) (q.0 (1)) (r.0 (0 1)))
    ((n.0 (1 1 1)) (q.0 (1)) (r.0 (0 0 1)))
    ((n.0 (0 1 1)) (q.0 (1)) (r.0 (1 1)))
    ((n.0 (0 0 0 0 1)) (q.0 (0 1)) (r.0 (1 1 1)))
    ((n.0 (1 0 0 0 1)) (q.0 (0 1)) (r.0 (0 0 0 1))))
)

(test-check 'powers-of-exp-3
  (solve 7 (n b r) (logo n b (build 3) r))
  '(((n.0 (1)) (b.0 (1)) (r.0 ()))
    ((n.0 _.0) (b.0 ()) (r.0 _.0))
    ((n.0 (0 0 0 1)) (b.0 (0 1)) (r.0 ()))
    ((n.0 (1 1 0 1 1)) (b.0 (1 1)) (r.0 ()))
    ((n.0 (0 1)) (b.0 (1)) (r.0 (1)))
    ((n.0 (1 0 0 1)) (b.0 (0 1)) (r.0 (1)))
    ((n.0 (0 0 1 1 1)) (b.0 (1 1)) (r.0 (1))))
)
