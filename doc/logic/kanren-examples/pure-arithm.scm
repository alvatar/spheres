;	     Pure, declarative, and constructive arithmetics
;
; aka: division as relation.
; The function //o below is a KANREN relation between three Peano numerals
; n, m, and q such that the following holds
;	exists r. 0<=r<m, n = q*m + r
;
; That relation is more remarkable than it may look. First, it's possible to
; ask (//o 1 0 q). Obviously, such q does not exist. The relation
; thus fail -- it does not try to enumerate all natural numbers.
; A more general relation is (divo n m q r). We can invoke it like
; (divo 5 m 1 _), where n is a free variable. The answers are (5 4 3).
; Again, (divo 5 m 7 _) simply fails and does not loop forever.
; We can even try (//o x y z), which returns a stream of triples
; (((x.0 ()) (y.0 (() . *anon.0)) (z.0 ()))
;  meaning 0/n = 0 for any n >0
; ((x.0 (() . *anon.0)) (y.0 (() . *anon.0)) (z.0 (())))
;   meaning (1+a)/(1+a) = 1 for any a >=0
; ((x.0 x.0) (y.0 (())) (z.0 x.0))
;   meaning x/1=x for any x
; We can derive algebraic equalities!
;
; Such relations are easy to implement in an impure system such as Prolog,
; with the help of a predicate 'var'. The latter can tell if its argument
; is an uninstantiated variable. However, 'var' is impure. The present
; file shows the implementation of arithmetic relations in a _pure_
; logic system.
; 
; Furthermore, by judicially setting sound boundaries, we make the system
; constructive. So, our arithmetic relations are not only pure
; and sound but also complete.
;
; $Id: pure-arithm.scm,v 4.50 2005/02/12 00:04:48 oleg Exp $

; Auxiliary functions to build and show Peano numerals
; ()          represents 0
; (())        represents 1
; (() ())     represents 2
; (() () () ) represents 3, etc.
;
(define (build n)
  (if (zero? n) '() (cons '() (build (- n 1)))))

(define trans length)

; (zeroo x) holds if x is zero Peano numeral
(define zeroo
  (fact () '()))

; (succo a b) holds if b = a + 1
(define succo
  (extend-relation (a b)
    (fact () '() '( () ))
    (fact (x) `(() . ,x) `(() () . ,x)))) 

; (less a b): a < b
(define less
  (extend-relation (a l)
    (fact () '() `(() . ,_))
    (relation (x l) (to-show `(() . ,x) `(() . ,l)) (less x l))))

(define succo-limit ; a+1 = b; b < l
  (extend-relation (a b l)
    (fact () '() '(()) `(() () . ,_))
    (relation (x l)
      (to-show `(() . ,x) `(() () . ,x) `(() () . ,l))
      (less x l))))

(define pos-succo-limit ; a+1 = b; b < l, a>0
  (relation (x l)
    (to-show `(() . ,x) `(() () . ,x) `(() () . ,l))
    (less x l)))

; a + b = c
(define ++o
  (relation (head-let a b c)
    (any
      (all (zeroo b) (== a c))  ; a + 0 = a
      (all (zeroo a) (== b c))  ; 0 + b = b
      ; if a>0 and b>0 then exists a1, a1+1 = a and a < (a+b),
      ; hence succo-limit
      (exists (a1 c1) (all  (succo-limit a1 a c) (++o a1 b c1) (succo c1 c)))
      )))

; a - b = c
(define --o
  (lambda (x y out)
    (++o y out x)))
      
(define <o  ; n < m iff n == (m-1) or n < (m-1)
  (relation (head-let n m)
    (exists (m1)
      (all (succo m1 m) (any (== n m1) (<o n m1))))))

(define <o less)

; n * m = p
(define **o
  (relation (head-let n m p)
    (any
      (all (zeroo n) (== p '()))	; 0 * m = 0
      (all (zeroo m) (== p '()))	; n * 0 = 0
      (all (succo '() n) (== m p))	; 1 * m = m
      (all (succo '() m) (== n p))	; n * 1 = n
      (exists (n1 p1)
	; if n>1 and m>1 then (n*m) > n and exists n1, n1+1=n
	; (n+1)* m = n*m + m
	(all (pos-succo-limit n1 n p) 
	     (pos-succo-limit _  m p)
	  (**o n1 m p1) (++o p1 m p))))))

; n = q*m + r
; where 0<=r<m
; The following however loops if 'm' is not defined. See divo below
; for a more general and better behaving relation.
(define //o
  (relation (head-let n m q)
    (exists (r p)
      (all (<o r m)  (++o p r n) ;(trace-vars 1 (p r n))
	(**o q m p)))))

; n = q*m + r
; where 0<=r<m
(define divo
  (relation (head-let n m q r)
    (any-interleave
      (all (== q '()) (== r n) (<o n m))      ; if n < m, then q=0, n=r
      (all (== n m) (== q '(())) (== r '()))  ; n = 1*n + 0
      (exists (p)
	(all (<o m n) (<o r m)  (++o p r n) ;(trace-vars 1 (p r n))
	  (**o q m p))))))



(define-syntax test
  (syntax-rules ()
    ((_ (x) gl)
      (query (redok subst x) gl
	(display (trans (subst-in x subst)))
	(newline)))))

(cout nl "addition" nl)
(test (x) (++o (build 29) (build 3) x))
(test (x) (++o (build 3) x (build 29)))
(test (x) (++o x (build 3) (build 29)))
(test-check 'addition-all-1
  (solve 10 (w)
    (exists (y z)
      (all (++o y z (build 4))
	(project (y z) (== `(,(trans y) ,(trans z)) w)))))
   '(((w.0 (4 0)))
     ((w.0 (0 4)))
     ((w.0 (1 3)))
     ((w.0 (2 2)))
     ((w.0 (3 1))))
  )

(cout nl "subtraction" nl)
(test (x) (--o (build 29) (build 3) x))
(test (x) (--o (build 29) x (build 3)))
(test (x) (--o x (build 3) (build 26)))
(test (x) (--o (build 29) (build 29) x))
(test (x) (--o (build 29) (build 30) x))
(test-check 'subtraction-all-1
  (solve 5 (w)
    (exists (y z)
      (all (--o y z (build 4))
	(project (y z) (== `(,(trans y) ,(trans z)) w)))))
   '(((w.0 (4 0)))
     ((w.0 (5 1)))
     ((w.0 (6 2)))
     ((w.0 (7 3)))
     ((w.0 (8 4))))
  )

(cout nl "comparisons" nl)
(test (x) (<o x (build 4)))
(test (x) (all (== x (build 3)) (<o x (build 4))))
(test (x) (all (== x (build 4)) (<o x (build 3))))
(cout (solve 5 (x) (<o x (build 3))) nl)


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
  '(((w.0 (1 6))) ((w.0 (6 1))) ((w.0 (2 3))) ((w.0 (3 2)))))


(cout nl "division" nl)
(test (x) (//o (build 4) (build 2) x))
(test-check 'div-fail-1 (test (x) (//o (build 4) (build 0) x)) '())
(test (x) (//o (build 4) (build 3) x))
(test (x) (//o (build 4) (build 4) x))
(test (x) (//o (build 4) (build 5) x))

(test (x) (//o (build 33) (build 3) x))
(test (x) (//o (build 33) x (build 11)))
(test (x) (//o x (build 3) (build 11)))

(test-check 'div-all-1
  (solve 3 (x y z) (//o x y z))
'(((x.0 ()) (y.0 (() . *anon.0)) (z.0 ())) ; 0/n = 0 for any n >0
 ((x.0 (() . *anon.0)) (y.0 (() . *anon.0)) (z.0 (())))
 ((x.0 x.0) (y.0 (())) (z.0 x.0)))
)

(cout nl "division, general" nl)

(test (x) (divo (build 4) (build 2) x _))
(test-check 'div-fail-1 (test (x) (divo (build 4) (build 0) x _)) '())
(test (x) (divo (build 4) (build 3) x _))
(test (x) (divo (build 4) (build 4) x _))
(test (x) (divo (build 4) (build 5) x _))
(test (x) (divo (build 4) (build 5) _ x))

(test (x) (divo (build 33) (build 3) x _))
(test (x) (divo (build 33) x (build 11) _))
(test (x) (divo x (build 3) (build 11) _))

(test (x) (divo x (build 5) _ (build 4)))
(test (x) (divo x (build 5) (build 3) (build 4)))
(test (x) (divo x _ (build 3) (build 4)))
(test-check 'div-fail-2 (test (x) (divo (build 5) x (build 7) _)) '())

; Check later
(test-check 'div-all-2
  (solve 7 (w) 
    (exists (z) (all (divo (build 5) z (build 1) _)
		    (project (z) (== `(,(trans z)) w)))))
  '(((w.0 (5))) ((w.0 (3))) ((w.0 (4)))))

(test-check 'div-all-3
  (solve 3 (x y z r) (divo x y z r))
'(((x.0 ()) (y.0 (() . *anon.0)) (z.0 ()) (r.0 ())) ; 0 = 1*0 + 0
 ((x.0 y.0) (y.0 y.0) (z.0 (())) (r.0 ()))
 ((x.0 (() () . *anon.0))
  (y.0 (()))
  (z.0 (() () . *anon.0))
  (r.0 ())))
)

; the following take a long time
;(test (x) (//o (build 33) (build 5) x))
;(test (x) (//o (build 29) (build 3) x))

