; Type Inference
;
; We show two variations of Hindley-Milner type inference. Both
; variations support polymorphic, generalizing `let'. Both variations
; use Kanren's logical variables for type variables, and take advantage
; of Kanren's unifier to solve the equations that arise during the course
; of type inference. These features make the Kanren realization of the
; type inference algorithm concise and lucid.
;
; The variations differ in the syntax of the `source' language, and in
; the way type environments are implemented.  One variation realizes
; type environments as regular lists, of associations between symbolic
; variable names and their types. The other variation extends the type
; entailment relation (which is a first-class relation in Kanren). The
; latter approach is similar to that of inductive proofs (see files
; ./deduction.scm and ./mirror-equ.scm)
;
; $Id: type-inference.scm,v 4.50 2005/02/12 00:05:01 oleg Exp $

(display "Type inference") (newline)

; Variation 1: use a subset of Scheme itself as the source language
; The following two functions translate between the source language
; and intermediate one.

(define parse
  (lambda (e)
    (cond
      ((symbol? e) `(var ,e))
      ((number? e) `(intc ,e))
      ((boolean? e) `(boolc ,e))
      (else (case (car e)
              ((zero?) `(zero? ,(parse (cadr e))))
              ((sub1) `(sub1 ,(parse (cadr e))))
              ((+) `(+ ,(parse (cadr e)) ,(parse (caddr e))))
              ((if) `(if ,(parse (cadr e)) ,(parse (caddr e)) ,(parse (cadddr e))))
              ((fix) `(fix ,(parse (cadr e))))
              ((lambda) `(lambda ,(cadr e) ,(parse (caddr e))))
              ((let) `(let ((,(car (car (cadr e))) ,(parse (cadr (car (cadr e))))))
                        ,(parse (caddr e))))
              (else `(app ,(parse (car e)) ,(parse (cadr e)))))))))

(define unparse
  (lambda (e)
    (case (car e)
      ((var) (cadr e))
      ((intc) (cadr e))
      ((boolc) (cadr e))
      ((zero?) `(zero? ,(unparse (cadr e))))
      ((sub1) `(sub1 ,(unparse (cadr e))))
      ((+) `(+ ,(unparse (cadr e)) ,(unparse (caddr e))))
      ((if) `(if ,(unparse (cadr e)) ,(unparse (caddr e)) ,(unparse (cadddr e))))
      ((fix) `(fix ,(unparse (cadr e))))
      ((lambda) `(lambda (,(car (cadr e))) ,(unparse (caddr e))))
      ((let) 
       `(let ((,(car (car (cadr e)))
               ,(unparse (cadr (car (cadr e))))))
          ,(unparse (caddr e))))
      ((app) `(,(unparse (cadr e)) ,(unparse (caddr e)))))))

; Type environments
;
; A type environment (often denoted as \Gamma, or g in this code)
; is an association between the names of variables of source language
; terms and the types of those variables.
; As a side condition, each variable may occur in the list
; exactly once.
; Hmm, to model lexical scope better, we may relax that condition.
;
; Here we implement type environments as regular associative lists,
; lists of triples:
;    (<var-name> non-generic <type>)
;    (<var-name> generic <type-gen>)
;
; <var-name> is a symbolic name of a source term variable.
; <type> is a type term, e.g., int, bool, (--> int bool), etc.
; <type> may include logical variables, which are treated then as
; type variables.
;
; The association '(<var-name> generic <type-gen>)' asserts that
; <var-name> is given a _generic_ type. <type-gen> then is a
; predicate of arity 1. To be more precise, (<type-gen> <type>)
; is an goal that succeeds or fails depending on the fact if
; <type> is an instance of a generic type represented by <type-gen>.
; 
; This is precisely the logical meaning of generalization, as
; pointed out by Ken:
; <blockquote>
; A cleaner, but less efficient, formulation of HM type inference is to
; use the following let rule instead:
;
;     Gamma |- M : t    Gamma |- N[M/x] : t'
;     -------------------------------------- Let
;          Gamma |- let x = M in N : t'
;
; Look ma, no FV!  In words, this rule treats let as a construct for
; syntactic substitution.  This means storing either M, or a thunk
; returning (a logical variable associated with a fresh copy of) the type
; of M, under x in the environment.  This formulation avoids var? while
; taking advantage of built-in unification (to some extent).
; </blockquote>
;
; We must emphasize that in Kanren, relations are first-class, and may,
; therefore, be included as parts of a data structure: of an associative
; list in our case.

; Because type environments are regular lists, we can build them using
; regular cons. The empty type environemnt is the empty list.  The
; following is a Kanren relation that searches the associative
; list. We are interested in the first match.

; The following is a general-purpose function
; (membero v l) holds if v is a member of the list l. 
; 'v' must be sufficiently instantiated (at least, the search key
; must be instantiated, to justify our use of the committed choice
; non-determinism).
(define membero
  (relation (v lt lh)
    (to-show v `(,lh . ,lt))
    (if-some (== v lh) succeed
      (membero v lt))))

; The following is the type-environment-specific function.
; (env g v t) holds if the source term variable v has a type t
; in the environment g.
; We require that 'v' be instantiated, to justify our use
; of the committed choice non-determinism (e.g., membero).

(define env
  (relation (head-let g v t)
    (exists (tq)
      (all!!
	(membero `(,v . ,tq) g)
	(any
	  (== tq `(non-generic ,t))
	  (exists (type-gen)
	    (all!!
	      (== tq `(generic ,type-gen))
	      (project (type-gen)
		(type-gen t)))))))))

;;;; This starts the rules

(define int 'int)
(define bool 'bool)

(define var-rel
  (relation (g v t)
    (to-show g `(var ,v) t)
    (all! (env g v t))))

(define int-rel
  (fact (g x) g `(intc ,x) int))

(define bool-rel
  (fact (g x) g `(boolc ,x) bool))

(define zero?-rel
  (relation (g x)
    (to-show g `(zero? ,x) bool)
    (all! (!- g x int))))

(define sub1-rel
  (relation (g x)
    (to-show g `(sub1 ,x) int)
    (all! (!- g x int))))

(define +-rel
  (relation (g x y)
    (to-show g `(+ ,x ,y) int)
    (all!! (!- g x int) (!- g y int))))

(define if-rel
  (relation (g t test conseq alt)
    (to-show g `(if ,test ,conseq ,alt) t)
    (all!! (!- g test bool) (!- g conseq t) (!- g alt t))))

(define lambda-rel
  (relation (g v t body type-v)
    (to-show g `(lambda (,v) ,body) `(--> ,type-v ,t))
    (all! (!- `((,v non-generic ,type-v) . ,g) body t))))

(define app-rel
  (relation (g t rand rator)
    (to-show g `(app ,rator ,rand) t)
    (exists (t-rand)
      (all!! (!- g rator `(--> ,t-rand ,t)) (!- g rand t-rand)))))

(define fix-rel
  (relation (g rand t)
    (to-show g `(fix ,rand) t)
    (all! (!- g rand `(--> ,t ,t)))))

; Type-checking polymorphic let: (let ([,v ,rand]) ,body)
; There is obviously an inefficiency, because we typecheck `rand'
; every time the variable `v' occurs in the body (and once more). 
; We can fix it, with copy term. But for now, we leave this optimization out.
; The reason to test `(!- g rand some-type)' at the very beginning is
; to make sure that `rand' itself is well-typed. As Ken pointed out,
; we must outlaw expressions such as (let ((x (z z))) y) where 'x'
; does not occur in the body. The variable 'x' still must have some
; type.

(define polylet-rel
  (relation (g v rand body t)
    (to-show g `(let ((,v ,rand)) ,body) t)
    (all!!
      (exists (some-type) (!- g rand some-type))
      (!- `((,v generic ,(relation (head-let t-rand)
			   (all!!
			     (!- g rand t-rand)
			     (trace-vars 'poly-let (t-rand rand)))))
	     . ,g)
	body t))))


(define !-
  (extend-relation (a1 a2 a3)
    var-rel int-rel bool-rel zero?-rel sub1-rel +-rel 
    if-rel lambda-rel app-rel fix-rel polylet-rel))

(test-check 'test-!-1
  (and
    (equal?
      (solution (?) (!- '() '(intc 17) int))
      '((?.0 _.0)))
    (equal?
      (solution (?) (!- '() '(intc 17) ?))
      '((?.0 int))))
  #t)

(test-check 'arithmetic-primitives
  (solution (?) (!- '() '(zero? (intc 24)) ?))
  '((?.0 bool)))

(test-check 'test-!-sub1
  (solution (?) (!- '() '(zero? (sub1 (intc 24))) ?))
  '((?.0 bool)))

(test-check 'test-!-+
  (solution (?)
      (!- '() '(zero? (sub1 (+ (intc 18) (+ (intc 24) (intc 50))))) ?))
  '((?.0 bool)))

(test-check 'test-!-2
  (and
    (equal?
      (solution (?) (!- '() '(zero? (intc 24)) ?))
      '((?.0 bool)))
    (equal?
      (solution (?) (!- '() '(zero? (+ (intc 24) (intc 50))) ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!- '() '(zero? (sub1 (+ (intc 18) (+ (intc 24) (intc 50))))) ?))
      '((?.0 bool))))
  #t)

(test-check 'test-!-3
  (solution (?) (!- '() '(if (zero? (intc 24)) (intc 3) (intc 4)) ?))
  '((?.0 int)))

(test-check 'if-expressions
  (solution (?)
    (!- '() '(if (zero? (intc 24)) (zero? (intc 3)) (zero? (intc 4))) ?))
  '((?.0 bool)))

(test-check 'variables
  (and
    (equal?
      (solution (?)
          (env '((b non-generic int) (a non-generic bool)) 'a ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!- '((a non-generic int)) '(zero? (var a)) ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!- '((b non-generic bool) (a non-generic int))
            '(zero? (var a))
            ?))
      '((?.0 bool))))
  #t)

(test-check 'variables-4a
  (solution (?)
    (!- '((b non-generic bool) (a non-generic int))
        '(lambda (x) (+ (var x) (intc 5)))
        ?))
  '((?.0 (--> int int))))

(test-check 'variables-4b
  (solution (?)
    (!- '((b non-generic bool) (a non-generic int))
        '(lambda (x) (+ (var x) (var a)))
        ?))
  '((?.0 (--> int int))))

(test-check 'variables-4c
  (solution (?)
    (!- '() '(lambda (a) (lambda (x) (+ (var x) (var a)))) ?))
  '((?.0 (--> int (--> int int)))))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- '() (parse
              '(lambda (f)
                 (lambda (x)
                   ((f x) x))))
        ?))
  '((?.0 (-->
           (--> _.0 (--> _.0 _.1))
           (--> _.0 _.1)))))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- '()
        (parse
          '((fix (lambda (sum)
                   (lambda (n)
                     (if (zero? n)
                         0
                         (+ n (sum (sub1 n)))))))
            10))
        ?))
  '((?.0 int)))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- '()
        (parse
          '((fix (lambda (sum)
                   (lambda (n)
                     (+ n (sum (sub1 n))))))
            10))
        ?))
  '((?.0 int)))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- '()
        (parse '((lambda (f)
                   (if (f (zero? 5))
                       (+ (f 4) 8)
                       (+ (f 3) 7)))
                 (lambda (x) x)))
        ?))
  #f)

(test-check 'polymorphic-let
  (solution (?)
    (!- '()
        (parse
          '(let ((f (lambda (x) x)))
             (if (f (zero? 5))
                 (+ (f 4) 8)
                 (+ (f 3) 7))))
        ?))
  '((?.0 int)))

(test-check 'with-robust-syntax
  (solution (?)
    (!- '()
        '(app
           (fix
             (lambda (sum)
               (lambda (n)
                 (if (if (zero? (var n)) (boolc #t) (boolc #f))
                     (intc 0)
                     (+ (var n) (app (var sum) (sub1 (var n))))))))
           (intc 10))
        ?))
  '((?.0 int)))

(test-check 'with-robust-syntax-but-long-jumps/poly-let
  (solution (?)
    (!- '()
        '(let ((f (lambda (x) (var x))))
           (if (app (var f) (zero? (intc 5)))
               (+ (app (var f) (intc 4)) (intc 8))
               (+ (app (var f) (intc 3)) (intc 7))))
        ?))
  '((?.0 int)))

(test-check 'type-habitation-1
  (solution (g ?)
    (!- g ? '(--> int int)))
  '((g.0 ((_.0 non-generic (--> int int)) . _.1)) (?.0 (var _.0))))

(test-check 'type-habitation-2
  (solution (g h r q z y t)
    (!- g `(,h ,r (,q ,z ,y)) t))
  '((g.0 ((_.0 non-generic int) . _.1))
    (h.0 +)
    (r.0 (var _.0))
    (q.0 +)
    (z.0 (var _.0))
    (y.0 (var _.0))
    (t.0 int))
)

(test-check 'type-habitation-3
  (and
    (equal?
      (solution (la f b)
	(!- '() `(,la (,f) ,b) '(--> int int)))
      '((la.0 lambda) (f.0 _.0) (b.0 (var _.0))))
    (equal?
      (solution (h r q z y t u v)
	(!- '() `(,h ,r (,q ,z ,y)) `(,t ,u ,v)))
      '((h.0 lambda)
        (r.0 (_.0))
        (q.0 +)
        (z.0 (var _.0))
        (y.0 (var _.0))
        (t.0 -->)
        (u.0 int)
        (v.0 int))))
  #t)

; Some historical baggage

;;; long cuts
;;; No cuts are needed any more
; (define !-generator
;   (lambda (long-cut)
;     (letrec
;       ((!- (extend-relation (a1 a2 a3)
;              (relation (g v t)
;                (to-show g `(var ,v) t)
;                (all long-cut (env g v t)))
;              (fact (g x) g `(intc ,x) int)
;              (fact (g x) g `(boolc ,x) bool)
;              (relation (g x)
;                (to-show g `(zero? ,x) bool)
;                (all long-cut (!- g x int)))
;              (relation (g x)
;                (to-show g `(sub1 ,x) int)
;                (all long-cut (!- g x int)))
;              (relation (g x y)
;                (to-show g `(+ ,x ,y) int)
;                (all long-cut (all! (!- g x int) (!- g y int))))
;              (relation (g t test conseq alt)
;                (to-show g `(if ,test ,conseq ,alt) t)
;                (all long-cut
; 		 (all! (!- g test bool) (!- g conseq t) (!- g alt t))))
;              (relation (g v t body type-v)
;                (to-show g `(lambda (,v) ,body) `(--> ,type-v ,t))
;                (all long-cut (!- `(non-generic ,v ,type-v ,g) body t)))
;              (relation (g t rand rator)
;                (to-show g `(app ,rator ,rand) t)
;                (exists (t-rand)
;                  (all long-cut
; 		   (all!
;                      (!- g rator `(--> ,t-rand ,t))
;                      (!- g rand t-rand)))))
;              (relation (g rand t)
;                (to-show g `(fix ,rand) t)
;                (all long-cut (!- g rand `(--> ,t ,t))))
;              (relation (g v rand body t)
;                (to-show g `(let ((,v ,rand)) ,body) t)
;                (exists (t-rand)
;                  (all long-cut
; 		   (all!
;                      (!- g rand t-rand)
;                      (!- `(generic ,v ,t-rand ,g) body t))))))))
;       !-)))
;
; (define !-
;   (relation/cut cut (g exp t)
;     (to-show g exp t)
;     ((!-generator cut) g exp t)))


; (relation-cond vars clause ...)
; clause::= ((local-var...) (condition ...) (conseq ...))

; (define-syntax relation-cond
;   (syntax-rules ()
;     ((_ (global-var ...) clause0 clause1 ...)
;       (lambda (global-var ...)
; 	(lambda@ (sk fk subst)
; 	  (relation-cond-clause (sk fk subst)
; 	    clause0 clause1 ...))))))

; (define-syntax relation-cond-clause
;   (syntax-rules ()
;     ((_ (sk fk subst)) (fk)) ; no more choices: fail
;     ((_ (sk fk subst) 
;        (local-vars (condition ...) conseq)
;        clause ...)
;       (let-lv local-vars			; a bit sloppy, need exists...
; 	(cout "running " '(condition ...) nl)
; 	(@ (all!! condition ...)
; 	; sk
; 	  (lambda@ (fk-ign)
; 	    (@ conseq sk fk))
; 	; fk
; 	  (lambda () (relation-cond-clause (sk fk subst) clause ...))
; 	  subst)))))

; (define !-
;   (relation-cond (g exp t)
;     ((v) ((== exp `(var ,v)))
;       (env g v t))
;     (() ((== exp `(intc ,_)) (== t int)) succeed)
;     (() ((== exp `(boolc ,_)) (== t bool)) succeed)
;     ((x) ((== exp `(zero? ,x)) (== t bool))
;       (!- g x int))
;     ((x) ((== exp `(sub1 ,x)) (== t int))
;       (!- g x int))
;     ((x y) ((== exp `(+ ,x ,y)) (== t int))
;       (all!! (!- g x int) (!- g y int)))
;     ((test conseq alt) ((== exp `(if ,test ,conseq ,alt)))
;       (all!! (!- g test bool) (!- g conseq t) (!- g alt t)))
;     ((body type-v v t1) ((== exp `(lambda (,v) ,body)) 
; 			 (== t `(--> ,type-v ,t1)))
;       (!- `(non-generic ,v ,type-v ,g) body t1))
;     ((rand rator) ((== exp `(app ,rator ,rand)))
;       (exists (t-rand)
; 	(all!!
; 	  (!- g rator `(--> ,t-rand ,t))
; 	  (!- g rand t-rand))))
;     ((rand) ((== exp `(fix ,rand)))
;       (!- g rand `(--> ,t ,t)))
;     ((v rand body) ((== exp `(let ((,v ,rand)) ,body)))
;       (!- `(generic ,v ,(relation (head-let t-rand)
; 			  (!- g rand t-rand))
; 	     ,g)
; 	body t))))

; '(define !-
;   (relation-cond (g exp t)
;     ((v) ((== exp `(var ,v)))
;       succeed)))

; (cond-expand
;   (chez
;     (pretty-print (expand '(relation-cond (g exp t)
; 			     ((v) ((== exp `(var ,v)))
; 			       succeed))))
;     )
;   (else #f))

; (test-check 'with-robust-syntax-but-long-jumps/poly-let
;   (solution (?)
;     (eigen (g)
;       (!- g
;         '(let ((f (lambda (x) (var x))))
;            (if (app (var f) (zero? (intc 5)))
;                (+ (app (var f) (intc 4)) (intc 8))
;                (+ (app (var f) (intc 3)) (intc 7))))
;         ?)))
;   '((?.0 int)))


;----------------------------------------------------------------------
; A different implementation of type environments
; We define a first-class (and recursive) relation !-
; so that (!- `(var ,v) t) holds iff the source term variable v has a type
; t. 
; This variant is close to the `natural deduction' scheme.
; It also has an OO flavor: we need open recursion.

; The following are the separate components of which the relation
; !- will be built. All these components nevertheless receive the full
; !- as the argument. Actually, they will receive the 'self'-like
; argument. We need to explicitly find the fixpoint.

(cout nl "Natural-deduction-like type inference" nl nl)


(define pint-rel
  (lambda (s!-)
    (fact (x) `(intc ,x) int)))

(define pbool-rel
  (lambda (s!-)
    (fact (x) `(boolc ,x) bool)))

(define pzero?-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (x)
	(to-show `(zero? ,x) bool)
	(all! (!- x int))))))

(define psub1-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (x)
	(to-show `(sub1 ,x) int)
	(all! (!- x int))))))

(define p+-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (x y)
	(to-show `(+ ,x ,y) int)
	(all!! (!- x int) (!- y int))))))

(define pif-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (t test conseq alt)
	(to-show `(if ,test ,conseq ,alt) t)
	(all!! (!- test bool) (!- conseq t) (!- alt t))))))

; Here we extend !- with an additional assumption that v has the type
; type-v. This extension corresponds to a non-generic, regular type.
(define plambda-rel
  (lambda (s!-)
    (relation (v t body type-v)
      (to-show `(lambda (,v) ,body) `(--> ,type-v ,t))
      (let* ((snew-!-
	       (lambda (self)
		 (extend-relation (v t)
		   (fact () `(var ,v) type-v) ; lexically-scoped relation
		   (s!- self))))
	      (!- (snew-!- snew-!-)))
	(all! (!- body t))))))


(define papp-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (t rand rator)
	(to-show `(app ,rator ,rand) t)
	(exists (t-rand)
	  (all!! (!- rator `(--> ,t-rand ,t)) (!- rand t-rand)))))))

(define pfix-rel
  (lambda (s!-)
     (let ((!- (s!- s!-)))
       (relation (rand t)
	 (to-show `(fix ,rand) t)
	 (all! (!- rand `(--> ,t ,t)))))))

; Type-checking polymorphic let: (let ((,v ,rand)) ,body)
; There is obviously an inefficiency, because we typecheck `rand'
; every time the variable `v' occurs in the body (and once more). 
; We can fix it, with copy term. But for now, we leave this optimization out.
; The reason to test `(!- g rand some-type)' at the very beginning is
; to make sure that `rand' itself is well-typed. As Ken pointed out,
; we must outlaw expressions such as (let ((x (z z))) y) where 'x'
; does not occur in the body. The variable 'x' still must have some
; type.

(define ppolylet-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (v rand body t)
	(to-show `(let ((,v ,rand)) ,body) t)
	(all!! 
	  (exists (some-type) (!- rand some-type))
	  (let* ((snew-!-
		   (lambda (self)
		     (extend-relation (v t)
		       (relation (head-let `(var ,v) t-rand)
			 (all!!
			   (!- rand t-rand)
			   (trace-vars 'poly-let (t-rand rand))))
		       (s!- self))))
		  (!- (snew-!- snew-!-)))
	    (!- body t)))))))

; Now we build the recursive !- relation, as a fixpoint

(define s!-
  (lambda (self)
    (lambda (v t)
      ((extend-relation (a1 a2)
	 (pint-rel self)
	 (pbool-rel self)  (pzero?-rel self)
	 (psub1-rel self)  (p+-rel self)
	 (pif-rel self)    (plambda-rel self)
	 (papp-rel self)   (pfix-rel self)
	 (ppolylet-rel self)) v t))))

(define !- (s!- s!-))


; And we re-do all the tests

(test-check 'test-!-1
  (and
    (equal?
      (solution (?) (!- '(intc 17) int))
      '((?.0 _.0)))
    (equal?
      (solution (?) (!- '(intc 17) ?))
      '((?.0 int))))
  #t)

(test-check 'arithmetic-primitives
  (solution (?) (!- '(zero? (intc 24)) ?))
  '((?.0 bool)))

(test-check 'test-!-sub1
  (solution (?) (!- '(zero? (sub1 (intc 24))) ?))
  '((?.0 bool)))

(test-check 'test-!-+
  (solution (?)
      (!- '(zero? (sub1 (+ (intc 18) (+ (intc 24) (intc 50))))) ?))
  '((?.0 bool)))

(test-check 'test-!-2
  (and
    (equal?
      (solution (?) (!- '(zero? (intc 24)) ?))
      '((?.0 bool)))
    (equal?
      (solution (?) (!- '(zero? (+ (intc 24) (intc 50))) ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!- '(zero? (sub1 (+ (intc 18) (+ (intc 24) (intc 50))))) ?))
      '((?.0 bool))))
  #t)

(test-check 'test-!-3
  (solution (?) (!- '(if (zero? (intc 24)) (intc 3) (intc 4)) ?))
  '((?.0 int)))

(test-check 'if-expressions
  (solution (?)
    (!- '(if (zero? (intc 24)) (zero? (intc 3)) (zero? (intc 4))) ?))
  '((?.0 bool)))

; Commented out: we need to extend !- if we wish to typecheck open terms
'(test-check 'variables
  (and
    (equal?
      (solution (?)
          (env '((b non-generic int) (a non-generic bool)) 'a ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!- '((a non-generic int)) '(zero? (var a)) ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!- '((b non-generic bool) (a non-generic int))
            '(zero? (var a))
            ?))
      '((?.0 bool))))
  #t)

(test-check 'variables-4a
  (solution (?)
    (!- '(lambda (x) (+ (var x) (intc 5)))
        ?))
  '((?.0 (--> int int))))

; Commented out: we need to extend !- if we wish to typecheck open terms
'(test-check 'variables-4b
  (solution (?)
    (!- '((b non-generic bool) (a non-generic int))
        '(lambda (x) (+ (var x) (var a)))
        ?))
  '((?.0 (--> int int))))

(test-check 'variables-4c
  (solution (?)
    (!- '(lambda (a) (lambda (x) (+ (var x) (var a)))) ?))
  '((?.0 (--> int (--> int int)))))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- (parse
	  '(lambda (f)
	     (lambda (x)
	       ((f x) x))))
      ?))
  '((?.0 (-->
           (--> _.0 (--> _.0 _.1))
           (--> _.0 _.1)))))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- (parse
          '((fix (lambda (sum)
                   (lambda (n)
                     (if (zero? n)
                         0
                         (+ n (sum (sub1 n)))))))
            10))
        ?))
  '((?.0 int)))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- (parse
          '((fix (lambda (sum)
                   (lambda (n)
                     (+ n (sum (sub1 n))))))
            10))
        ?))
  '((?.0 int)))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- (parse '((lambda (f)
                   (if (f (zero? 5))
                       (+ (f 4) 8)
                       (+ (f 3) 7)))
                 (lambda (x) x)))
        ?))
  #f)

(test-check 'polymorphic-let
  (solution (?)
    (!- (parse
          '(let ((f (lambda (x) x)))
             (if (f (zero? 5))
                 (+ (f 4) 8)
                 (+ (f 3) 7))))
        ?))
  '((?.0 int)))

(test-check 'with-robust-syntax
  (solution (?)
    (!- '(app
           (fix
             (lambda (sum)
               (lambda (n)
                 (if (if (zero? (var n)) (boolc #t) (boolc #f))
                     (intc 0)
                     (+ (var n) (app (var sum) (sub1 (var n))))))))
           (intc 10))
        ?))
  '((?.0 int)))

(test-check 'with-robust-syntax-but-long-jumps/poly-let
  (solution (?)
    (!- '(let ((f (lambda (x) (var x))))
           (if (app (var f) (zero? (intc 5)))
               (+ (app (var f) (intc 4)) (intc 8))
               (+ (app (var f) (intc 3)) (intc 7))))
        ?))
  '((?.0 int)))

; The latter doesn't work: but it wasn't too informative anyway
'(test-check 'type-habitation-1
  (solution (?)
    (!- ? '(--> int int)))
  '((g.0 ((v.0 non-generic (--> int int)) . lt.0)) (?.0 (var v.0))))

(test-check 'type-habitation-2
  (solution (h r q z y t)
    (!- `(,h ,r (,q ,z ,y)) t))
  '((h.0 +)
    (r.0 (intc _.0))
    (q.0 +)
    (z.0 (intc _.1))
    (y.0 (intc _.2))
    (t.0 int))
)

(test-check 'type-habitation-3
  (and
    (equal?
      (solution (la f b)
	(!-  `(,la (,f) ,b) '(--> int int)))
      '((la.0 lambda) (f.0 _.0) (b.0 (var _.0))))
    (equal?
      (solution (h r q z y t u v)
	(!-  `(,h ,r (,q ,z ,y)) `(,t ,u ,v)))
      '((h.0 lambda)
        (r.0 (_.0))
        (q.0 +)
        (z.0 (var _.0))
        (y.0 (var _.0))
        (t.0 -->)
        (u.0 int)
        (v.0 int))))
  #t)


; The code below uses the low-level function var? Every use of var?
; entails a proof obligation that such use is safe. In our case here,
; invertible-binary-function->ternary-relation and
; invertible-unary-function->binary-relation are sound.

(define invertible-binary-function->ternary-relation
  (lambda (op inverted-op)
    (relation (head-let x y z)
      (project/no-check (z)
	(if-only (predicate (var? z))
          (project (x y) (== z (op x y))) ; z is free, x and y must not
	  (project/no-check (y)
	    (if-only (predicate (var? y)) ; y is free, z is not
	      (project (x)
		(== y (inverted-op z x)))
	      (project/no-check (x)
		(if-only (predicate (var? x)) ; x is free, y and z are not
		  (== x (inverted-op z y))
		  (== z (op x y)))))))))))


(define ++ (invertible-binary-function->ternary-relation + -))
(define -- (invertible-binary-function->ternary-relation - +))
(define ** (invertible-binary-function->ternary-relation * /))
(define // (invertible-binary-function->ternary-relation / *))

(test-check 'test-instantiated-1
  (and
    (equal?
      (solution (x) (++ x 16.0 8))
      '((x.0 -8.0)))
    (equal?
      (solution (x) (++ 10 16.0 x))
      '((x.0 26.0)))
    (equal?
      (solution (x) (-- 10 x 3))
      '((x.0 13))))
  #t)

(define symbol->lnum
  (lambda (sym)
    (map char->integer (string->list (symbol->string sym)))))

(define lnum->symbol
  (lambda (lnums)
    (string->symbol (list->string (map integer->char lnums)))))

(define invertible-unary-function->binary-relation
  (lambda (op inverted-op)
    (relation (head-let x y)
      (project/no-check (y)
	(if-only (predicate (var? y))
	  (project (x) (== y (op x)))	; y is free, x must not
	  (project/no-check (x)
	    (if-only (predicate (var? x))
	      (== x (inverted-op y))
	      (== y (op x)))))))))

(define name
  (invertible-unary-function->binary-relation symbol->lnum lnum->symbol))

(test-check 'test-instantiated-2
  (and
    (equal?
      (solution (x) (name 'sleep x))
      '((x.0 (115 108 101 101 112))))
    (equal?
      (solution (x) (name x '(115 108 101 101 112)))
      '((x.0 sleep))))
  #t)
