(cout nl "Structural Inductive proof: mirror" nl)
;
; $Id: mirror.scm,v 4.50 2005/02/12 00:04:47 oleg Exp $

; First we need an extendible database of relations.
; We should be able to add to the database later on -- extend
; it with assumptions.
;
; One approach for the database is a finite map (hash table, assoc
; list) from the name of a relation to the procedure that is a relation
; in our system. Or, to make it even better, from a tuple
; (name arity) to the body of the relation.
; This is the approach of Prolog.
; Suppose we have a term (foo ?a ?b ?c) where ?a, ?b and ?c are arbitrary
; terms (logical variables, constants, expressions, etc). 
; We would like to check if this term is consistent with (i.e., can
; be proven by) a particular instance of the database.
; First, we need to look up a key (foo 3) in the database. If the
; lookup fails, so does our query. If the lookup succeeds, we get
; a procedure of three arguments. We apply this procedure to
; ?a, ?b, and ?c and obtain an goal, which we can 'solve'
; as usual.

; In the following, we chose a different approach. We represent the database
; of relations as a relation itself -- we will call it KB. That
; relation takes one argument -- the term to prove, and returns an goal
; that represents the answer (that goal may be 'fail').
; A database of one fact
;  foo(a,b,c).
; in Prolog notation will be represented in our approach as a relation
;   (relation _ () (to-show `(foo a b c)))
; If we want to add another relation, say
; bar(X,X).
; we need to _extend_ the above relation with
;  (relation _ (x) (to-show `(bar x x))).
;
; This approach is probably less efficient than the first one. It has
; however a redeeming value -- we do not need a separate procedure
; to look up names/arities of relations. We don't need separate procedures
; for extending our database. We can use the existing machinery of
; 'solving' relations for 'solving' the database of relations.
; This approach seems reminiscent of the Futamura projections:
; we use the same engine for meta-evaluations. Bootstrapping.

; First we define the inductive structure

; In Athena:
;  (structure (BTree S)
;     (leaf S)
;     (root (BTree S) (BTree S)))

; In Prolog
; btree(leaf(S)).
; btree(root(T1,T2)) :- btree(T1),btree(T2).

; Note, our trees here (as well as those in Prolog) are polytypic
; (polymorphic): leaves can have values of different sorts.

; When we attempt to translate
;	btree(root(T1,T2)) :- btree(T1),btree(T2).
; into our system, we encounter the first difficulty. To find out
; if a term btree(root(T1,T2)) is consistent with our database of relations,
; we need to check if terms  btree(T1) and  btree(T2) are consistent.
; Thus, to add btree(root(T1,T2)) to our database, we need to use
; the database itself to verify btree(T1) and btree(T2). Clearly,
; we need a fixpoint. The need for the fixpoint exists no matter what is
; the representation of the database -- a finite map or a relation.
; Prolog solves the fixpoint problem by making the database global
; and using mutations (similar to the way letrec is implemented in Scheme).
; If we attempt to be purely functional, we must make the fixpoint explicit
; and employ Y.

; Note, the kb variable below represents the "current" database.
; In our approach, the database is a relation of one argument,
; which is a term to prove. A Second-order relation???

(define btree
  (lambda (kb)
    (extend-relation (t)
      (fact (val) `(btree (leaf ,val)))
      (relation (t1 t2)
	(to-show `(btree (root ,t1 ,t2)))
	(project (t1 t2)
          (all
	    (predicate (printf "btree ~s ~s ~n" t1 t2))
	    (kb `(btree ,t1))
	    (kb `(btree ,t2))))))))

;%> (declare mirror ((S) -> ((BTree S)) (BTree S)))

; Introduce an equality predicate and the first axiom for mirror
; In Athena:
; (define mirror-axiom-1
;   (forall ?x
;     (= (mirror (leaf ?x)) (leaf ?x))))

; In Prolog
; myeq(leaf(X),mirror(leaf(X))).

(define mirror-axiom-eq-1
  (lambda (kb)
    (fact (val) `(myeq (leaf ,val) (mirror (leaf ,val))))))

; The second axiom
; In Athena:
; (define mirror-axiom-eq-2
;   (forall ?t1 ?t2
;     (= (mirror (root ?t1 ?t2))
;       (root (mirror ?t2) (mirror ?t1)))))

; In Prolog
; myeq(root(B,A),mirror(root(T1,T2))) :- myeq(A,mirror(T1)),myeq(B,mirror(T2)).

; implicitly the axiom in Prolog and the one below assume
; the transitivity of myeq. Indeed, one may think that the direct
; translation from Athena to Prolog would be
;
;   myeq(mirror(root(T1,T2)),root(mirror(T2),mirror(T1)))
; or
;   myeq(mirror(root(T1,T2)),root(B,A)) :- B = T2, A = T1.
; However, Athena actually assumes that B and T2 can be myeq rather
; than merely identical. We also switched the order of arguments
; in myeq, assuming symmetry of myeq.
; It really helped in Prolog. In our system, we could have used
; the same order as in Athena and add:
;    myeq(A,A).   % reflexivity: identity implies equality
;    myeq(A,B) :- myeq(B,A). % symmetry
; Clearly if we add these relations to Prolog code, it will diverge.
; In our system, we can use with-depth to keep divergence in check.
; Still, for simplicity and clarity we will simply model the Prolog solution
; in our code.

(define mirror-axiom-eq-2
  (lambda (kb)
    (relation  (a b t1 t2)
      (to-show `(myeq (root ,b ,a) (mirror (root ,t1 ,t2))))
      (all
	(kb `(myeq ,a (mirror ,t1)))
	(kb `(myeq ,b (mirror ,t2)))))))

; we could also add reflexivity and transitivity and symmetry axioms
; and with-depth to keep them from diverging.

; Define the goal
; In Athena:
;  (define (goal t)
;     (= (mirror (mirror t)) t))

; In Prolog
; Note, the goal is _equivalent_ to the conjunction of the
; predicates. That's why we couldn't use the standard Prolog
; notation goal(T) :- btree(T), ...
; because the latter would give us only the implication.
; goal(T,[btree(T),myeq(T,mirror(T1)),myeq(T1,mirror(T))]).

(define goal
  (lambda (t)
    (let-lv (t1)
      (list
	`(btree ,t)
	`(myeq ,t  (mirror ,t1))
	`(myeq ,t1 (mirror ,t))))))

; For clarity, the above predicate can be written as two (prolog) relations
; The forward relation:
; (goal t) is implied by (btree t), (myeq t (mirror t1)) and 
;                        (myeq t1 (mirror t))
; In the above, t is universally quantified and t1 is existentially
; quantified

(define goal-fwd
  (lambda (kb)
    (relation (t t1)
      (to-show `(goal ,t))
      (all
	(kb `(btree ,t))
	(kb `(myeq ,t  (mirror ,t1)))
	(kb `(myeq ,t1 (mirror ,t)))))))

; The reverse relation for the goal:
; (goal t) implies (btree t), (myeq t (mirror t1)) and 
;                             (myeq t1 (mirror t))
; In the above, t is universally quantified and t1 is existentially
; quantified
; Because t1 now appears on the left-hand side, it is represented
; as an eigenvariable (skolem function) rather than a logical variable

(define goal-rev
  (let* ((sk (eigen-variable 'sk))
	 (t1-sk (lambda (t) `(,sk ,t))))
    (lambda (kb)
      (extend-relation (t)
	(relation (t)			; (goal t) => (btree t)
	  (to-show `(btree ,t))
	  (kb `(goal ,t)))
	(relation (t)			; (goal t) => (myeq t  (mirror t1))
	  (to-show `(myeq ,t  (mirror ,(t1-sk t))))
	  (kb `(goal ,t)))
	(relation (t)			; (goal t) => (myeq t1 (mirror t))
	  (to-show `(myeq ,(t1-sk t) (mirror ,t)))
	  (kb `(goal ,t)))
	))))
      
; The initial assumptions: just the btree
(define init-kb (Y btree))

; Verification engine
;	verify-goal PREDS KB
; returns a nullary relation that is the conjunction of preds against the
; assumption base kb
(define verify-goal
  (lambda (preds kb)
    (cond
      ((null? (cdr preds)) (kb (car preds)))
      (else (all
              (kb (car preds))
              (verify-goal (cdr preds) kb))))))

; extend the kb with the list of assumptions
; this is just like 'any' only it's a procedure rather than a syntax
; Why we need universalize?
; Suppose, the list of facts includes
;	(fact (x) (foo x)) and (fact (x) (bar x))
; definitely, we do not want to imply that facts foo and bar _share_
; the same logical variable. The facts are independent and should
; not have any variables in common.
; Furthermore, we do not want to add
;	(fact (x) (foo x))
; because that would mean exist x. foo x
; We want our facts to be universally quantified. So, we add
;	(fact () (foo 'unique-symbol))
; See the distinction between sigma and pi in Lambda-Prolog.
; We use extend-kb to extend the database with assumptions, which most
; often are universally quantified.

(define extend-kb
  (lambda (facts kb)
    (let ((facts (universalize facts)))
      (printf "Extending KB with ~s~%" facts)
      (let loop ((facts facts))
        (if (null? facts) kb
            (extend-relation (t)
              (fact () (car facts))
              (loop (cdr facts))))))))

; Here's Athena's induction proof.
;
; (by-induction-on ?t (goal ?t)
;   ((leaf x) (!pf (goal (leaf x)) [mirror-axiom-1]))
;   ((root t1 t2) 
;     (!pf (goal (root t1 t2)) [(goal t1) (goal t2)  mirror-axiom-2])))

; The first part of it, the base case, can be expressed in Prolog
; as follows.
; ?- goal(leaf(X),C),verify(C,[]).
; Here how it looks in our system:
(test-check "First check the base case"
  (query (_ subst)
    (verify-goal (goal '(leaf x))
      (extend-relation (t) (mirror-axiom-eq-1 init-kb) init-kb))
    (reify-subst '() subst))
  '((val.0 x) (t1.0 (leaf x)) (val.0 x) (val.0 x)))

(test-check "Check the base case, using goal-fwd"
  (query (_ subst)
    (let ((kb0
	    (extend-relation (t) (mirror-axiom-eq-1 init-kb) init-kb)))
      (let ((kb1
	      (extend-relation (t) (goal-fwd kb0) kb0)))
	(kb1 '(goal (leaf x))))) ; note, x is an eigenvariable!
    (reify-subst '() subst))
  '((val.0 x) (t1.0 (leaf x)) (val.0 x) (val.0 x) (t.0 (leaf x))))

; that is, we obtain the list of subgoals to verify '(leaf x)
; by invoking the function 'goal'.
; we extend the initial database (which contains btree facts)
; with mirror-axiom-eq-1. Thus, mirror-axiom-eq-1 and btree form
; the assumptions. We then verify the subgoals against the assumptions.
; Note that we wrote
;    '(leaf x)
; rather than
;    (let-lv (x) `(leaf ,x))
; because we want to prove that (goal '(leaf x)) holds for _all_ x
; rather than for some particular x.
;
; non-empty result printed by the above expressions means success...


; The inductive case.
; Now, assume the goal holds for t1 and t2 and check if it holds
; for root(t1,t2)
;?- goal(t1,A1),goal(t2,A2), append(A1,A2,A), goal(root(t1,t2),C), verify(C,A).

(test-check "Some preliminary checks"
  (solution (foo)
    (verify-goal '((btree t2)) ; (goal t2) => (btree t2)
      (let ((kb0
	      (extend-kb (goal 't1) 
		(extend-kb (goal 't2) init-kb))))
	kb0)))
  '((foo.0 _.0)))

(test-check "Some preliminary checks, using goal-rev"
  (solution (foo)
    (let ((kb
	    (Y
	      (lambda (kb)
		(extend-relation (t)
		  (btree kb)
		  (goal-rev kb)
		  (fact () '(goal t1))
		  (fact () '(goal t2)))))))
      (kb '(btree t2))))
  '((foo.0 _.0)))

; the above two expressions should give the same result: a non-empty stream
; (with an empty substitution: no variables leak)

(test-check "Another check"
  (solution (foo)
	;(goal t1), (goal t2) => (btree (root t1 t2))
    (verify-goal '((btree t1) (btree t2)
		   (btree (root t1 t2)))
      (let ((kb0
	      (extend-kb (goal 't1) 
		(extend-kb (goal 't2) 
		  (fact () 'nothing)))))
	(Y
	  (lambda (kb)
	    (extend-relation (t)
	      kb0
	      (btree kb)
	      (mirror-axiom-eq-2 kb)))))))
  '((foo.0 _.0)))

(test-check "Another check, using goal-rev"
  (solution (foo)
    (let ((kb
	    (Y
	      (lambda (kb)
		(extend-relation (t)
		  (btree kb)
		  (goal-rev kb)
		  (mirror-axiom-eq-2 kb)
		  (fact () '(goal t1))
		  (fact () '(goal t2)))))))
      (kb '(btree (root t1 t2)))))
  '((foo.0 _.0)))

; now we really need Y because we rely on the clause
;	btree(root(T1,T2)) :- btree(T1),btree(T2).
; which is recursive.

(test-check "Check the inductive case"
  (query (_ subst)
    (verify-goal (goal '(root t1 t2))
      (let ((kb0
	      (extend-kb (goal 't1) 
		(extend-kb (goal 't2) 
		  (fact () 'initial)))))
	(Y
	  (lambda (kb)
	    (extend-relation (t)
	      kb0
	      (btree kb)
	      (mirror-axiom-eq-2 kb))))))
    (cout (reify-subst '() subst) nl) #t)
  #t)

(printf "~%Check particulars of the inductive case, using goal-rev, goal-fwd ~s~%"
  (let ((kb
          (Y
            (lambda (kb)
              (extend-relation (t)
                (btree kb)
                (fact () '(goal t1))
                (fact () '(goal t2))
                (mirror-axiom-eq-2 kb)
                (goal-rev kb)
                )))))
    (list
      (solve 1 (x) (kb `(myeq (root t1 t2)  (mirror ,x))))
      (solve 1 (x) (kb `(myeq ,x (mirror (root t1 t2))))))))

(test-check "Check the inductive case, using goal-rev, goal-fwd"
  (query (_ subst)
    (let ((kb
	    (Y
	      (lambda (kb)
		(extend-relation (t)
		  (btree kb)
		  (fact () '(goal t1))
		  (fact () '(goal t2))
		  (mirror-axiom-eq-2 kb)
		  (goal-rev kb))))))
      (let ((kb1 (goal-fwd kb)))
	(kb1 '(goal (root t1 t2)))))
    (cout (reify-subst '() subst) nl) #t)
  #t)


; Again, we use Y because btree and mirror-axiom-eq-2 are recursive.
; We need the database that is the fixpoint of all constituent
; relations.
; The output above is a non-empty list: meaning that the inductive
; phase of the proof checks.
