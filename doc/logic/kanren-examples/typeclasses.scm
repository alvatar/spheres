(newline)
(display "Checking for dependency satisfaction in Haskell typeclasses")
(newline)
; Suppose we have the following Haskell class and instance declarations
;      class C a b c | a b -> c 
;      instance C a b c => C a (x,y,b) c
;      instance C a (a,c,b) c
;
; They will be compiled into the following database of instances,
; which define the class membership.
(define typeclass-C-instance-1
  (relation (a b c x y)
    (to-show a `(,x ,y ,b) c)
    (typeclass-C a b c)))

(define typeclass-C-instance-2
  (relation (a b c)
    (to-show a `(,a ,c ,b) c)
    succeed))

(define typeclass-C
  (extend-relation (a b c) 
    typeclass-C-instance-2
    typeclass-C-instance-1))

; Run the checker for the dependency a b -> c
; Try to find the counter-example, that is, two members of (C a b c)
; such that a's and b's are the same but the c's are different.

  
(define typeclass-counter-example-query
  (lambda (a b c1 c2)
    (all 
      (typeclass-C a b c1)
      (typeclass-C a b c2)
      (fails (project/no-check (c1 c2) (predicate (*equal? c1 c2)))))))

(printf "~%Counter-example: ~s~%"
  (solution (a b c1 c2)
    (typeclass-counter-example-query a b c1 c2)))

; This does loop
'(define typeclass-C
   (extend-relation (a b c) 
     typeclass-C-instance-1
     typeclass-C-instance-2))

(define typeclass-C
  (extend-relation-with-recur-limit 2 (a b c)
    typeclass-C-instance-1
    typeclass-C-instance-2))

(printf "~%Counter-example: ~s~%"
  (solution (a b c1 c2)
    (typeclass-counter-example-query a b c1 c2)))

(printf "~%Counter-example: ~s~%"
  (solve 4 (a b c1 c2)
    (typeclass-counter-example-query a b c1 c2)))

(printf "~%Test: checking dependency satisfaction: Another example.~%")
; Suppose we have the following Haskell class and instance declarations
;	class F a b | a->b
;	instance F a b => F [a] [b]
;	instance F [a] a
;

(define typeclass-F
  (extend-relation-with-recur-limit 10 (a b)
    (relation (a b)
      (to-show `(list ,a) `(list ,b))
      (typeclass-F a b))
    (fact (a) `(list ,a) a)))


; Run the checker for the dependency a -> b
; Try to find the counter-example, that is, two members of (F a b)
; such that as is the same but bs are different.
(define typeclass-F-counter-example-query
  (lambda (a b1 b2)
    (all 
      (typeclass-F a b1)
      (typeclass-F a b2)
      (fails (project/no-check (b1 b2) (predicate (*equal? b1 b2)))))))

 (printf "~%Counter-example: ~s~%" 
   (solve 4 (a b1 b2) (typeclass-F-counter-example-query a b1 b2)))

 
(printf "~%Overloading resolution in Haskell.~%")
; Suppose we have the following Haskell class and instance declarations
;	class F a b | a->b where f :: a->b->Bool
;	instance F a b => F [a] [b]
;
; we need to typecheck
;   g x = f [x] x
; which says that f:: [a] -> a -> Bool
; In general, we need to figure out which instance to choose for f.
; In other words, we need to find out which subset of F to use.
; Here's only one instance. So we need to figure out if it applies.

(define typeclass-F-instance-1
  (relation (a b)
    (to-show `(list ,a) `(list ,b))
    (typeclass-F a b)))

; This is a closed-world assumption
(define typeclass-F
  (extend-relation-with-recur-limit 10 (a b)
    typeclass-F-instance-1))

(test-check "Typechecking (closed world)" 
  (solve 4 (a)
    (typeclass-F-instance-1 `(list ,a) a))
  '())					; meaning: does not typecheck!

; This is an open-world assumption
(define typeclass-F
  (extend-relation-with-recur-limit 2 (a b)
    typeclass-F-instance-1
    (relation (a b1 b2)	; a relation under constraint a->b
      (to-show a b1)
      (fails
	(all!
	  (typeclass-F a b2)
	  (fails (project/no-check (b1 b2) (predicate (*equal? b1 b2)))))))
    ))

(printf "~%Typechecking (open world): ~s~%" 
  (solve 4 (a) (typeclass-F-instance-1 `(list ,a) a)))

(test-check "Typechecking (open world) f [x] int" 
  (solve 4 (a) (typeclass-F-instance-1 `(list ,a) 'int))
  '())					; meaning: does not typecheck!
