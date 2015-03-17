; Taken from _Algebra 1_, Glencoe/McGraw-Hill, New York, New York, 1998 
; pg. 411, Problem 56
; 
; There are 8 houses on McArthur St, all in a row.  These houses
; are numbered from 1 to 8.
; 
; Allison, whose house number is greater than 2, lives next door
; to her best friend, Adrienne.  Belinda, whose house number is
; greater than 5, lives 2 doors away from her boyfriend, Benito.
; Cheri, whose house number is greater than Benito's, lives
; three doors away from her piano teacher, Mr. Crawford.  Daryl,
; whose house number is less than 4, lives 4 doors from his
; teammate, Don.  Who lives in each house?

(define pos
  (relation (name address)
    (to-show name address)
    (pump address)))

(define pump
  (extend-relation (a1)
    (fact () 1)
    (fact () 2)
    (fact () 3)
    (fact () 4)
    (fact () 5)
    (fact () 6)
    (fact () 7)
    (fact () 8)))

(define no-dups
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [(null? (cdr ls)) #t]
      [(= (car ls) (cadr ls)) #f]
      [else (no-dups (cdr ls))])))
	    
(define mapping
  (relation (n1 n2 n3 n4 n5 n6 n7 n8)
    (to-show n1 n2 n3 n4 n5 n6 n7 n8)
    (all
      (pos 'allison n1)
      (pos 'adrienne n2)
      (pos 'belinda n3)
      (pos 'benito n4)
      (pos 'cheri n5)
      (pos 'crawford n6)
      (pos 'daryl n7)
      (pos 'don n8)
      (project (n1 n2 n3 n4 n5 n6 n7 n8)
	(all!!
	  (predicate (no-dups (sort < `(,n1 ,n2 ,n3 ,n4 ,n5 ,n6 ,n7 ,n8))))
	  (predicate (> n1 2))
	  (any
	    (predicate (= n2 (+ n1 1)))
	    (predicate (= n2 (- n1 1))))
	  (predicate (> n3 5))
	  (any
	    (predicate (= n4 (+ n3 2)))
	    (predicate (= n4 (- n3 2))))
	  (predicate (> n5 n4))
	  (any
	    (predicate (= n6 (+ n5 3)))
	    (predicate (= n6 (- n5 3))))
	  (predicate (< n7 4))
	  (any
	    (predicate (= n8 (+ n7 4)))
	    (predicate (= n8 (- n7 4)))))))))

(define houses-test
  (lambda ()
    (solution (n1 n2 n3 n4 n5 n6 n7 n8)
      (mapping n1 n2 n3 n4 n5 n6 n7 n8))))
	  
	  
      
    
