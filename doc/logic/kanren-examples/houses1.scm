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
  (relation (head-let name address)
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
      ((null? ls) #t)
      ((null? (cdr ls)) #t)
      ((= (car ls) (cadr ls)) #f)
      (else (no-dups (cdr ls))))))
	    
(define mapping
  (relation (n1 n2 n3 n4 n5 n6 n7 n8)
    (to-show n1 n2 n3 n4 n5 n6 n7 n8)
    (all
      (pos 'allison n1)
      (project (n1)
	(all
	  (predicate (> n1 2))
	  (pos 'adrienne n2)
	  (project (n2)
	    (all
	      (any
	        (predicate (= n2 (+ n1 1)))
	        (predicate (= n2 (- n1 1))))
	      (pos 'belinda n3)
	      (project (n3)
		(all
		  (predicate (> n3 5))
		  (pos 'benito n4)
		  (project (n4)
		    (all
		      (any
		        (predicate (= n4 (+ n3 2)))
		        (predicate (= n4 (- n3 2))))
		      (pos 'cheri n5)
		      (project (n5)
			(all
			  (predicate (> n5 n4))
		          (pos 'crawford n6)
			  (project (n6)
			    (all
			      (any
                		(predicate (= n6 (+ n5 3)))
		                (predicate (= n6 (- n5 3))))
	                      (pos 'daryl n7)
			      (project (n7)
				(all
			          (predicate (< n7 4))
				  (pos 'don n8)
				  (project (n8)
				    (all
			              (any
             		                (predicate (= n8 (+ n7 4)))
		                        (predicate (= n8 (- n7 4))))
				      (predicate
		                        (no-dups
					  (sort <
					    `(,n1 ,n2 ,n3 ,n4 ,n5 ,n6 ,n7 ,n8)))))))))))))))))))))))

(define houses-test
  (lambda ()
    (solution (n1 n2 n3 n4 n5 n6 n7 n8)
      (mapping n1 n2 n3 n4 n5 n6 n7 n8))))

(define benchmark_count 1000)

(display "Timing per iterations: ") (display benchmark_count) (newline)
(time (do ((i 0 (+ 1 i))) ((>= i benchmark_count))
          (houses-test)))
