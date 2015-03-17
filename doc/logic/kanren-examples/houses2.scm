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

(define mapping
  (relation (head-let n1 n2 n3 n4 n5 n6 n7 n8)
    (let ([nlist '()])
      (all!
	(pump n1)
	(project (n1)
	  (all!
	    (inject (> n1 2))
	    (pump n2)
	    (project (n2)
	      (let ([nlist (cons n1 nlist)])
		(all!
		  (inject (not (memv n2 nlist)))
		  (any
		    (inject (= n2 (+ n1 1)))
		    (inject (= n2 (- n1 1))))
		  (pump n3)
		  (project (n3)
		    (let ([nlist (cons n2 nlist)])		
		      (all!
			(inject (not (memv n3 nlist)))
			(inject (> n3 5))
			(pump n4)
			(project (n4)
			  (let ([nlist (cons n3 nlist)])
			    (all!
			      (inject (not (memv n4 nlist)))
			      (any
				(inject (= n4 (+ n3 2)))
				(inject (= n4 (- n3 2))))
			      (pump n5)
			      (project (n5)
				(let ([nlist (cons n4 nlist)])
				  (all!
				    (inject (not (memv n5 nlist)))
				    (inject (> n5 n4))
				    (pump n6)
				    (project (n6)
				      (let ([nlist (cons n5 nlist)])
					(all!
					  (inject (not (memv n6 nlist)))
					  (any
					    (inject (= n6 (+ n5 3)))
					    (inject (= n6 (- n5 3))))
					  (pump n7)
					  (project (n7)
					    (let ([nlist (cons n6 nlist)])
					      (all!
						(inject (not (memv n7 nlist)))
						(inject (< n7 4))
						(pump n8)
						(project (n8)
						  (let ([nlist (cons n7 nlist)])
						    (all!!
						      (inject (not (memv n8 nlist)))
						      (any
							(inject (= n8 (+ n7 4)))
							(inject (= n8 (- n7 4))))
						      )))))))))))))))))))))))))))

(define houses-test
  (lambda ()
    (solution (allison adrienne belinda benito cheri crawford daryl don)
      (mapping allison adrienne belinda benito cheri crawford daryl don))))

(define benchmark_count 1000)

(display "Timing per iterations: ") (display benchmark_count) (newline)
(time (do ((i 0 (+ 1 i))) ((>= i benchmark_count))
          (houses-test)))
