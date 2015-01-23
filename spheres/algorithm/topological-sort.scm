;;! Topological sort
;; .author Mikael Djurfeldt, Copyright (C) 1995
;; .author Alvaro Castro-Castilla, 2015

;;! Sort the directed acyclic graph so that for the order of each adjancency list
;; is respected in the resulting list of vertices
;; 
;; .parameter dag is a list of sublists.
;; .parameter pred equality test
;; The car of each sublist is a vertex. The cdr is the adjacency list of that
;; vertex, i.e. a list of all vertices to which there exists an edge from the
;; car vertex.
;; The algorithm is inspired by Cormen, Leiserson and Rivest (1990)
;; Introduction to Algorithms, chapter 23.
;; Time complexity: O (|V| + |E|)
;; Example (from Cormen):
;; Prof. Bumstead topologically sorts his clothing when getting
;; dressed.  The first argument to @0 describes which
;; garments he needs to put on before others.  (For example,
;; Prof Bumstead needs to put on his shirt before he puts on his
;; tie or his belt.)  @0 gives the correct order of dressing:
;; (topological-sort
;;  '((shirt tie belt)
;;    (tie jacket)
;;    (belt jacket)
;;    (watch)
;;    (pants shoes belt)
;;    (undershorts pants shoes)
;;    (socks shoes))
;;    eq?)
;; (socks undershorts pants shoes watch shirt belt tie jacket)
(define (topological-sort dag pred)
  (if (null? dag)
      '()
      (let ((adj-table (make-table
                        size: (car (primes> (length dag) 1))
                        test: pred))
            (sorted '()))
	(letrec ((visit
		  (lambda (u adj-list)
		    ;; Color vertex u
		    (table-set! adj-table u 'colored)
		    ;; Visit uncolored vertices which u connects to
		    (for-each (lambda (v)
				(let ((val (table-ref adj-table v #f)))
				  (if (not (eq? val 'colored))
				      (visit v (or val '())))))
			      adj-list)
		    ;; Since all vertices downstream u are visited
		    ;; by now, we can safely put u on the output list
		    (set! sorted (cons u sorted)))))
	  ;; Hash adjacency lists
	  (for-each (lambda (def)
		      (table-set! adj-table (car def) (cdr def)))
		    (cdr dag))
	  ;; Visit vertices
	  (visit (caar dag) (cdar dag))
	  (for-each (lambda (def)
		      (let ((val (table-ref adj-table (car def) #f)))
			(if (not (eq? val 'colored))
			    (visit (car def) (cdr def)))))
		    (cdr dag)))
	sorted)))
