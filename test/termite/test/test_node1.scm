(%load-library '(spheres/concurrency termite))

(define node1 (make-node "localhost" 3001))
(define node2 (make-node "localhost" 3002))
(node-init node1)

(begin '((%load-library '(spheres/concurrency termite))

         (define node1 (make-node "localhost" 3001))
         (define node2 (make-node "localhost" 3002))
         (node-init node2)))

;;(debug (current-node))
(on node2 (lambda () (write 'success!) (newline)))

(define pid 
  (spawn 
	(lambda () 
	  (migrate-task node2)
	  (termite-info 'migration-ok!))))

