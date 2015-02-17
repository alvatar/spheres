(%load-library '(spheres/concurrency termite))

(define node1 (make-node "localhost" 3001))
(define node2 (make-node "localhost" 3002))
(node-init node2)
