(define (main #!optional (len "42"))
  (cond
   ((equal? (current-node) node1)
    ;; code for node 1
    (let ((len (string->number len))
          (duration 5))
      (pingpong duration len)
      (remote-spawn node2 (lambda () (exit)))
      (? 1 'done)))

   ;; code for node2
   (else
    (write (?))
    (newline))))
