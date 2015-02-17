(declare (block))

(define (iota n)
  (if (= n 0)
      '()
      (cons n (iota (- n 1)))))

(define (ping-pong-player)
  (let loop ((n 0))
    (let ((msg (?)))
      (let ((from (car msg))
            (ball (cdr msg)))
        (if (eq? ball 'done)
            (! from n)
            (begin
              (! from (cons (self) ball))
              (loop (+ n 1))))))))


(define (run player1 player2 duration len)
  (! player1 (cons player2 (iota len)))
  (? duration 'ok) ;; pause
  
  (! player1 (cons (self) 'done))
  (! player2 (cons (self) 'done))
  (?))

(define (pingpong duration len)
  (write `(pingpong
           termite
           ,len
           ,(round (/ (run
                       (spawn ping-pong-player)
                       (remote-spawn node2 ping-pong-player)
                       duration
                       len)
                      duration))))
  (newline))

(define (main #!optional (len 42))
  (cond
   ((equal? (current-node) node1)
    ;; code for node 1
    (let ((duration 5))
      (pingpong duration len)
      (remote-spawn node2 (lambda () (exit)))
      (? 1 'done)))

   ;; code for node2
   (else
    (?))))
