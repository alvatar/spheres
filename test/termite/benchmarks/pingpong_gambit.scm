(define (iota n)
  (if (= n 0)
      '()
      (cons n (iota (- n 1)))))

(define (ping-pong-player)
  (let loop ((n 0))
    (let ((msg (thread-receive)))
      (let ((from (car msg))
            (ball (cdr msg)))
        (if (eq? ball 'done)
            (thread-send from n)
            (begin
              (thread-send from (cons (current-thread) ball))
              (loop (+ n 1))))))))

(define player1 (thread-start! (make-thread ping-pong-player)))
(define player2 (thread-start! (make-thread ping-pong-player)))

(define (bench duration len)
  (thread-send player1 (cons player2 (iota len)))
  (thread-receive duration 'ok)                      ; pause
  (thread-send player1 (cons (current-thread) 'done))
  (thread-send player2 (cons (current-thread) 'done))
  (thread-receive))

(define (main #!optional (len "42"))
  (let ((len (string->number len)))
    (let ((duration 5))
      (write `(pingpong
               gambit
               ,len
               ,(round (/ (bench duration len) duration))))
      (newline)))
  (thread-receive 1 'done))
