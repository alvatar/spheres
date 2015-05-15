;;(use gochan test srfi-1 srfi-13)

(test-group
 "simple chan"
 (define c (make-chan))
 (chan-send c 'one)
 (test "synchronous recv" 'one (chan-receive c))
 (chan-send c 'two)
 (chan-send c 'three)
 (chan-close c)
 (test "closed?" #t (chan-closed? c))
 (test-error "send to closed chan fails" (chan-send c 'three))

 (test "closed channel keeps buffer" 'two        (chan-receive c))
 (test "chan-receive*"             'three (car (chan-receive* c #f)))

 (test-error "errors on receiving from closed and empty chan" (chan-receive c) )

 (test "chan-receive* #f when closed" #f (chan-receive* c #f))
 (test "on-closed unlocks mutex"        #f (chan-receive* c #f))

 )

(test-group
 "multiple receivers - no blocking"

 (define channel (make-chan))
 (define result (make-chan))
 (define (process) (chan-send result (chan-receive channel)))
 (define t1 (thread-start! (make-thread process "tst1")))
 (define t2 (thread-start! (make-thread process "tst2")))

 (thread-yield!) ;; make both t1 and t2 wait on condvar

 (begin
   (chan-send channel "1")
   (chan-send channel "2"))

 (test "1" (chan-receive result))
 (test "2" (chan-receive result)))


(test-group
 "multiple channels"

 (define c1 (make-chan))
 (define c2 (make-chan))
 (chan-send c1 'c1)
 (chan-send c2 'c2)

 (test "nonempty first"  'c1 (chan-receive (list c1 c2)))
 (test "nonempty second" 'c2 (chan-receive (list c1 c2)))

 (chan-send c2 'c2-again)
 (chan-close c1)
 (chan-close c2)

 (test "close first" 'c2-again (chan-receive (list c1 c2)))
 (test "both closed" #f (chan-receive* (list c1 c2) #f))
 )

(test-group
 "empty multiple channels"
 (define c1 (make-chan))
 (define c2 (make-chan))
 (define (process) (chan-fold (list c1 c2) (lambda (x s) (thread-yield!) (+ x s)) 0))
 (define workers (map thread-start! (make-list 4 process)))

 ;; a couple of challenges
 (thread-yield!) (for-each (cut chan-send c1 <>) (iota 10 1000))
 (thread-yield!) (for-each (cut chan-send c2 <>) (iota 10 100))
 (thread-yield!)

 (chan-close c1)
 (chan-close c2)

 (let ((worker-sums (map thread-join! workers)))
   (test "fair worker distribution" #t (every (cut < 1000 <>) worker-sums))
   (test "multiple empty channels with multiple workers"
         (+ 10000 9 8 7 6 5 4 3 2 1
            1000  9 8 7 6 5 4 3 2 1)
         (fold + 0 worker-sums)))
 )

(test-group
 "chan-for-each"

 (define c (make-chan))
 (chan-send c "a")
 (chan-send c "b")
 (chan-send c "c")
 (chan-close c)

 (test "simple for-each"
       "abc"
       (with-output-to-string (lambda () (chan-for-each c display)))))

(test-group
 "chan-fold"
  (define c (make-chan))
  (for-each (cut chan-send c <>) (iota 101))
  (chan-close c)
  (test "chan-fold sum" 5050 (chan-fold c (lambda (msg state) (+ msg state)) 0)))

(test-group
 "chan-select"
 (define c1 (make-chan))
 (define c2 (make-chan))
 (chan-send c2 2)
 (chan-send c1 1)
 (chan-send c1 1)
 (chan-close c1)
 (chan-close c2)
 (define (next)
   (chan-select
    (c1 msg (list "c1" msg))
    (c2 obj (list "c2" obj))))

 (test "chan-select 1" '("c1" 1) (next))
 (test "chan-select 2" '("c1" 1) (next))
 (test "chan-select 3" '("c2" 2) (next))
 )

(test-group
 "timeouts"

 (test "immediate timeout" #t (chan-receive* (make-chan) 0))
 (test-error "timeout error" (chan-receive (make-chan) 0))

 (define c (make-chan))
 (define workers (map thread-start! (make-list 4 (lambda () (chan-receive* c 0.1)))))

 (test "simultaneous timeouts"
       '(#t #t #t #t)
       (map thread-join! workers))


 (test 'timeout
       (chan-select
        (c msg (error "from c1" msg))
        (0.1 'timeout)))


 (define cclosed (make-chan))
 (chan-close cclosed)
 (test-error
  "no timeout, closed channel still produces error"
  (chan-select
   (cclosed msg 'msg)
   (0 'to)))
 )

(test-exit)




;;;;;;;;;;;;;;;;;;;;;


;; worker-pools example, based on https://gobyexample.com/worker-pools
;; time csi -s worker-pool.scm tells us we spend 1 second doing a
;; 5-second job.
(use gochan miscmacros srfi-1 test)

(define (worker jobs results)
  (gochan-for-each jobs
                   (lambda (job)
                     (print (current-thread) " processing job " job)
                     (thread-sleep! 1)
                     (gochan-send results (* -1 job))))
  (print (current-thread) " worker exit"))

(define jobs (make-gochan))
(define res (make-gochan))

(define workers
  (map
   (lambda (x) (thread-start! (make-thread (cut worker jobs res) x)))
   (iota 10))) ;; <-- 10 worker threads

;; 5 jobs
(repeat* 5 (gochan-send jobs it))
(gochan-close jobs) ;; this will exit workers when channel is drained
(repeat* 5 (print "result: " (gochan-receive res)))

(for-each thread-join! workers)
