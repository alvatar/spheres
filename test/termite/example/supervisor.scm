;; This supervisor is intended to be a transparent wrapper for a
;; process.

;; The code of the process must be in a thunk.

;; The only interface requirement for the supervised process is to
;; answer to the message 'shutdown

(define (take lst n)
  (cond
   ((< n 1) '())
   (else
    (cons (car lst)
          (take (cdr lst)
                (- n 1))))))

(define (spawn-supervised thunk 
                          #!key
                          (threshold        5)
                          (window        1000)
                          (shutdown-delay 0.5)
                          (init-hook (lambda (pid) 'ok)))
  (spawn
    (lambda ()
      (let loop ((failures '()))
        (let ((pid (spawn-link thunk)))
          (with-exception-catcher
           (lambda (exception)
             (match (exception-object exception)
               (('exit ,pid reason . _)
                (match reason
                  ('failure 
                   (info "process failed")
                    
                   (if (>= (length failures) threshold)
                       (if (< (- (now) 
                                 (list-ref failures 
                                           (- threshold 1)))
                              window)
                           (begin
                             (print 
                              `(info: not restarting (too many failures)))
                             (print 
                              `(info: halting supervisor))
                             (halt!))
                           (loop (cons (now) 
                                       (take failures 
                                             (- threshold 1)))))
                       (begin
                         (print `(info: restarting...))
                         (loop (cons (now) failures)))))
                   
                  ('normal 
                   (info "process is done executing")
                   (info "halting supervisor")
                   (halt!))
                   
                  ('terminated
                   (info "had to terminate the process")
                   (info "halting supervisor")
                   (halt!))))

               ;; the exception doesn't concern us, relay it to the
               ;; process
               (_
                (! pid exception)
                (loop failures))))

           (lambda ()
             (info "starting up supervised process")

             ;; if an exception is signaled during the execution of
             ;; init-hook, signal it to the process
             (with-exception-catcher
              (lambda (e) (! pid e))
              (lambda () (init-hook pid)))
          
             (let loop ()
               (recv
                 ('shutdown (shutdown pid shutdown-delay))
                 (msg (! pid msg)))
               (loop)))))))))


(define (shutdown pid shutdown-delay)
  (match shutdown-delay
    (#f (terminate! pid))
    (#t (! pid 'shutdown))
    (n (where (number? n))
       (! pid 'shutdown)
       (recv
         (after shutdown-delay 
           (terminate! pid)
           (raise (list 'exit pid 'terminated)))))))


(define (server-start-supervised plugin . args)
  (spawn-supervised
   (lambda ()
     (server plugin))
   init-hook: (lambda (pid)
                (!? pid (cons 'init args) *server-timeout*))))

;; (define p
;;   (spawn-supervised 
;;    (lambda ()
;;      (let loop ()
;;        (recv
;;          ('crash (/ 1 0))
;;          ('shutdown (halt!))
;;          (msg 
;;           (print (list msg: msg))
;;           (loop)))))
;;    ))
;; (! p 'foo)
;; 
;; (##repl)
