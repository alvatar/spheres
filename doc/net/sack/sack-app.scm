(import server
        show-exceptions
        pool-session-store
        cookie
        (std misc/exception))

(sack-start!
 (lambda (env)
   (let ((ret #t))
     (values 200
             '(("Content-Type" . "text/plain"))
             (lambda ()
               (and
                ret
                (begin
                  (set! ret #f)
                  (with-output-to-u8vector
                   (list char-encoding: 'UTF-8)
                   (lambda ()
                     (display "Hello, world!")))))))))
 port-number: 3333)






(define (sack-app env)
  (error "hej"))

(define app
  (let ((pool (make-session-pool)))
    (lambda (env)
      ((show-exceptions
        (cookies->
         (pool-session-store
          sack-app
          pool: pool)))
       env))))

(define quit
  (sack-start! (lambda (env) (app env))
               port-number: 3333))








(load "../../github-modules/build")

(import server
        show-exceptions
        pool-session-store
        cookie
        (std misc/exception))

(import "http://github.com/pereckerdal/sack/raw/master/src/server.scm"
        "http://github.com/pereckerdal/sack/raw/master/src/show-exceptions.scm"
        "http://github.com/pereckerdal/sack/raw/master/src/pool-session-store.scm"
        "http://github.com/pereckerdal/sack/raw/master/src/cookie.scm"
        (std misc/exception))

(define (sack-app env)
  (pp (list sess: ((env 'sack:session:get-all))))
  ((env 'sack:session:set!) "hej" "du")
  
  (let ((ret #t))
    (values 200
            '(("Content-Type" . "text/plain; charset=UTF-8"))
            (lambda ()
              (and
               ret
               (begin
                 (set! ret #f)
                 (with-output-to-u8vector
                  (list char-encoding: 'UTF-8)
                  (lambda ()
                    (display "hej")))))))))

(define app
  (let ((pool (make-session-pool)))
    (lambda (env)
      ((show-exceptions
        (cookies->
         (pool-session-store
          sack-app
          pool: pool)))
       env))))

(thread-start!
 (make-thread
  (lambda ()
    (sack-start!
     (lambda (env)
       (app env))
     port-number: 3333))))



(lambda (env)
  (values 200
          '(("Content-Type" . "text/plain; charset=UTF-8"))
          (lambda ()
            (display "hej")
            #f)))

the anatomy of a spork request:
* the spork sack app gets called.
* we find out where to enter (a previous continuation id or a fresh spork), and that frame (the previous continuation's frame or empty in the case of a fresh spork)
* the continuation is captured. (this continuation will not be serialized)
* a new thread is created, with the continuation, the sack environment and the current frame as thread data. This frame starts executing our entrance point. The thread is created to make sure that, regardless of the sack stack/dynamic environment, it's possible to serialize continuations from this thread. It will also reduce unnecessary stuff from the continuation data.
* the thread of the sack request terminates itself, making sure not to return.
* the newly created thread will sooner or later decide to show a response and will then invoke the continuation that was previously captured.


saved in thread-specific storage:
* sack continuation.
* sack environment.
* current frame. ;; to implement bt vars

render-widget-cont ;; to implement fork
run-before-show-redirect ;; to implement ajax-fork

order of implementation:
bt vars
fork
ajax-fork







(begin
  (define (wr obj)
    (call-with-output-string
     '()
     (lambda (p)
       (output-port-readtable-set!
        p
        (readtable-sharing-allowed?-set
         (output-port-readtable p)
         'serialize))
       (write obj p))))
  
  (define (rd str)
    (call-with-input-string
     str
     (lambda (p)
       (input-port-readtable-set!
        p
        (readtable-sharing-allowed?-set
         (input-port-readtable p)
         'serialize))
       (read p))))
  
  (define fifo (open-vector))

  (define (suspend-and-die!)
    (call/cc
     (lambda (k)
       (write (wr k) fifo)
       (newline fifo)
       (force-output fifo)
       (thread-terminate! (current-thread)))))
  
  (let ((dummy-port (open-string)))
    (parameterize ((current-input-port dummy-port)
                   (current-output-port dummy-port))
      (thread-start!
       (make-thread
        (lambda ()
          (* 100
             (suspend-and-die!)))))))

  (define s (read fifo))
  (thread-join!
   (thread-start!
    (make-thread
     (lambda ()
       ((rd s) 111)))))
  
  (thread-join!
   (thread-start!
    (make-thread
     (lambda ()
       ((rd s) 222)))))
  
  (string-length s))

(u8vector-length
 (object->u8vector (open-string)
                   (lambda (x)
                     (cond
                      ((mutex? x)
                       (write x) #f)
                      ((condition-variable? x)
                       (write x) #f)
                      (else
                       x)))))

(##parameterize
 ((current-output-port #f))
 (print "hej\n"))

(define fifo (open-vector))

(thread-start!
 (##make-root-thread
  (lambda ()
    (let ((blah (string-append
                 "hej"
                 "san"
                 (number->string
                  (time->seconds (current-time)))))
          (blah2 (string-append
                 "hej"
                 "san"
                 (number->string
                  (time->seconds (current-time)))))
          (blah3 (string-append
                 "hej"
                 "san"
                 (number->string
                  (time->seconds (current-time))))))
      (call/cc
       (lambda (k)
         (write (object->u8vector k) fifo)))
      (display blah)
      (display blah2)
      (display blah3)))
  "hej"
  (thread-thread-group (current-thread))
  #f
  #f))

(u8vector-length (read fifo))

((u8vector->object (read fifo)))



(lambda (env)
  (values 200
          '(("Content-Type" . "text/plain; charset=UTF-8"))
          (lambda ()
            (display "Hello, world!")
            #f)))

