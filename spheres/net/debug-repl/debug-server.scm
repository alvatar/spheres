#!/usr/bin/env gsi-script
;; Sense (debug-server) used in conjuction with remote debugging functionality

(include "rdi.scm")

;;;----------------------------------------------------------------------------

(define console-window-num 0)

(define (new-console-window-num)
  (let ((x (+ console-window-num 1)))
    (set! console-window-num x)
    x))

(define *within-emacs* #f)

(define (open-console-window console-id)
  (let ((tcp-port (+ 9000 (new-console-window-num))))
    (if *within-emacs*
        (begin (display "EMACS-EVAL: ")
               (pp `(sense-open-client ,console-id ,tcp-port)))
        (open-process
         (list path: "xterm"
               arguments: (list "-e"
                                "gsi"
                                "~~spheres/energy/src/remote/pump.scm"
                                (number->string tcp-port)))))
    (let loop ()
      (let ((port
             (with-exception-catcher
              (lambda (e)
                #f)
              (lambda ()
                (let ((port (open-tcp-client
                             (list server-address: "localhost"
                                   port-number: tcp-port))))
                  (tcp-client-peer-socket-info port)
                  port)))))
        (if (not port)
            (begin
              (thread-sleep! .1) ;; wait until the pump starts
              (loop))
            port)))))

;;;-----------------------------------------------------------------------------

(define rdi #f)

(define (debug-server-rdi-function fn)
  (case fn
    ((register-console)
     rdi-register-console)
    ((console-output)
     rdi-console-output)
    (else
     (error "unknown function"))))

;;;-----------------------------------------------------------------------------

(define rdi-console-table (make-table))

(define (rdi-register-console console-id)
  (let ((console-port (open-console-window console-id)))
    (table-set! rdi-console-table console-id console-port)
    (rdi-console-input-pump-start! console-id console-port)
    #f))

(define (rdi-console-output console-id output)
  (let ((console-port
         (table-ref rdi-console-table console-id #f)))
    (if console-port
        (begin
          (display output console-port)
          (force-output console-port))))
  #t)

(define (read-substring-blocking-for-1 str start end port)
  (if (< start end)
      (begin
        (input-port-timeout-set! port +inf.0) ;; block for the first byte
        (let ((n (read-substring str start (+ start 1) port)))
          (input-port-timeout-set! port -inf.0) ;; don't block for the rest
          (if (= n 1)
              (+ 1 (read-substring str (+ start 1) end port))
              n)))
      0))

(define (rdi-console-input-pump-start! console-id console-port)
  (thread-start!
   (make-thread
    (lambda ()
      (let* ((buflen 1000)
             (buf (make-string buflen)))
        (let loop ()
          (let ((n (read-substring-blocking-for-1 buf 0 buflen console-port)))
            (if (> n 0)
                (begin
                  (rdi-remote-call rdi
                                   'console-input
                                   console-id
                                   (substring buf 0 n))
                  (loop))))))))))

;;! Main
(##define (main #!optional (port #f) (emacs? #f))
  (if (and emacs? (string=? emacs? "emacs"))
      (begin (println "Running Emacs remote Gambit debugging")
             (set! *within-emacs* #t)))
  (println "To close this server, kill the 'gsi' process.")
  (println "Listening on port " (or port "20000"))
  (rdi-set-rdi-function! debug-server-rdi-function)
  (set! rdi (rdi-create-server (and port (string->number port))))
  (rdi-force-connection rdi))

;; (let ((args (cdr (command-line))))
;;   (if (null? args)
;;       (main)
;;       (if (null? (cdr args))
;;           (main args)
;;           (err "Too many arguments. Use: 'sense <port>'"))))
