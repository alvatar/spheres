;; Functionality to embed remote debugging capabilities in a program



;;;-----------------------------------------------------------------------------

(define rdi #f)

(define (make-rdi-host host)
  (set! rdi (rdi-create-client host)))

(define (debuggee-rdi-function fn)
  (case fn
    ((console-input)
     rdi-console-input)
    (else
     (error "unknown function"))))

;;;-----------------------------------------------------------------------------

(define rdi-console-table (make-table))

(define (rdi-console-input console-id input)
  (let ((remote-port
         (table-ref rdi-console-table console-id #f)))
    (if remote-port
        (begin
          (display input remote-port)
          (force-output remote-port))))
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

(define (rdi-console-output-pump-start! console-id remote-port)
  (thread-start!
   (make-thread
    (lambda ()
      (let* ((buflen 1000)
             (buf (make-string buflen)))
        (let loop ()
          (let ((n (read-substring-blocking-for-1 buf 0 buflen remote-port)))
            (if (> n 0)
                (begin
                  (rdi-remote-call rdi
                                   'console-output
                                   console-id
                                   (substring buf 0 n))
                  (loop))))))))))

(define (rdi-register-console thread remote-port)
  (let ((console-id (object->serial-number thread)))
    (table-set! rdi-console-table console-id remote-port)
    (rdi-remote-call rdi 'register-console console-id)
    (rdi-console-output-pump-start! console-id remote-port)))

(define (make-repl-channel-remote-port thread)
  (receive (local-port remote-port) (open-string-pipe)
    (begin

      ;; Hack... set the name of the port to pretend it is the "console"
      (##vector-set! local-port 4 (lambda (port) '(console)))

      (rdi-register-console thread remote-port)
      local-port)))

(define open-dummy-console? #t)

(define (thread-make-repl-channel-remote thread)
  (with-exception-catcher
   (lambda (e)
     (let ((i (open-input-string ""))
           (o (open-output-string)))
       (##make-repl-channel-ports i o)))
   (lambda ()
     (let ((local-port (make-repl-channel-remote-port thread)))
       (##make-repl-channel-ports local-port local-port)))))



;;;-----------------------------------------------------------------------------

;;! Installs a remote REPL
;; .parameter ip Ip number of the debug server
;; .parameters options
(define* (remote-repl-setup! ip (port: 20000))
  (let ((address (string-append ip ":" (number->string port))))
    (and (with-exception-catcher
          (lambda (e) #f)
          (lambda () (let ((p (open-tcp-client address)))
                 (write '() p)
                 (force-output p))))
         (begin
           (rdi-set-rdi-function! debuggee-rdi-function)
           (set! ##thread-make-repl-channel
                 thread-make-repl-channel-remote)
           (make-rdi-host address)
           'connected))))

;;! Runs the installed REPL immediately. It must be installed first.
(define (remote-repl-run!)
  (thread-start!
   (make-thread
    (lambda () (##repl-debug-main)))))
