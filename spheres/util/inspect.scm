;;!!! Inspection and debugging utilities

;;! Inspect a thread's continuation backtrace and its environment
(define* (%inspect-thread (thread (current-thread))
                          (display-environment? #t)
                          (max-head #f)
                          (max-tail #f))
  (let ((thread-type (##structure-type thread)))
    ;; Well, this pokes into Gambit's internals, so it may end up blowing up
    ;; macro-less version of: http://gambitscheme.org/wiki/index.php/Internal_Documentation
    (define (thread-end-condvar obj)
      ((let () (##declare (extended-bindings)) ##structure-ref) obj 16 thread-type #f))
    (define (thread-exception? obj)
      ((let () (##declare (extended-bindings)) ##structure-ref) obj 17 thread-type #f))
    (define (thread-result obj)
      ((let () (##declare (extended-bindings)) ##structure-ref) obj 18 thread-type #f))
    (let* ((end-condvar (thread-end-condvar thread))
           (exception? (thread-exception? thread))
           (result (thread-result thread)))
      (cond ((not end-condvar)
             ;; thread has terminated
             (if exception?
                 (begin
                   ;; thread has terminated with exception
                   (display "Terminated with exception:\n")
                   (display-exception result))
                 (begin
                   ;; thread has terminated with result
                   (display "Terminated with result:\n")
                   (pretty-print result))))
            (exception?
             ;; thread has never run and is not terminated
             (if (not result)
                 (begin
                   ;; thread is not yet started
                   (display "Not yet started\n"))
                 (begin
                   ;; thread is started but has not yet run
                   (display "Started but has not yet run\n"))))
            (else
             (let ((c (##thread-continuation-capture thread))) ; See note above
               (cond ((and max-head max-tail)
                      (display-continuation-backtrace c (current-output-port) #f display-environment? max-head max-tail))
                     (max-head
                      (display-continuation-backtrace c (current-output-port) #f display-environment? max-head))
                     (else
                      (display-continuation-backtrace c (current-output-port) #f display-environment?)))))))))
