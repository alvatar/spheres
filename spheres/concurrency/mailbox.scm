;;!!! In-RAM Mailbox object
;;
;; .author Marc Feeley, 1994-2009
;; This code is from Gambit's documentation.

;; an implementation of a mailbox object of depth one; this
;; implementation does not behave well in the presence of forced
;; thread terminations using thread-terminate! (deadlock can occur
;; if a thread is terminated in the middle of a put! or get! operation)

(define (make-empty-mailbox)
  (let ((mutex (make-mutex))
        (put-condvar (make-condition-variable))
        (get-condvar (make-condition-variable))
        (full? #f)
        (cell #f))
    (define (put! obj)
      (mutex-lock! mutex)
      (if full?
          (begin
            (mutex-unlock! mutex put-condvar)
            (put! obj))
          (begin
            (set! cell obj)
            (set! full? #t)
            (condition-variable-signal! get-condvar)
            (mutex-unlock! mutex))))
    (define (get!)
      (mutex-lock! mutex)
      (if (not full?)
          (begin
            (mutex-unlock! mutex get-condvar)
            (get!))
          (let ((result cell))
            (set! cell #f)              ; avoid space leaks
            (set! full? #f)
            (condition-variable-signal! put-condvar)
            (mutex-unlock! mutex)
            result)))
    (lambda (msg)
      (case msg
        ((put!) put!)
        ((get!) get!)
        (else (error "unknown message"))))))

(define (mailbox-put! m obj) ((m 'put!) obj))

(define (mailbox-get! m) ((m 'get!)))
