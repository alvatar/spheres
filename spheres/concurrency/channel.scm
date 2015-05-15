;;!!! thread-safe channel (FIFO), inspired by channels from Go
;;
;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; Copyright (c) 2014 Kristian Lein-Mathisen.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-record-type chan
  (%make-chan mutex semaphores front rear closed?)
  chan?
  (mutex chan-mutex chan-mutex-set!)
  (semaphores chan-semaphores chan-semaphores-set!)
  (front chan-front chan-front-set!)
  (rear chan-rear chan-rear-set!)
  (closed? chan-closed? chan-closed-set!))

(define (semaphore-close! cv) (condition-variable-specific-set! cv #t))
(define (semaphore-open! cv)  (condition-variable-specific-set! cv #f))
(define (semaphore-open? cv)  (eq? #f (condition-variable-specific cv)))

;;! make a binary open/closed semaphore. default state is open.
;; actually a condition-variable which can be signalled. we need the
;; state a semaphore provides for guarantees of the sender-end
;; "awaking" the receiving end.
(define (make-semaphore name)
  (let ((cv (make-condition-variable name)))
    (semaphore-open! cv)
    cv))

;;! returns #t on successful signal, #f if semaphore was already
;; closed.
(define (semaphore-signal! semaphore)
  (cond ((semaphore-open? semaphore)
         (semaphore-close! semaphore)
         (condition-variable-signal! semaphore) ;; triggers receiver
         #t)
        (else #f))) ;; already signalled

;;! wait for semaphore to close. #f means timeout, #t otherwise.
(define (semaphore-wait! semaphore timeout)
  (cond ((semaphore-open? semaphore)
         (mutex-unlock! (make-mutex) semaphore timeout)) ;; <-- #f on timeout
        (else #t))) ;; already signalled

(define (make-chan)
  (%make-chan (make-mutex) ;; mutex
              '()          ;; condition variables
              '()          ;; front
              '()          ;; rear
              #f))         ;; not closed

(define (chan-empty? chan)
  (null? (chan-front chan)))

(define (chan-send chan obj)
  (mutex-lock! (chan-mutex chan))
  (if (chan-closed? chan)
      (begin (mutex-unlock! (chan-mutex chan))
             (error "chan closed" chan)))
  (let ((new (list obj))
        (rear (chan-rear chan)))
    (chan-rear-set! chan new)
    (cond
     ((pair? rear)
      (set-cdr! rear new))
     (else ;; sending to empty chan
      (chan-front-set! chan new)))

    ;; signal anyone who has registered
    (let loop ((semaphores (chan-semaphores chan)))
      (cond ((pair? semaphores)
             (if (semaphore-signal! (car semaphores))
                 ;; signalled! remove from semaphore list
                 (chan-semaphores-set! chan (cdr semaphores))
                 ;; not signalled, ignore and remove (someone else
                 ;; signalled)
                 (loop (cdr semaphores))))))) ;; next in line

  (mutex-unlock! (chan-mutex chan)))

;; returns:
;; #f if channel closed
;; #t if registered with semaphore
;; (cons msg (cdr chan) if chan is pair (userdata)
;; (cons msg '()) if chain is chan (no userdata)
;; userdata is useful for finding which channel a message came from.
(define (chan-receive** chan% semaphore)
  (let ((chan (if (pair? chan%) (car chan%) chan%)))
    (mutex-lock! (chan-mutex chan))
    (let ((front (chan-front chan)))
      (cond
       ((null? front) ;; receiving from empty chan
        (cond ((chan-closed? chan)
               (mutex-unlock! (chan-mutex chan))
               #f) ;; #f for fail
              (else
               ;; register semaphore with channel
               (chan-semaphores-set! chan (cons semaphore (chan-semaphores chan)))
               (mutex-unlock! (chan-mutex chan))
               #t)))
       (else
        (chan-front-set! chan (cdr front))
        (if (null? (cdr front))
            (chan-rear-set! chan '()))
        (mutex-unlock! (chan-mutex chan))
        ;; return associated userdata if present:
        (cons (car front) (if (pair? chan%) (cdr chan%) '())))))))

;; accept channel or list of channels. returns:
;; (list msg channel) on success,
;; #f for all channels closed
;; #t for timeout
(define (chan-receive* chans% timeout)
  (let ((chans (if (pair? chans%) chans% (list chans%)))
        (semaphore (make-semaphore (current-thread))))

    (let loop ((chans chans)
               (never-used? #t)) ;; anybody registered our semaphore?
      (if (pair? chans)
          (let* ((chan (car chans))
                 (msgchan (chan-receive** chan semaphore)))
            (cond ((pair? msgchan) msgchan)
                  ;; channel registered with semaphore
                  ((eq? #t msgchan)
                   (loop (cdr chans) #f))
                  ;; channel was closed!
                  (else (loop (cdr chans) never-used?))))
          ;; we've run through all channels without a message.
          (cond (never-used? #f) ;; all channels were closed
                (else ;; we have registered our semaphore with all channels
                 (if (semaphore-wait! semaphore timeout)
                     (chan-receive* chans% timeout)
                     #t))))))) ;;<-- timeout

(define (chan-receive chan #!optional timeout)
  (let ((msg* (chan-receive* chan timeout)))
    (cond ((eq? msg* #t) (error "timeout" chan))
          (msg* => car) ;; normal msgpair
          (else (error "channels closed" chan)))))

(define (chan-close c)
  (mutex-lock! (chan-mutex c))
  (chan-closed-set! c #t)
  (for-each semaphore-signal! (chan-semaphores c))
  (mutex-unlock! (chan-mutex c)))

;; apply proc to each incoming msg as they appear on the channel,
;; return (void) when channel is emptied and closed.
(define (chan-for-each c proc)
  (let loop ()
    (cond ((chan-receive* c #f) =>
           (lambda (msg)
             (proc (car msg))
             (loop))))))

(define (chan-fold chans proc initial)
  (let loop ((state initial))
    (cond ((chan-receive* chans #f) =>
           (lambda (msg)
             (loop (proc (car msg) state))))
          (else state))))

(define (chan-select* chan.proc-alist timeout timeout-proc)
  (let ((msgpair (chan-receive* chan.proc-alist timeout)))
    (cond ((eq? #t msgpair) (timeout-proc)) ;; <-- timeout
          (msgpair
           (let ((msg (car msgpair))
                 (proc (cdr msgpair)))
             (proc msg)))
          (else (error "channels closed" chan.proc-alist)))))

;; (chan-select
;;  (c1 msg body ...)
;;  (10 timeout-body ...)
;;  (c2 obj body ...))
;; becomes:
;; (chan-select*
;;  (cons (cons       c1 (lambda (msg) body ...))
;;        (cons (cons c2 (lambda (obj) body ...)) '()))
;;  10 (lambda () timeout-body ...))

(define-syntax %chan-select
  (syntax-rules ()
    ((_ (channel varname body ...) rest ...)
     (cons (cons channel (lambda (varname) body ...))
           (%chan-select rest ...)))
    ((_) '())))

