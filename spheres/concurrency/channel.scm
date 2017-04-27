;;!!! thread-safe channel (FIFO), inspired by channels from Go
;;
;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; Copyright (c) 2017 Kristian Lein-Mathisen.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; multiple receives
;; multiple sends
;; multiple receive/send simultaniously
;; buffering
;; timeouts (as channels)

;; for me, it helps to think about semaphore as return-values that can
;; block. each gochan-select will create a semaphore and wait for
;; somebody to signal it (sometimes immediately (without waiting),
;; sometimes externally (having to wait)). it's important to
;; understand that it's ok for a semaphore to be registered in a
;; channel (as sender/receiver) even though it's already been
;; signalled. this is ok because already-signalled subscribers just be
;; skipped (you cannot re-deliver data to a (not %gosem-open?)
;; semaphore).
(define-record-type gosem
  (make-gosem mutex cv data meta ok)
  gosem?
  (mutex gosem-mutex)
  (cv    gosem-cv)
  (data  gosem-data gosem-data-set!)
  (meta  gosem-meta gosem-meta-set!)
  (ok    gosem-ok   gosem-ok-set!))

(define (make-semaphore)
  (make-gosem (make-mutex)
              (make-condition-variable)
              #f
              #f
              #t))

(define (%gosem-open? sem) (eq? #f (gosem-meta sem)))

;; returns #t on successful signal, #f if semaphore was already
;; signalled.
(define (semaphore-signal! sem data meta ok)
  (info "signalling " sem " meta: " meta " data: " data " ok: " ok)
  (mutex-lock! (gosem-mutex sem))
  (cond ((%gosem-open? sem) ;; available!
         (gosem-data-set! sem data)
         (gosem-meta-set! sem meta)
         (gosem-ok-set! sem ok)
         (condition-variable-signal! (gosem-cv sem))
         (mutex-unlock! (gosem-mutex sem))
         #t)
        (else ;; already signalled
         (mutex-unlock! (gosem-mutex sem))
         #f)))

(define-record-type gotimer
  (make-gotimer mutex receivers when data ok next)
  gotimer?
  (mutex     gotimer-mutex)
  (receivers gotimer-receivers gotimer-receivers-set!)
  (when      gotimer-when gotimer-when-set!) ;; when may be #f if never to trigger again
  (data      gotimer-data gotimer-data-set!)
  (ok        gotimer-ok   gotimer-ok-set!)
  (next      gotimer-next))

;; next must be a thunk which returns (values when-next data ok),
;; where `when-next` is when to trigger next in
;; (current-milliseconds). `next` will be called exaclty once on every
;; timeout and once at "startup" and can thus mutate its own private
;; state. `next` is called within a gotimer mutex lock, so it
;; shouldn't ever error!
(define (gotimer next)
  (receive (when-next data ok) (next)
           (make-gotimer (make-mutex)
                         (list->queue '())
                         when-next
                         data
                         ok
                         next)))

;; it's important that when-next is set in lockstep as gotimer-next is
;; called so that gotimer-next get's called only once per "when" it
;; returns. gotimer-next should never be called if gotimer-when is #f.
(define (gotimer-tick! timer)
  (receive (when-next data ok) ((gotimer-next timer))
           (gotimer-when-set! timer when-next) ;; may be #f if never to timout again
           (gotimer-data-set! timer data)
           (gotimer-ok-set!   timer ok)))


(define (gochan-after duration:ms)
  (let ((when (+ (current-milliseconds) duration:ms)))
    (gotimer (lambda () (let ((tmp when))
                          (set! when #f) ;; never trigger again
                          ;;      when-next  data ok
                          (values tmp        tmp  #t))))))

(define (gochan-tick duration:ms)
  (let ((when (current-milliseconds)))
    (gotimer (lambda ()
               (set! when (+ when duration:ms))
               (values when when #t)))))

(define-record-type gochan
  (make-gochan mutex cap buffer receivers senders closed)
  gochan?
  (mutex     gochan-mutex)
  (cap       gochan-cap)
  (buffer    gochan-buffer)
  (receivers gochan-receivers)
  (senders   gochan-senders)
  (closed    gochan-closed gochan-closed-set!))

(define (gochan cap)
  (assert (fixnum? cap))
  (make-gochan (make-mutex)
               cap               ;; capacity (max buffer size)
               (list->queue '()) ;; buffer
               (list->queue '()) ;; senders
               (list->queue '()) ;; receivers
               #f))              ;; not closed

(define (make-send-subscription sem data meta) (cons sem (cons data meta)))
(define send-subscription-sem  car)
(define send-subscription-data cadr)
(define send-subscription-meta cddr)

(define (make-recv-subscription sem meta) (cons sem meta))
(define recv-subscription-sem  car)
(define recv-subscription-meta cdr)

(define (%gochan-free-capacity? chan)
  (< (queue-length (gochan-buffer chan))
     (gochan-cap chan)))

;; we want to send, so let's notify any receivers that are ready. if
;; this succeeds, we close %sem. otherwise we return #f and we'll need
;; to use our semaphore. %sem must already be locked and open!
;;
;; returns #t if registered as subscriber, #f otherwise.
(define (gochan-signal-receiver/subscribe chan %sem msg meta)
  ;; because meta is also used to tell if a semaphore has already been
  ;; signalled (#f) or not (≠ #f).
  (if (eq? #f meta) (error "metadata cannot be #f (in gochan-select* alist)"))
  (mutex-lock! (gochan-mutex chan))

  (if (gochan-closed chan)
      ;; trying to send to a closed channel! the golang channel api
      ;; panics in this situation. but we've got the `ok` variable
      ;; conveniently available at the results of gochan-sends (just
      ;; like on gochan-receive), so let's try this instead: sending
      ;; to a closed channel will not panic, but instead immediately
      ;; unblock and receive a zero-value with `ok` set to #f.
      (begin
        (gosem-meta-set! %sem meta) ;; closes semaphore
        (gosem-ok-set!   %sem #f) ;; sending to closed channel is not ok!
        (mutex-unlock! (gochan-mutex chan))
        #f)
      (let ((q (gochan-receivers chan)))
        (let %loop ()
          (if (queue-empty? q)
              (begin
                ;; no semaphores left, nobody is around to receive our
                ;; data :( try to fill the buffer then
                (if (%gochan-free-capacity? chan)
                    (begin (queue-add! (gochan-buffer chan) msg)
                           (gosem-meta-set! %sem meta) ;; closes semaphore
                           (gosem-ok-set!   %sem #t)
                           (mutex-unlock! (gochan-mutex chan))
                           #f)
                    ;; buffer didn't help us either! we are running
                    ;; out of options now. we'll need to allow some
                    ;; future receiver to notify us when they need
                    ;; data :(
                    (begin (assert (%gosem-open? %sem))
                           (queue-add! (gochan-senders chan)
                                       (make-send-subscription %sem msg meta))
                           (mutex-unlock! (gochan-mutex chan))
                           #t)))
              (let ((sub (queue-remove! q)))
                (if (semaphore-signal! (recv-subscription-sem sub) msg
                                       (recv-subscription-meta sub) #t)
                    ;; receiver was signalled, signal self
                    (begin (gosem-meta-set! %sem meta) ;; close!
                           (mutex-unlock! (gochan-mutex chan))
                           #f)
                    ;; receiver was already signalled by somebody else,
                    ;; try next receiver
                    (%loop))))))))

;; we want to receive stuff, try to signal someone who's ready to
;; send. %sem must be locked and open!
;;
;; returns #t if semaphore was registered on channel.
(define (gochan-signal-sender/subscribe chan %sem meta)
  (if (eq? #f meta) (error "metadata cannot be #f (in gochan-select* alist)"))
  (mutex-lock! (gochan-mutex chan))
  ;; TODO: if closed, signal receiver immediately

  (if (> (queue-length (gochan-buffer chan)) 0)
      ;; data already in buffer! pop buffer and put data in our
      ;; semaphore for ourselves (then signal any blocked senders).
      (begin (gosem-meta-set! %sem meta) ;; close
             (gosem-data-set! %sem (queue-remove! (gochan-buffer chan)))
             (gosem-ok-set!   %sem #t)
             ;; here's a trick! since we just freed 1 buffer item, so
             ;; we might have need to unblocking a sender who's
             ;; waiting for buffer capacity. we can do it all in one
             ;; go here on the sender's behalf, since we've got the
             ;; channel mutex already: signal a sender and put its
             ;; data in the buffer.
             (let ((q (gochan-senders chan)))
               (let %loop ()
                 (unless (queue-empty? q) ;; nobody to signal? no problem
                         (let ((sub (queue-remove! q)))
                           (if (semaphore-signal! (send-subscription-sem sub) #f
                                                  (send-subscription-meta sub) #t)
                               ;; sender was signalled/unblocked! add its
                               ;; data to the buffer
                               (begin (queue-add! (gochan-buffer chan)
                                                  (send-subscription-data sub))
                                      (mutex-unlock! (gochan-mutex chan))
                                      #f)
                               ;; sender was already signalled externally, try next
                               (%loop))))))
             (mutex-unlock! (gochan-mutex chan)))
      (if (gochan-closed chan)
          ;; oh no! trying to receive from a closed channel. you know the
          ;; drill.
          (begin (gosem-meta-set! %sem meta)
                 (gosem-ok-set!   %sem #f) ;; not ok this time around!
                 (mutex-unlock! (gochan-mutex chan))
                 #f)
          (let ((q (gochan-senders chan)))
            (let %loop ()
              (if (queue-empty? q)
                  (begin
                    ;; nobody had data for us :-( awww. but we can add
                    ;; ourselves here so when they do, they can signal us.
                    (assert (%gosem-open? %sem))
                    (queue-add! (gochan-receivers chan)
                                (make-recv-subscription %sem meta))
                    (mutex-unlock! (gochan-mutex chan))
                    #t)
                  (let ((sub (queue-remove! q)))
                    ;; signalling a sender-semaphore. they don't care about
                    ;; data, they just want to be unblocked.
                    (if (semaphore-signal! (send-subscription-sem sub) #f (send-subscription-meta sub) #t)
                        ;; receiver was signalled, fill in our semaphore
                        ;; (which can return immediately)
                        (begin (gosem-meta-set! %sem meta) ;; close
                               (gosem-data-set! %sem (send-subscription-data sub))
                               (gosem-ok-set!   %sem #t)
                               (mutex-unlock! (gochan-mutex chan))
                               #f)
                        ;; sender was already signalled externally, try next
                        (%loop)))))))))

;; we want to add a timeout to our semaphore. if the chan (aka gotimer)
;; has already timed out, we immediately alert %sem and tick the
;; timer. otherwise, we register %sem as a chan/gotimer subscriber.
;;
;; returns #t is %sem was registered on timer as receiver subscriber.
(define (gochan-signal-timer/subscribe chan %sem meta)
  ;; note that chan here is really a gotimer
  (if (eq? #f meta) (error "metadata cannot be #f (in gochan-select* alist)"))
  (mutex-lock! (gotimer-mutex chan))
  (if (gotimer-when chan)
      (begin
        (if (<= (gotimer-when chan) (current-milliseconds))
            ;; timer already expired!
            (begin
              (gosem-meta-set! %sem meta) ;; <-- also closes %sem
              (gosem-data-set! %sem (gotimer-data chan))
              (gosem-ok-set!   %sem (gotimer-ok   chan))
              (gotimer-tick! chan)
              (mutex-unlock! (gotimer-mutex chan))
              #f)
            (begin
              ;; timer in the future, register us
              (queue-add! (gotimer-receivers chan)
                          (make-recv-subscription %sem meta))
              (mutex-unlock! (gotimer-mutex chan))
              #t)))
      (begin (mutex-unlock! (gotimer-mutex chan))
             #f)))

;; if timer has expired, signal a single receiver that `timer` has
;; triggered and then tick the timer for its next timeout value.
;;
;; whichever thread we're in, unless the timer has expired, transfer
;; data from the timer to the receiver's semaphore. many threads might
;; be waiting for the same timer, but the first to timer's mutex will
;; do the work on behalf of everybody - the rest will do nothing.
(define (gotimer-signal timer)
  (mutex-lock! (gotimer-mutex timer))
  (info "signalling timer " timer)
  (if (gotimer-when timer)
      (if (<= (gotimer-when timer) (current-milliseconds))
          (let ((q (gotimer-receivers timer)))
            (let loop ()
              (if (queue-empty? q)
                  (void)
                  (let ((sub (queue-remove! q)))
                    (info "trying to signal " sub)
                    (if (semaphore-signal! (recv-subscription-sem sub)
                                           (gotimer-data timer)
                                           (recv-subscription-meta sub)
                                           (gotimer-ok timer))
                        ;; receiver was signalled ok, tick timer.
                        (gotimer-tick! timer)
                        ;; semaphore was already signalled, can't
                        ;; deliver value. try next subscriber!
                        (loop))))))
          (info timer " was postponed"))
      ;; somebody else grabbed our timer trigger from us.
      (info timer " is no longer with us"))
  (mutex-unlock! (gotimer-mutex timer)))

(define (%remove-queue-item q semaphore)
  (let loop ((n (queue-length q)))
    (when (> n 0)
          (let ((sub (queue-remove! q)))
            (unless (eq? semaphore (car sub))
                    (queue-add! q sub)))
          (loop (sub1 n)))))

;; run through the channels' semaphores (queue) and remove any
;; instances of `semaphore`.
(define (gochan-unsubscribe-senders chan semaphore)
  (mutex-lock! (gochan-mutex chan))
  (%remove-queue-item (gochan-senders chan) semaphore)
  (mutex-unlock! (gochan-mutex chan)))

(define (gochan-unsubscribe-receivers chan semaphore)
  (mutex-lock! (gochan-mutex chan))
  (info "unsubscribing " chan)
  (%remove-queue-item (gochan-receivers chan) semaphore)
  (mutex-unlock! (gochan-mutex chan)))

(define (gochan-unsubscribe-timers chan semaphore)
  (mutex-lock! (gotimer-mutex chan))
  (%remove-queue-item (gotimer-receivers chan) semaphore)
  (mutex-unlock! (gotimer-mutex chan)))



;; the heart of it all! takes input that looks like this:
;;
;; (gochan-select `((,chan1 meta1)
;;                  (,chan2 meta2 message)
;;                  (,chan3 meta3) ...))
;;
;; gochan-select* returns:
;;
;; (msg ok meta)
;;
;; where msg is the message that was send over the channel, ok is #f
;; is channel was closed and #t otherwise, meta is the datum supplied
;; in the arguments.
;;
;; if a message arrived on chan3 above, for example, meta would be
;; 'meta3 in that case. this allows you to see which channel a message
;; came from (ie if you supply meta data that's is the channel itself)
;;
(define (gochan-select* chans)
  (let ((semaphore (make-semaphore))
        ;; sorting trick! this lets us load-balance in case there are
        ;; multiple channels with data always available. there's
        ;; probably much faster ways to do this, though...
        (chans (map cdr
                    (sort (map (lambda (spec)
                                 (cons (/ (random 256) 256.0) spec))
                               chans)
                          (lambda (a b) (< (car a) (car b)))))))
    ;; keep our semaphore locked while we check channels for data
    ;; ready, so that we can't get signalled externally while we do
    ;; this.
    (mutex-lock! (gosem-mutex semaphore))
    (let loop ((chans chans)
               (sendsub '()) ;; list of gochans we're subscribed on send
               (recvsub '()) ;; list of gochans we're subscribed on recv
               (timesub '()) ;; list of gotimer we're subscribed on recv/trigger
               (else-thunk #f))
      (if (and (%gosem-open? semaphore)
               (pair? chans))
          (let ((chanspec  (car chans)))
            (match chanspec

                   (((? gochan? chan) meta msg) ;; want to send to chan
                    (loop (cdr chans)
                          (if (gochan-signal-receiver/subscribe chan semaphore msg meta)
                              (cons chan sendsub)
                              sendsub)
                          recvsub
                          timesub
                          else-thunk))

                   (((? gochan? chan) meta) ;; want to recv on chan
                    (loop (cdr chans)
                          sendsub
                          (if (gochan-signal-sender/subscribe   chan semaphore meta)
                              (cons chan recvsub)
                              recvsub)
                          timesub
                          else-thunk))

                   (((? gotimer? chan) meta) ;; want to "recv" on timeout
                    (loop (cdr chans)
                          sendsub
                          recvsub
                          (if (gochan-signal-timer/subscribe    chan semaphore meta)
                              (cons chan timesub)
                              timesub)
                          else-thunk))

                   (('else thunk) ;; want to execute body if nobody available
                    (loop (cdr chans)
                          sendsub
                          recvsub
                          timesub
                          thunk))))
          (let %retry () ;; lock semaphore mutex before retrying!
            (if (%gosem-open? semaphore)
                (if else-thunk
                    ;; no data immediately available, but we have an
                    ;; else clause
                    (begin
                      (gosem-meta-set! semaphore else-thunk)
                      (gosem-data-set! semaphore #f)
                      (gosem-ok-set!   semaphore #t))
                    ;; no data immediately available on any of the
                    ;; channels, so we need to wait for somebody else
                    ;; to signal us.
                    (if (pair? timesub)
                        ;; we need to resort timesub here in case the
                        ;; previous timeout caused gotimer-when
                        ;; modifications. obs: cheeky gotimer-when peek
                        ;; without mutex! since we're mutex-free, timers
                        ;; may trigger at any time in here. but as long as
                        ;; we sort *after* we pick out gotimer-when, we're
                        ;; sure to not all of a sudden have our timeout
                        ;; become #f and that's the only really critical
                        ;; thing.
                        (let* ((timers* (sort (map (lambda (timer)
                                                     (cons (gotimer-when timer) timer))
                                                   timesub)
                                              (lambda (a b) ;; sort #f's last
                                                (let ((a (car a))
                                                      (b (car b)))
                                                  (if a
                                                      (if b (< a b) #t)
                                                      #f)))))
                               (timer (cdr (car timers*)))
                               (timeout (let ((when (car (car timers*))))
                                          (and when (max 0 (/ (- when
                                                                 (current-milliseconds))
                                                              1000))))))
                          (info "wait for data with timer " timer " and timeout " timeout)
                          (if (mutex-unlock! (gosem-mutex semaphore)
                                             (gosem-cv semaphore)
                                             timeout)
                              ;; no timeout, semaphore must have been
                              ;; signalled, data should be in semaphore
                              (void)
                              ;; timeout! if we're lucky, our semaphore
                              ;; won't be open after gotimer-signal.
                              (begin
                                (gotimer-signal timer)
                                ;; at this point, we know there was a
                                ;; timeout on timer but we don't know who
                                ;; received its signal.
                                (mutex-lock! (gosem-mutex semaphore))
                                (%retry))))
                        ;; we don't have any timers, wait on cv forever
                        (begin (info "wait for data without timer")
                               (mutex-unlock! (gosem-mutex semaphore)
                                              (gosem-cv semaphore)))))
                ;; yey, semaphore has data already!
                (begin (info "no need to wait, data already there")
                       (mutex-unlock! (gosem-mutex semaphore))))

            ;; it's important that we remove our semaphore from
            ;; wherever it may be registered so we don't leak it. it
            ;; wouldn't get cleared out otherwise until someone else
            ;; tries to signal it - which may never happen
            (for-each (lambda (chan) (gochan-unsubscribe-senders chan semaphore))   sendsub)
            (for-each (lambda (chan) (gochan-unsubscribe-receivers chan semaphore)) recvsub)
            (for-each (lambda (chan) (gochan-unsubscribe-timers chan semaphore))    timesub)

            (assert (gosem-meta semaphore)) ;; just to make sure
            (values (gosem-data semaphore)
                    (gosem-ok   semaphore)
                    (gosem-meta semaphore)))))))

(define (gochan-send chan msg)
  (assert (gochan? chan))
  (gochan-select* `((,chan #t ,msg))))

(define (gochan-recv chan)
  (assert (or (gochan? chan)
              (gotimer? chan)))
  (gochan-select* `((,chan #t))))

;; close channel. unlike in go, this operation is idempotent (and
;; hopefully that's a good idea).
(define (gochan-close chan)
  (mutex-lock! (gochan-mutex chan))

  ;; NOTE: should we error here, like go, if channel is already
  ;; closed? is that really a good api? shouldn't that be idempotent?

  (info "closing " chan " with "
        "receivers: " (queue->list (gochan-receivers chan))
        "senders: "   (queue->list (gochan-senders   chan)))

  (gochan-closed-set! chan #t)
  ;; signal *everybody* that we're closing (waking them all up,
  ;; because now there are tons of #f-messages available to them)
  (let ((q (gochan-receivers chan)))
    (let %loop ()
      (unless (queue-empty? q)
              (let ((sub (queue-remove! q)))
                (semaphore-signal! (recv-subscription-sem  sub) #f ;; no data
                                   (recv-subscription-meta sub) #f) ;; not ok
                (%loop)))))
  (let ((q (gochan-senders chan)))
    (let %loop ()
      (unless (queue-empty? q)
              (let ((sub (queue-remove! q)))
                (semaphore-signal! (send-subscription-sem  sub) #f ;; no data
                                   (send-subscription-meta sub) #f) ;; not ok
                (%loop)))))

  (mutex-unlock! (gochan-mutex chan)))
