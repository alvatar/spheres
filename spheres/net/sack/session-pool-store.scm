;;!!! Sack session store that stores the session data in memory
;; .author Per Eckerdal, 2008-2009
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 2008-2009 Per Eckerdal


;; It should be possible to optimize the cleanup routine by using some
;; kind of FIFO queue instead of looping through the whole table all
;; the time.


(define* (make-session-pool (timeout: (* 15 60)))
  ;; This code makes use of wills to make it possible to make sure
  ;; that the garbage collector thread terminates itself when the
  ;; session pool is to be garbage collected.
  (let ((data-will (make-will
                    (make-table)
                    (lambda (data)
                      ;; Do nothing; it's not needed. The garbage
                      ;; collector thread will detect that the will's
                      ;; testator object is #f and terminate itself.
                      #f)))
        (mutex (make-mutex)))
    ;; Start the cleanup thread
    (thread-start!
     (make-thread
      (lambda ()
        (let loop ()
          (thread-sleep! timeout)
          (let ((data (will-testator data-will)))
            (cond
             ;; If data is #f, quit the loop. That means that the pool
             ;; object is unreachable, so this cleanup is unnecessary
             ;; and even wasteful.
             (data
              ;; Do the cleanup
              (let ((t (time->seconds (current-time)))
                    ;; A list of session ids that should be
                    ;; removed. This is necessary, because we're not
                    ;; allowed to do table-set!s within the
                    ;; table-for-each loop.
                    (to-be-removed '()))
                (mutex-lock! mutex)
                (table-for-each
                 (lambda (key value)
                   (if (> (- t (car value)) timeout)
                       (set! to-be-removed
                             (cons key to-be-removed))))
                 data)
                (for-each (lambda (key)
                            (table-set! data key))
                          to-be-removed)
                (mutex-unlock! mutex)
                (loop)))))))
      '(pool-session-store cleanup-thread)))
    ;; Create and return the session pool object
    (let ((data (will-testator data-will)))
      (lambda (thunk)
        (mutex-lock! mutex)
        (let ((ret (thunk data)))
          (mutex-unlock! mutex)
          ret)))))

(define* (pool-session-store app
                             (pool: (make-session-pool))
                             (generate-id: default-session-generate-id)
                             (cookie-name: default-session-cookie-name))
  (cookie-id-based-session-store
   app
   get:
   (lambda (id)
     (let ((ret (pool
                 (lambda (tbl)
                   (table-ref tbl id #f)))))
       (and ret (u8vector->object (cdr ret)))))
   destroy!:
   (lambda (id)
     (pool (lambda (tbl)
             (table-set! tbl id))))
   commit!:
   (lambda (id session)
     (pool
      (lambda (tbl)
        (table-set! tbl
                    id
                    (cons (time->seconds (current-time))
                          (object->u8vector session))))))
   generate-id: generate-id
   cookie-name: cookie-name))
