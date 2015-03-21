;;; Tools for making session middleware that rely on putting the
;;; session id in a cookie.
;;;
;;; Copyright (c) 2008-2009 Per Eckerdal

;;   API for session sack middleware:
;; (session-generate-id)
;; (cookie-id-based-session-store
;;  app get: destroy!: commit!: [generate-id:] [cookie-name:])
;;
;; the get function will be called at most once, with a session id as
;; parameter. It should return a hash table or #f.
;;
;; the destroy! function will be called at most once, always after
;; thunk has returned, and never with a session id other than that sent
;; to get.
;;
;; the commit! function will be called at most once, always after
;; thunk has returned. commit! will always be called if get has been
;; called and not destroy!. It takes an id and a hash table as
;; parameters.
;;
;; get, destroy!, commit! must be thread safe if the server is multi
;; threaded.

;; TODO Update to match the specification for get-id, regenerate-id!,
;; destroy!, get-all


(define (default-session-generate-id)
  (make-uuid))

(define default-session-cookie-name "_s")

(define* (cookie-id-based-session-store app
                                        (get: #f)
                                        (destroy!: #f)
                                        (commit!: #f)
                                        (generate-id: default-session-generate-id)
                                        (cookie-name: default-session-cookie-name))
  (if (not (and get destroy! commit! generate-id cookie-name))
      (error "Not all required parameters were supplied"))
  (lambda (env)
    (let* ((get-cookie (env 'sack:cookie:get))
           (set-cookie! (env 'sack:cookie:set!))
           ;; The session hash table. #f means that the session isn't
           ;; retrieved yet or that the session has been destroyed and
           ;; not been recreated.
           (session #f)
           ;; This is a flag that is set when the session that was in
           ;; the beginning of the request should be destroyed. This
           ;; is also used to implement sack:session:regenerate-id!
           (session-destroyed? #f)
           (initial-session-id (let ((cookie (get-cookie cookie-name)))
                                 (and cookie (cookie-value cookie))))
           ;; The current session id. This should never be #f if
           ;; session is #f.
           (session-id initial-session-id)
           (generate-and-set-session-id!
            (lambda ()
              (set! session-id (generate-id))
              (set-cookie! (make-cookie cookie-name
                                        session-id
                                        path: "/"))
              session-id))
           (get-session
            (lambda ()
              (or session
                  (begin
                    (set! session
                          (cond
                           ((not session-id)
                            (generate-and-set-session-id!)
                            (make-table))
                           (session-destroyed?
                            (make-table))
                           (else
                            (let ((val (get session-id)))
                              (or val
                                  (begin ;; Invalid session id
                                    (generate-and-set-session-id!)
                                    (make-table)))))))
                    session))))
           (extended-functions-table
            (list->table
             `((sack:session:get-all
                .
                ,(lambda ()
                   (table->list (get-session))))
               (sack:session:get
                .
                ,(lambda (name)
                   (table-ref (get-session) name #f)))
               (sack:session:set!
                .
                ,(lambda (name value)
                   (table-set! (get-session) name value)))
               (sack:session:delete!
                .
                ,(lambda (name)
                   (table-set! (get-session) name)))
               (sack:session:destroy!
                .
                ,(lambda ()
                   (set! session-destroyed? #t)
                   (set! session #f)
                   (set-cookie!
                    (make-cookie cookie-name
                                 ""
                                 expires: date-in-the-past))))
               (sack:session:get-id
                .
                ,(lambda ()
                   session-id))
               (sack:session:regenerate-id!
                .
                ,(lambda ()
                   (generate-and-set-session-id!)
                   (set! session-destroyed? #t)
                   (set! session-id (generate-id))))))))
      (let ((return-value
             (app
              (lambda (arg)
                (let ((fn (table-ref extended-functions-table arg #f)))
                  (or fn (env arg)))))))
        (if session-destroyed?
            (destroy! initial-session-id))
        (if session
            (commit! session-id session))
        return-value))))
