;;!!! API for session handling in sack. This is the glue and common
;; interface that is used by sack applications that use sessions and
;; different session implementation middleware.
;; .author Per Eckerdal, 2008-2009
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 2008-2009 Per Eckerdal
;;
;; Different types of sessions:
;;
;; * Memory pool based
;; * Memcached based
;; * Database based
;; * File based
;;
;; * Serialized vs non-serialized (?)
;;
;; * Cookie data based
;; * Cookie-id based
;; * URL-rewriting-id based
;;
;; How to design an API that hides these differences?
;;
;;   API for code that uses sessions:
;; (make-session-variable [unique-id (a string)])
;; (session-destroy!)
;;
;;   API for session sack middleware:
;; (with-session-store fetch store! destroy! thunk [extra-data])
;; (session-store-extra-data)



(define-type session-store
  id: 46ECA2AB-010A-4EE2-B87A-2802390672F0
  constructor: make-session-store
  (fetch read-only:)
  (store! read-only:)
  (destroy! read-only:))

(define session-store (make-parameter #f))

(define (make-counter)
  (let ((num 0))
    (lambda ()
      (set! num (+ num 1))
      num)))

(define session-variable-counter (make-counter))
(define session-variable-nochange (list 'nochange))

(define* (make-session-variable (id (session-variable-counter)))
  (lambda* ((set-to session-variable-nochange))
      (let ((store (session-store)))
        (if (eq? set-to session-variable-nochange)
            (and store
                 ((session-store-fetch conf) id))
            (if (session-store-store! id set-to)
                '(session-open conf) ;; TODO
                (error "Not in a session dynamic environment"))))))

(define (session-destroy!)
  ((session-store-destroy!
    (or (session-store)
        (error "Not in a session dynamic environment")))))

(define (with-session-store fetch store! destroy! thunk)
  (parameterize
   ((session-store (make-session-store fetch store! destroy!)))
   (thunk)))
