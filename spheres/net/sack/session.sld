;;!!! API for session handling in sack. This is the glue and common
;; interface that is used by sack applications that use sessions and
;; different session implementation middleware.
;; .author Per Eckerdal, 2008-2009
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/net/sack session)
  (export make-session-variable
          session-destroy!
          with-session-store)

  (include "session.scm"))
