;;!!! A simple first-in-first-out queue library. The implementation is
;; list-based and relies on set-car!/set-cdr!. All operations are constant time.
;; .author Per Eckerdal, 2008
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure queue)
  (export make-queue
          queue-push!
          queue-pop!
          queue-size
          queue-empty?
          queue-empty!
          queue-front)

  (include "queue.scm"))
