;;!!! In-RAM Mailbox object
;;
;; .author Marc Feeley, 1994-2009

(define-library (spheres/concurrency mailbox)
  (export make-empty-mailbox
          mailbox-put!
          mailbox-get!)

  (include "mailbox.scm"))
