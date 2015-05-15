;;!!! thread-safe channel (FIFO), inspired by channels from Go
;;
;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; Copyright (c) 2014 Kristian Lein-Mathisen.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-library (spheres/concurrency channel)
  (export make-chan
          chan-receive
          chan-receive*
          chan-send
          chan-close
          chan-closed?
          chan-for-each
          chan-fold
          chan-select
          chan-select*)

  (define-macro (chan-select x r t)
    (let loop ((forms (cdr x)) ;; original specs+timeout
               (timeout #f)
               (timeout-proc #f)
               (specs '())) ;; non-timeout specs
      (if (pair? forms)
          (let ((spec (car forms)))
            (if (number? (car spec))
                (if timeout
                    (error "multiple timeouts specified" spec)
                    (loop (cdr forms)
                          (car spec)           ;; timeout in seconds
                          `(lambda () ,@(cdr spec)) ;; timeout body
                          specs))
                (loop (cdr forms)
                      timeout timeout-proc
                      (cons spec specs))))
          `(,(r 'chan-select*)
            (,(r '%chan-select) ,@(reverse specs))
            ,timeout ,timeout-proc))))

  (include "channel.scm"))
