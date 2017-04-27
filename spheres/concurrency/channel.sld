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

  (import (spheres/core match))

  (define-syntax go
    (syntax-rules ()
      ((_ body ...)
       (thread-start! (lambda () body ...)))))

  ;; turn gochan-select form into `((,chan1 . ,proc1) (,chan2 . ,proc2) ...)
  (define-syntax gochan-select-alist
    (syntax-rules (-> <- else)

      ;; recv without ok flag
      ((_ ((channel -> varname) body ...) rest ...)
       `((,channel ,(lambda (varname ok) (if ok (begin body ...))))
         ,@(gochan-select-alist rest ...)))
      ;; recv with ok flag
      ((_ ((channel -> varname ok) body ...) rest ...)
       `((,channel ,(lambda (varname ok) (begin body ...)))
         ,@(gochan-select-alist rest ...)))
      ;; send without ok flag
      ((_ ((channel <- msg) body ...) rest ...)
       `((,channel ,(lambda (_ ok) (if ok (begin body ...))) ,msg)
         ,@(gochan-select-alist rest ...)))
      ;; send with ok flag
      ((_ ((channel <- msg ok) body ...) rest ...)
       `((,channel ,(lambda (_ ok) (begin body ...)) ,msg)
         ,@(gochan-select-alist rest ...)))
      ;; default (no rest ..., else must be last expression)
      ((_ (else body ...))
       `((else ,(lambda (_ _) (begin body ...)))))

      ((_) '())))

  (define-syntax gochan-select
    (syntax-rules ()
      ((_ form ...)
       (call-with-values
           (lambda () (gochan-select* (gochan-select-alist form ...)))
         (lambda (?msg ?ok ?meta)
           (?meta ?msg ?ok))))))

  (include "channel.scm"))
