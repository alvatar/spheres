;;!!! Exceptions
;; .author Christian Jaeger, 2006-2008
;; .author Per Eckerdal
;; .author Mikael More
;; .author Alvaro Castro-Castilla, 2015


;; todo what;s their new safe name? none?
(define (cmd-b cont port depth)
  (if (and (##continuation? cont)
	   (port? port)
	   (fixnum? depth))
      (##cmd-b cont port depth)
      (error "cmd-b: invalid argument types of cont port depth:" cont port depth)))

(define (cmd-y cont port pinpoint? depth)
  (if (and (##continuation? cont)
	   (port? port)
	   (fixnum? depth))
      (##cmd-y cont port pinpoint? depth)
      (error "cmd-y: invalid argument types of cont port depth:" cont port depth)))

(define-type exception/continuation
  id: 4bad9e82-f84c-4ae4-9ba7-c8964bf3dffc
  exception
  continuation)

;; private
(define (to-port-or-string maybe-port fn)
  (if maybe-port
      (fn maybe-port)
      (with-output-to-string "" (lambda () (fn (current-output-port))))))

(define* (exception/continuation-contextline e (port #f))
  (to-port-or-string
   port
   (lambda (port)
     (cmd-y (exception/continuation-continuation e)
	    port
	    #f
	    0))))

(define* (exception/continuation-contextlines e (port #f))
  (to-port-or-string
   port
   (lambda (port)
     (cmd-b (exception/continuation-continuation e)
	    port
	    0))))

(define* (exception/continuation-message-in-context e (port #f))
  (to-port-or-string
   port
   (lambda (port)
     (display-exception-in-context (exception/continuation-exception e)
				   (exception/continuation-continuation e)
				   port))))

(define (exception/continuation-procedure e)
  (##exception->procedure
   (exception/continuation-exception e)
   (exception/continuation-continuation e)))

(define (exception/continuation-locat e)
  (##exception->locat
   (exception/continuation-exception e)
   (exception/continuation-continuation e)))


;;-------------------------------------------------------------------------------
;;!! Delegates

(define* (exception/continuation-text e (port #f))
  (to-port-or-string
   port
   (lambda (port)
     (display-exception
      (exception/continuation-exception e)
      port))))

(define (repl-within-exception/continuation e)
  (if (exception/continuation? e)
      (##repl-within (exception/continuation-continuation e)
		     "repl-within-exception/continuation")
      ;; ^ don't know where the 2nd argument to ##repl-within is used
      (error " not a exception/continuation:" e)))


;;-------------------------------------------------------------------------------
;; Serialisation

;; private
(define-type exception/continuation&string
  id: d3a6b590-3d09-48e2-99e3-01e076126796
  exception/continuation
  string)

(define (exception/continuation->serialisation-object e)
  (make-exception/continuation&string
   e
   (exception/continuation-contextlines e)))

(define (exception/continuation->u8vector e)
  (object->u8vector
   (exception/continuation->serialisation-object e)))

(define (u8vector->backtrace vec)
  (exception/continuation&string-string
   (u8vector->object vec)))

(define (with-exception/continuation-catcher handler th)
  (continuation-capture
   (lambda (cont)
     (with-exception-handler
      (lambda (e)
	(continuation-capture
	 (lambda (c)
	   (continuation-graft
	    cont
	    handler
	    (make-exception/continuation e c)))))
      th))))

;; example:
;;  create an exception object with the continuation and
;;  raise this in the context of with-ec-catcher

;; private
(define (with-ec-catcher thunk)
  (continuation-capture
   (lambda (exit)
     (with-exception-handler
      (lambda (e)
	(continuation-capture
	 (lambda (cont)
	   (continuation-graft
	    exit
	    (lambda ()
	      (raise (make-exception/continuation e cont)))))))
      thunk))))

(define (exception->string exc)
  (call-with-output-string
   ""
   (lambda (port)
     (display-exception exc port))))

(define* (exception/continuation->string exc (for-console #f))
  (let* ((errdesc (exception->string exc))
         (the-display-exception-in-context
          (with-exception-handler
           exception->string
           (lambda ()
             (call-with-output-string
              ""
              (lambda (port)
                (display-exception-in-context
                 (exception/continuation-continuation exc)
                 (exception/continuation-continuation exc)
                 port))))))
         (the-exception/continuation-message-in-context
          (exception/continuation-message-in-context exc))
         (the-cmd-b ; (exception/continuation-contextline exc)
          (with-exception-handler
           exception->string
           (lambda ()
             (call-with-output-string
              (list output-width: 200)
              (lambda (port)
                (cmd-b (exception/continuation-continuation exc)
                       port
                       0)))))))
    (if for-console
        (string-append
         "***\n"
         "*** Exception thrown.\n"
         "*** display-exception: " errdesc "\n"
         "*** display-exception-in-context: "
         the-display-exception-in-context "\n"
         "*** exception/continuation-message-in-context: "
         the-exception/continuation-message-in-context "\n"
         "*** cmd-b: " the-cmd-b)
        (string-append
         "display-exception:\n"
         errdesc "\n"
         "display-exception-in-context:\n"
         the-display-exception-in-context "\n"
         "exception/continuation-message-in-context:\n"
         the-exception/continuation-message-in-context "\n"
         "cmd-b: " the-cmd-b))))
