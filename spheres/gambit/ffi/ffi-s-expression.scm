;; !!! FFI S-expression extensions
;; .author Mikael More, 2010-2012
;; .author Alvaro Castro-Castilla, 2015
;; Copyright (C) 2010, 2011, 2012 Mikael More. MIT license.
;;
;; Adds ability to properly serialize FFI types on (write) and properly deserialize them on (read).
;;
;; ffi types need help - a ffi-write-transformer needs to be added for each ffi type.
;;
;; ## Exports
;; (ffi-write-transformer-add!
;;  'name-of-your-ffi-type
;;  (lambda (v) `(name-of-constructor-procedure-to-create-an-instance-of-this-ffi-type
;;                [ arguments needed to constructor to produce an instance exactly like
;;                the one in the v variable ])))
;;
;; ## Example use
;; (ffi-write-transformer-add!
;;  'your-ffi-2d-coord
;;  (lambda (v) `(make-your-ffi-2e-coord ,(your-ffi-2d-coord-x v) ,(your-ffi-2d-coord-y v)))
;;
;; REPL interaction now looks like
;;
;;      > (make-jason-2d-coord 10 20)
;;      #.(make-jason-2d-coord 10 20)
;;      >
;;
;; (rather than printing #<foreign your-ffi-2d-coord #x1234567>)
;;
;; ## References
;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2013-March/006510.html
;;
;; ## TODO
;;  * Structures and  .. structs are serialized automatically, while a possible extension would be
;;    that instead of translating ffi types into (eval) commands, translate them to
;;    #<ffi type-name . args>, have ffi-transformers using which reconstruction is made.
;;

;; key = ffi type name ('ptime etc.), key = procedure that takes such a value as argument and
;; returns an s-exp that produces one.
(define *ffi-write-transformers* (make-table test: eq?))

(define *sexp-ext:writer-under* #f)

(define (ffi-write-transformer-add! type serializer-proc)
  (table-set! *ffi-write-transformers* type serializer-proc))

;; Does not respect macro-writeenv-limit.
(define (%%sexp-ext:wr we obj)
  (cond
   ;; ((##structure?
   ((##foreign? obj)
    (let* ((name (let ((v (foreign-tags obj)))
                   (and (pair? v) (car v))))
           (transformer (table-ref *ffi-write-transformers* name #f)))
      (if transformer
          (let ((transformed-to (transformer obj)))
            (##wr-str we "#.")
            ;; (##wr-pair we transformed-to) - transformed-to may be sth else for instance a symbol
            ;; so instead go with the universal:
            (*sexp-ext:writer-under* we transformed-to))
          (##wr-foreign we obj))))
   (else
    (*sexp-ext:writer-under* we obj))))

;;! Reads #<tm [data] etc.
;; At entry to this procedure, reader is still at the
;; How to implement it: Check if post-<-char is not #\< and not #\|.
;; If so: Read all until coming >. Skip the object id. Somehow from the type definition of the
;; addressed type, get what names what slots have, and fill out the slots described by the
;; keywords with the respective values. Then produce the record, perhaps using the same mechanism
;; that ##read-sharp-dot uses to (eval) from the global namespace.
;; If not: Somehow rewind the filepos 1 or 2 bytes, possibly using
;; (macro-readenv-filepos-set! re start-pos) or (1- startpos).
(define (%%sexp-ext:read-sharp-less re next start-pos)
  (define (readenv-port re) (##vector-ref re 1)) ; = hack
  (let* ((port (readenv-port re))
         (<-char (read-char port))
         (post-<-char (peek-char port)))
    (print "<-char = " <-char ", then " (read-char port) "\n")
    #t)) ; (sigsegvs on return, learn more from ##..read-sharp-less what to return.)

(define (ffi-sexp-ext-install!)
  (and (not (eq? *sexp-ext:writer-under* ##wr))
       (begin
         (set! *sexp-ext:writer-under* ##wr)
         (set! ##wr %%sexp-ext:wr)
         (let* ((port (repl-input-port))
                (rt (input-port-readtable port)))
           ;; (##readtable-char-sharp-handler-set! rt #\< sexp-ext:read-sharp-less)
           (input-port-readtable-set! port (readtable-eval-allowed?-set rt #t))
           (void)))))

;; (ffi-sexp-ext-install!)

(define (ffi-sexp-ext-uninstall!)
  (if (eq? ##wr %%sexp-ext:wr)
      (begin (set! ##wr *sexp-ext:writer-under*)
             (set! *sexp-ext:writer-under* #f))))

(define object->string/ext object->string)

(define (string->object/ext s)
  (call-with-input-string
   s
   (lambda (port)
     (input-port-readtable-set!
      port (readtable-eval-allowed?-set (input-port-readtable port) #t))
     (read port))))
