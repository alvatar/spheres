;;!!! shift/reset
;; .author Oleg Kiselyov

;; The implementation of ordinary shift/reset derived 
;; by simplifying multi-prompt shift/reset in delimcc.scm

;; Although the present code should work on any R5RS Scheme system,
;; good performance should be expected only on the systems that implement
;; call/cc efficiently, such as Chez Scheme, Scheme48, Gambit, Larceny.

;; Even on systems that support call/cc only inefficiently,
;; this implementation has an advantage of not leaking memory.
;; The captured continuation, reified by shift, corresponds only
;; to the needed prefix of the full continuation, _even_
;; if call/cc copies the whole stack. In other words, this implementation
;; has a so-called JAR hack (see shift-reset.scm in Scheme48 distribution)
;; built in. Please see the memory-leak test at the end.


;; This ought to be a call-with-unwinding-continuation, if an
;; implementation supports such a thing.
;; (define call/cc call-with-current-continuation)
(define-syntax call/cc
  (syntax-rules ()
    ((_ . ?rest)
     (call-with-current-continuation . ?rest))))

(define shift-reset:go #f)

;; pstack is a list of k: stack fragments
(define shift-reset:pstack '())

;; Execute a thunk in the empty environment -- at the bottom of the stack --
;; and pass the result, too encapsulated as a thunk, to the
;; continuation at the top of pstack. The top-most pstack frame is
;; removed.

;; We rely on the insight that the capture of a delimited continuation
;; can be reduced to the capture of the undelimited one. We invoke 
;; (go th) to execute the thunk th in the delimited context. 
;; The call to 'go' is evaluated almost in the empty context
;; (near the `bottom of the stack'). Therefore,
;; any call/cc operation encountered during the evaluation of th
;; will capture at most the context established by the 'go' call, NOT
;; including the context of go's caller. Informally, invoking (go th)
;; creates a new stack segment; continuations captured by call/cc
;; cannot span the segment boundaries, and are hence delimited.

;; This emulation of delimited control is efficient providing that
;; call/cc is implemented efficiently, with the hybrid heap/stack or
;; stack segment strategies.

(let ((v
       (call/cc
        (lambda (k)
          (set! shift-reset:go k)
          (k #f)))))
  (if v
      (let* ((r (v))
             (h (car shift-reset:pstack))
             (_ (set! shift-reset:pstack
                      (cdr shift-reset:pstack))))
        (h r))))                        ; does not return

;; let push_prompt_aux (p : 'a prompt) (body : unit -> 'a) : 'a =
;;   let ek = get_ek () in
;;   let pframe = {pfr_mark = p.mark; pfr_ek = ek} in
;;   let () = ptop := pframe :: (!ptop) in
;;   let res = body () in
;;   let () = p.mbox := fun () -> res in
;;   raise DelimCCE

(define (reset* th)
  (call/cc
   (lambda (k)
     (set! shift-reset:pstack (cons k shift-reset:pstack))
     (shift-reset:go th))))             ; does not return

(define (shift* f)
  (call/cc
   (lambda (k)                               ; stack fragment
     (shift-reset:go 
      (lambda () 
        (f 
         (lambda (v)
           (call/cc (lambda (k1)
                      (set! shift-reset:pstack
                            (cons k1 shift-reset:pstack))
                      (k v))))))))))


;; ; ------------------------------- Tests

;; (display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
;; (newline)
;; ; --> 117

;; (display (* 10 (reset (* 2 (shift g (* 5 (shift f (+ (f 1) 1))))))))
;; (newline)
;; ; --> 60

;; (display (let ((f (lambda (x) (shift k (k (k x))))))
;; 	   (+ 1 (reset (+ 10 (f 100))))))
;; (newline)
;; ; --> 121

;; ; shift f1 tests that we implement shift rather than shift0
;; (display (reset
;; 	   (let ((x (shift f 
;; 		      (shift f1 (f1 (cons 'a (f '())))))))
;; 	     (shift g x))))
;; (newline)
;; ; ==> '(a)

;; (define (p x) (if (eq? x p) '(p p) `(p ,x)))
;; (reset (display (let ((x 'abcde)) (eq? x ((shift* shift*) x)))))
;; (newline)

;; (define traverse
;;   (lambda (xs)
;;     (letrec ((visit
;; 	       (lambda (xs)
;; 		 (if (null? xs)
;; 		   '()
;; 		   (visit (shift*
;; 			    (lambda (k)
;; 			      (cons (car xs) (k (cdr xs))))))))))
;;       (reset*
;; 	(lambda ()
;; 	  (visit xs))))))

;; (display "Ex by Olivier Danvy") (newline)
;; (display (traverse '(1 2 3 4 5)))
;; (newline)


;; ; Testing garbage-retention in Petite Chez Scheme
;; ; Using guardians
;; ; For explanations: http://www.scheme.com/csug/smgmt.html#g2352
;; ; This memory leak test is due to Chung-chieh Shan.
;; ; This test can be adjusted to run on any other system:
;; ; it should loop forever in constant memory. In fact, it was first
;; ; written in portable Scheme; guardians were added later.

;; (define (test-gc)
;;   (let ((g (make-guardian)))
;;     (let loop ((junk-identity
;; 		 (let ((junk (list 'junk)))
;; 		   (cons junk (reset (shift f f)))))
;; 		(done 10))
;;       (if (zero? done)
;; 	(begin
;; 	  (collect (collect-maximum-generation)) ; force all collection
;; 	  (display "checking if junk became inacessible:") (newline)
;; 	  (do ((junk-inaccessible (g) (g))) ((not junk-inaccessible))
;; 	    (display "collected junk of size: ")
;; 	    (display junk-inaccessible)
;; 	    (newline)))
;; 	(begin
;; 	  (g (car junk-identity)) ; register with the guardian
;; 	  (set-cdr! (car junk-identity)
;; 	    (list (cdr junk-identity)))
;; 	  (loop (cons (cdr (car junk-identity))
;; 		  (cdr junk-identity)) (- done 1)))))))


;; ;; > (test-gc)
;; ;; checking if junk became inacessible:
;; ;; collected junk of size: (junk #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; ;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; ;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; ;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; ;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; ;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; ;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; ;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure>)
;; ;; collected junk of size: (#<procedure> #<procedure> #<procedure>)
;; ;; collected junk of size: (#<procedure> #<procedure>)
;; ;;

;; ; This listing shows that the junk is collected rather than retained.
;; ; In contrast, the original implementation of shift/reset in terms of
;; ; call/cc collects nothing: all junk is retained.


;; ; Another leak test
;; (define (leak-test1-g identity-thunk)
;;   (let ((g (make-guardian)))
;;     (let loop ((id (lambda (x) x)) (done 10))
;;       (if (zero? done)
;; 	(begin
;; 	  (collect (collect-maximum-generation)) ; force all collection
;; 	  (display "collected pieces of junk: ")
;; 	  (display
;; 	    (do ((junk-inaccessible (g) (g)) (c 0 (+ 1 c)))
;; 	        ((not junk-inaccessible) c)))
;; 	    (newline))
;; 	 (begin
;; 	   (g id) ; register with the guardian
;; 	   (loop (id (identity-thunk)) (- done 1)))))))

;; ; (leak-test1-g (lambda () (reset (shift f f))))
;; ; collected pieces of junk: 10
