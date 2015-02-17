
; The `sxp:' prefix added to prevent name collision with "SSAX-code.scm"
(define-macro (sxp:run-test selector node expected-result)
  (let ((res (gensym)))
    `(begin
       (cerr "\nApplying " ',selector "\nto " ,node nl)
       (let ((,res (,selector ,node)))
	 (if (equal? ,res ,expected-result)
	     (cerr "gave the expected result: "
		   (lambda (port) (write ,res port)) nl)
	     (sxp:error "Unexpected result: " ,res "\nexpected"
                        ,expected-result))))))
