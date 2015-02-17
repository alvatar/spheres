
(define-macro (vcntxt:run-test selector node expected-result)
  (let ((res (gensym)))
    `(begin
       (cerr "\nApplying " ',selector "\nto " ,node nl)
       (let ((,res (,selector ,node)))
	 (if (equal? ,res ,expected-result)
	     (cerr "gave the expected result: "
		   (lambda (port) (write ,res port)) nl)
	     (cntxt:error "Unexpected result: " ,res "\nexpected"
                          ,expected-result))))))
