
; Test assertion: (equal? `(,selector ,@params) expected-result) 
(define-macro (xtest-assert expected-result selector . params)
  (let ((res (gensym)))
    `(begin
       (cerr "-- " ',selector nl 
	     (lambda(port) 
	       (for-each (lambda(p)
			   (xtest-ppw p port))
			 (list ,@params))))
       (let ((,res (,selector ,@params)))
	 (if (equal? ,res ,expected-result)
	   (cerr "->" nl
		 (lambda (port) (pp ,res port)) nl)
	   (begin
	     (cerr
              "Unexpected result: " nl
              (lambda (port) (pp ,res port)))
             (cerr
              "Expected: " nl
              (lambda (port) (pp ,expected-result port)))
             (cerr
              "See the difference: " nl
              (lambda (port) (pp (xtest:diff ,res ,expected-result) port)))
	     (exit -1)))))))
	
; Test assertion: 
; (begin
;   `(,selector ,@params) 
;   (equal? var expected-result)) 
(define-macro (xtest-assert-var var expected-result selector . params)
  (let ((res (gensym)))
    `(begin
       (let ((xtest--var ,var))
       (cerr nl xtest-sep-line ',selector xtest-sep-line
	     nl "(" ',selector nl 
	     (lambda(port) 
	       (for-each (lambda(p)
			   (xtest-ppw p port))
			 (list ,@params)))
	      ")")
         (,selector ,@params)
	 (cerr nl ";--[var]-->" nl
		 (lambda (port) (pp xtest--var port)))
	 (or (xtest-equal? ,expected-result xtest--var)
	   (begin
	     (cerr  
		 "### Expected:" nl
		 (lambda (port) (pp ,expected-result port))
		 "### ASSERTION FAILED for: " ',selector nl) 
	     (exit -1)))))))

; Test assertion: (equal? `(,selector ,@params) expected-result) 
(define-macro (xtest-assert-write expected-result selector . params)
  (let ((res (gensym)))
    `(begin
       (cerr nl xtest-sep-line ',selector xtest-sep-line  
	     nl "(" ',selector nl 
	     (lambda(port) 
	       (for-each (lambda(p)
			   (xtest-ppw p port))
			 (list ,@params)))
	     ")")
       (let ((,res (,selector ,@params)))
	   (cerr nl ";---->" nl
		 (lambda (port) (write ,res port)))
	 (or (xtest-equal? ,res ,expected-result)
	   (begin
	     (cerr 
		 "### Expected:" nl
		 (lambda (port) (write ,expected-result port))
		 "### ASSERTION FAILED for: " ',selector nl)
	     (exit -1)))))))
