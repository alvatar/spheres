;;!!! SRFI-51: Handling rest list
;; .author Joo ChurlSoo

;;! rest-values
(define (rest-values rest . default)
  (let* ((caller (if (or (null? default)
			 (boolean? (car default))
			 (integer? (car default))
			 (memq (car default) (list + -)))
		     '()
		     (if (string? rest) rest (list rest))))
	 (rest-list (if (null? caller) rest (car default)))
	 (rest-length (if (list? rest-list)
			  (length rest-list)
			  (if (string? caller)
			      (error caller rest-list 'rest-list
				     '(list? rest-list))
			      (apply error "bad rest list" rest-list 'rest-list
				     '(list? rest-list) caller))))
	 (default (if (null? caller) default (cdr default)))
	 (default-list (if (null? default) default (cdr default)))
	 (default-length (length default-list))
	 (number
	  (and (not (null? default))
	       (let ((option (car default)))
		 (or (and (integer? option)
			  (or (and (> rest-length (abs option))
				   (if (string? caller)
				       (error caller rest-list 'rest-list
					      `(<= (length rest-list)
						   ,(abs option)))
				       (apply error "too many arguments"
					      rest-list 'rest-list
					      `(<= (length rest-list)
						   ,(abs option))
					      caller)))
			      (and (> default-length (abs option))
				   (if (string? caller)
				       (error caller default-list
					      'default-list
					      `(<= (length default-list)
						   ,(abs option)))
				       (apply error "too many defaults"
					      default-list 'default-list
					      `(<= (length default-list)
						   ,(abs option))
					      caller)))
			      option))
		     (eq? option #t)
		     (and (not option) 'false)
		     (and (eq? option +) +)
		     (and (eq? option -) -)
		     (if (string? caller)
			 (error caller option 'option
				'(or (boolean? option)
				     (integer? option)
				     (memq option (list + -))))
			 (apply error "bad optional argument" option 'option
				'(or (boolean? option)
				     (integer? option)
				     (memq option (list + -)))
				caller)))))))
    (cond
     ((or (eq? #t number) (eq? 'false number))
      (and (not (every pair? default-list))
	   (if (string? caller)
	       (error caller default-list 'default-list
		      '(every pair? default-list))
	       (apply error "bad default list" default-list 'default-list
		      '(every pair? default-list) caller)))
      (let loop ((rest-list rest-list)
		 (default-list default-list)
		 (result '()))
	(if (null? default-list)
	    (if (null? rest-list)
		(apply values (reverse result))
		(if (eq? #t number)
		    (if (string? caller)
			(error caller rest-list 'rest-list '(null? rest-list))
			(apply error "bad argument" rest-list 'rest-list
			       '(null? rest-list) caller))
		    (apply values (append-reverse result rest-list))))
	    (if (null? rest-list)
		(apply values (append-reverse result (map car default-list)))
		(let ((default (car default-list)))
		  (let lp ((rest rest-list)
			   (head '()))
		    (if (null? rest)
			(loop (reverse head)
			      (cdr default-list)
			      (cons (car default) result))
			(if (list? default)
			    (if (member (car rest) default)
				(loop (append-reverse head (cdr rest))
				      (cdr default-list)
				      (cons (car rest) result))
				(lp (cdr rest) (cons (car rest) head)))
			    (if ((cdr default) (car rest))
				(loop (append-reverse head (cdr rest))
				      (cdr default-list)
				      (cons (car rest) result))
				(lp (cdr rest) (cons (car rest) head)))))))))))
     ((or (and (integer? number) (> number 0))
	  (eq? number +))
      (and (not (every pair? default-list))
	   (if (string? caller)
	       (error caller default-list 'default-list
		      '(every pair? default-list))
	       (apply error "bad default list" default-list 'default-list
		      '(every pair? default-list) caller)))
      (let loop ((rest rest-list)
		 (default default-list))
	(if (or (null? rest) (null? default))
	    (apply values
		   (if (> default-length rest-length)
		       (append rest-list
			       (map car (list-tail default-list rest-length)))
		       rest-list))
	    (let ((arg (car rest))
		  (par (car default)))
	      (if (list? par)
		  (if (member arg par)
		      (loop (cdr rest) (cdr default))
		      (if (string? caller)
			  (error caller arg 'arg `(member arg ,par))
			  (apply error "unmatched argument"
				 arg 'arg `(member arg ,par) caller)))
		  (if ((cdr par) arg)
		      (loop (cdr rest) (cdr default))
		      (if (string? caller)
			  (error caller arg 'arg `(,(cdr par) arg))
			  (apply error "incorrect argument"
				 arg 'arg `(,(cdr par) arg) caller))))))))
     (else
      (apply values (if (> default-length rest-length)
			(append rest-list (list-tail default-list rest-length))
			rest-list))))))
