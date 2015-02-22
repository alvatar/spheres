;;!!! Objective-C Utility Macros for Gambit Scheme
;; .author Jeffrey T. Read, March 19, 2011
;;
;; This software is licensed under the WTFPL.
;;
;; These were developed while I was working on the Gambit Gaming
;; Engine (GGE). They should be helpful in easily wrapping existing
;; Objective-C APIs with Gambit Scheme.
;;
;;
;; somewhere in your Gambit source file.
;;
;; The API is as follows:
;;
;; (objc-method class-name (formaltype1 ...) return-type method-name)
;;
;;   Creates a `c-lambda' that wraps an invocation of method
;;   `method-name' to objects of class `class-name'. So for example if
;;   you had:
;;
;;   @class Barney;
;;
;;   @interface Fred
;;   { ... }
;;
;;   -(int)frobWithBarney: (Barney *)aBarney wearFunnyHats: (BOOL) hats;
;;   +instanceNumber: (int) n
;;   @end
;;
;;   you could wrap the frobWithBarney method with something like the
;;   following:
;;
;;   (define frob-with-barney
;;    (objc-method "Fred" ((pointer "Barney") bool) int
;;                 "frobWithBarney:wearFunnyHats:"))
;;
;;   Then if Scheme-side you had a pointer to Fred `f' and a pointer
;;   to Barney `b' you could call from Scheme:
;;
;;   (frob-with-barney f b #t)
;;
;;   Procedures which wrap Objective-C methods in this way take one
;;   additional argument to the ones accounted for in their formals
;;   list. Their first argument should be a pointer to the object on
;;   which the method is invoked, followed by the arguments in the
;;   formals list, as in the example above which takes a pointer to
;;   Fred, a pointer to Barney, and a boolean value.
;;
;; (objc-class-method class-name (formaltype1 ...) return-type method-name)
;;
;;   Creates a `c-lambda' that wraps an invocation of class method
;;   `method-name' in class `class-name'. For instance, in class Fred
;;   above you could wrap the class method instanceNumber with the following:
;;
;;   (define fred-instance-number
;;    (objc-class-method "Fred" (int) (pointer Fred) "instanceNumber:"))
;;
;;   Then Scheme-side you could get a pointer to Fred with a call like:
;;
;;   (fred-instance-number 5)
;;
;;   Procedures which wrap Objective-C class methods in this way take
;;   only the arguments accounted for in their formals list.

(define-macro (%%objc-method class-name class? formal-types return-type method-name)
  (define (parse-method-name m)
    (define (split-at-colon s)
      (let ((l (string-length s)))
	(call-with-current-continuation
	 (lambda (k)
	   (do ((i 0 (+ i 1)))
	       ((>= i l) #f)
	     (if (char=? (string-ref s i) #\:)
		 (k (cons (substring s 0 (+ i 1))
			  (substring s (+ i 1) l)))))))))
    (define (parse-method-name1 m acc)
      (let ((p (split-at-colon m)))
	(if (not p)
	    (if (null? acc) (cons m acc) acc)
	    (parse-method-name1 (cdr p) (cons (car p) acc)))))
    (reverse (parse-method-name1 m '())))
  (define (make-methodcall lst start)
    (if (and (= (length lst) 1)
	     (not (char=? (string-ref
			   (car lst)
			   (- (string-length (car lst)) 1))
			  #\:)))
	(car lst)
	(do ((i start (+ i 1))
	     (l lst (cdr l))
	     (s ""
		(string-append s
			       (car l)
			       " ___arg"
			       (number->string i)
			       " ")))
	    ((null? l) s))))
  (let* ((res (cond
	       ((list? return-type)
		"___result_voidstar = (void *)")
	       ((eq? return-type 'void) "")
	       (else "___result = ")))
	 (methodparts (parse-method-name method-name)))
    `(c-lambda ,(if class? formal-types (cons (list 'pointer class-name) formal-types)) ,return-type
               ,(string-append
                 (if class?
                     (string-append res "[" class-name " ")
                     (string-append res "[___arg1 "))
                 (make-methodcall methodparts (if class? 1 2))
                 "];"))))

(define-macro (objc-method class-name formal-types return-type method-name)
  `(%%objc-method ,class-name #f ,formal-types ,return-type ,method-name))

(define-macro (objc-class-method class-name formal-types return-type method-name)
  `(%%objc-method ,class-name #t ,formal-types ,return-type ,method-name))
