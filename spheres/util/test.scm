;; Copyright (c) 2005, 2006, 2007, 2012 Per Bothner
;; Modified for Scheme Spheres by Álvaro Castro-Castilla, Copyright (c) 2012
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-syntax %test-record-define
  (syntax-rules ()
    ((%test-record-define alloc runner? (name index setter getter) ...)
     (define-record-type test-runner
       (alloc)
       runner?
       (name setter getter) ...))))

(%test-record-define
 %test-runner-alloc test-runner?
 ;; Cumulate count of all tests that have passed and were expected to.
 (pass-count 1 test-runner-pass-count test-runner-pass-count!)
 (fail-count 2 test-runner-fail-count test-runner-fail-count!)
 (xpass-count 3 test-runner-xpass-count test-runner-xpass-count!)
 (xfail-count 4 test-runner-xfail-count test-runner-xfail-count!)
 (skip-count 5 test-runner-skip-count test-runner-skip-count!)
 (skip-list 6 %test-runner-skip-list %test-runner-skip-list!)
 (fail-list 7 %test-runner-fail-list %test-runner-fail-list!)
 ;; Normally #t, except when in a test-apply.
 (run-list 8 %test-runner-run-list %test-runner-run-list!)
 (skip-save 9 %test-runner-skip-save %test-runner-skip-save!)
 (fail-save 10 %test-runner-fail-save %test-runner-fail-save!)
 (group-stack 11 test-runner-group-stack test-runner-group-stack!)
 (on-test-begin 12 test-runner-on-test-begin test-runner-on-test-begin!)
 (on-test-end 13 test-runner-on-test-end test-runner-on-test-end!)
 ;; Call-back when entering a group. Takes (runner suite-name count).
 (on-group-begin 14 test-runner-on-group-begin test-runner-on-group-begin!)
 ;; Call-back when leaving a group.
 (on-group-end 15 test-runner-on-group-end test-runner-on-group-end!)
 ;; Call-back when leaving the outermost group.
 (on-final 16 test-runner-on-final test-runner-on-final!)
 ;; Call-back when expected number of tests was wrong.
 (on-bad-count 17 test-runner-on-bad-count test-runner-on-bad-count!)
 ;; Call-back when name in test=end doesn't match test-begin.
 (on-bad-end-name 18 test-runner-on-bad-end-name test-runner-on-bad-end-name!)
 ;; Cumulate count of all tests that have been done.
 (total-count 19 %test-runner-total-count %test-runner-total-count!)
 ;; Stack (list) of (count-at-start . expected-count):
 (count-list 20 %test-runner-count-list %test-runner-count-list!)
 (result-alist 21 test-result-alist test-result-alist!)
 ;; Field can be used by test-runner for any purpose.
 ;; test-runner-simple uses it for a log file.
 (aux-value 22 test-runner-aux-value test-runner-aux-value!))

;;! test-runner-reset
(define (test-runner-reset runner)
    (test-result-alist! runner '())
    (test-runner-pass-count! runner 0)
    (test-runner-fail-count! runner 0)
    (test-runner-xpass-count! runner 0)
    (test-runner-xfail-count! runner 0)
    (test-runner-skip-count! runner 0)
    (%test-runner-total-count! runner 0)
    (%test-runner-count-list! runner '())
    (%test-runner-run-list! runner #t)
    (%test-runner-skip-list! runner '())
    (%test-runner-fail-list! runner '())
    (%test-runner-skip-save! runner '())
    (%test-runner-fail-save! runner '())
    (test-runner-group-stack! runner '()))

;;! test-runner-group-path
(define (test-runner-group-path runner)
  (reverse (test-runner-group-stack runner)))

(define (%test-null-callback runner) #f)

;;! test-runner-null
(define (test-runner-null)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner (lambda (runner name count) #f))
    (test-runner-on-group-end! runner %test-null-callback)
    (test-runner-on-final! runner %test-null-callback)
    (test-runner-on-test-begin! runner %test-null-callback)
    (test-runner-on-test-end! runner %test-null-callback)
    (test-runner-on-bad-count! runner (lambda (runner count expected) #f))
    (test-runner-on-bad-end-name! runner (lambda (runner begin end) #f))
    runner))

;;! Controls whether a log file is generated.
;; Not part of the specification.
(define test-log-to-file #f)

;;! test-runner-simple
(define (test-runner-simple)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner test-on-group-begin-simple)
    (test-runner-on-group-end! runner test-on-group-end-simple)
    (test-runner-on-final! runner test-on-final-simple)
    (test-runner-on-test-begin! runner test-on-test-begin-simple)
    (test-runner-on-test-end! runner test-on-test-end-simple)
    (test-runner-on-bad-count! runner test-on-bad-count-simple)
    (test-runner-on-bad-end-name! runner test-on-bad-end-name-simple)
    runner))

(define test-runner-current (make-parameter #f))
(define test-runner-factory (make-parameter test-runner-simple))

;;! A safer wrapper to test-runner-current.
(define (test-runner-get)
  (let ((r (test-runner-current)))
    (if (not r)
	(error "test-runner not initialized - test-begin missing?"))
    r))

(define (%test-specificier-matches spec runner)
  (spec runner))

;;! test-runner-create
(define (test-runner-create)
  ((test-runner-factory)))

(define (%test-any-specifier-matches list runner)
  (let ((result #f))
    (let loop ((l list))
      (cond ((null? l) result)
	    (else
	     (if (%test-specificier-matches (car l) runner)
		 (set! result #t))
	     (loop (cdr l)))))))

;; Returns #f, #t, or 'xfail.
(define (%test-should-execute runner)
  (let ((run (%test-runner-run-list runner)))
    (cond ((or
	    (not (or (eqv? run #t)
		     (%test-any-specifier-matches run runner)))
	    (%test-any-specifier-matches
	     (%test-runner-skip-list runner)
	     runner))
           (test-result-set! runner 'result-kind 'skip)
           #f)
	  ((%test-any-specifier-matches
	    (%test-runner-fail-list runner)
	    runner)
	   (test-result-set! runner 'result-kind 'xfail)
	   'xfail)
	  (else #t))))

(define (%test-begin suite-name count)
  (if (not (test-runner-current))
      (test-runner-current (test-runner-create)))
  (let ((runner (test-runner-current)))
    ((test-runner-on-group-begin runner) runner suite-name count)
    (%test-runner-skip-save! runner
                             (cons (%test-runner-skip-list runner)
                                   (%test-runner-skip-save runner)))
    (%test-runner-fail-save! runner
                             (cons (%test-runner-fail-list runner)
                                   (%test-runner-fail-save runner)))
    (%test-runner-count-list! runner
                              (cons (cons (%test-runner-total-count runner)
                                          count)
                                    (%test-runner-count-list runner)))
    (test-runner-group-stack! runner (cons suite-name
                                           (test-runner-group-stack runner)))))

;;! test-on-group-begin-simple
(define (test-on-group-begin-simple runner suite-name count)
  (if (null? (test-runner-group-stack runner))
      (begin
	(display "%%%% Starting test ")
	(display suite-name)
	(if test-log-to-file
	    (let* ((log-file-name
		    (if (string? test-log-to-file) test-log-to-file
			(string-append suite-name ".log")))
		   (log-file
		    (open-output-file log-file-name)))
	      (display "%%%% Starting test " log-file)
	      (display suite-name log-file)
	      (newline log-file)
	      (test-runner-aux-value! runner log-file)
	      (display "  (Writing full log to \"")
	      (display log-file-name)
	      (display "\")")))
	(newline)))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(begin
	  (display "Group begin: " log)
	  (display suite-name log)
	  (newline log))))
  #f)

;;! test-on-group-end-simple
(define (test-on-group-end-simple runner)
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(begin
	  (display "Group end: " log)
	  (display (car (test-runner-group-stack runner)) log)
	  (newline log))))
  #f)

(define (%test-on-bad-count-write runner count expected-count port)
  (display "*** Total number of tests was " port)
  (display count port)
  (display " but should be " port)
  (display expected-count port)
  (display ". ***" port)
  (newline port)
  (display "*** Discrepancy indicates testsuite error or exceptions. ***" port)
  (newline port))

;;! test-on-bad-count-simple
(define (test-on-bad-count-simple runner count expected-count)
  (%test-on-bad-count-write runner count expected-count (current-output-port))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(%test-on-bad-count-write runner count expected-count log))))

;;! test-on-bad-end-name-simple
(define (test-on-bad-end-name-simple runner begin-name end-name)
  (let ((msg (string-append (%test-format-line runner) "test-end " begin-name
			    " does not match test-begin " end-name)))
    (error msg)))
  
(define (%test-final-report1 value label port)
  (if (> value 0)
      (begin
	(display label port)
	(display value port)
	(newline port))))

(define (%test-final-report-simple runner port)
  (%test-final-report1 (test-runner-pass-count runner)
                       "# of expected passes      " port)
  (%test-final-report1 (test-runner-xfail-count runner)
                       "# of expected failures    " port)
  (%test-final-report1 (test-runner-xpass-count runner)
                       "# of unexpected successes " port)
  (%test-final-report1 (test-runner-fail-count runner)
                       "# of unexpected failures  " port)
  (%test-final-report1 (test-runner-skip-count runner)
                       "# of skipped tests        " port))

;;! test-on-final-simple
(define (test-on-final-simple runner)
  (%test-final-report-simple runner (current-output-port))
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(%test-final-report-simple runner log))))

(define (%test-format-line runner)
  (let* ((line-info (test-result-alist runner))
         (source-file (assq 'source-file line-info))
         (source-line (assq 'source-line line-info))
         (file (if source-file (cdr source-file) "")))
    (if source-line
        (string-append file ":"
                       (number->string (cdr source-line)) ": ")
        "")))

(define (%test-end suite-name line-info)
  (let* ((r (test-runner-get))
	 (groups (test-runner-group-stack r))
	 (line (%test-format-line r)))
    (test-result-alist! r line-info)
    (if (null? groups)
	(let ((msg (string-append line "test-end not in a group")))
	  (cond-expand
	   (srfi-23 (error msg))
	   (else (display msg) (newline)))))
    (if (and suite-name (not (equal? suite-name (car groups))))
	((test-runner-on-bad-end-name r) r suite-name (car groups)))
    (let* ((count-list (%test-runner-count-list r))
	   (expected-count (cdar count-list))
	   (saved-count (caar count-list))
	   (group-count (- (%test-runner-total-count r) saved-count)))
      (if (and expected-count
	       (not (= expected-count group-count)))
	  ((test-runner-on-bad-count r) r group-count expected-count))
      ((test-runner-on-group-end r) r)
      (test-runner-group-stack! r (cdr (test-runner-group-stack r)))
      (%test-runner-skip-list! r (car (%test-runner-skip-save r)))
      (%test-runner-skip-save! r (cdr (%test-runner-skip-save r)))
      (%test-runner-fail-list! r (car (%test-runner-fail-save r)))
      (%test-runner-fail-save! r (cdr (%test-runner-fail-save r)))
      (%test-runner-count-list! r (cdr count-list))
      (if (null? (test-runner-group-stack r))
	  ((test-runner-on-final r) r)))))

;;! test-on-test-begin-simple
(define (test-on-test-begin-simple runner)
  (let ((log (test-runner-aux-value runner)))
    (if (output-port? log)
	(let* ((results (test-result-alist runner))
	       (source-file (assq 'source-file results))
	       (source-line (assq 'source-line results))
	       (source-form (assq 'source-form results))
	       (test-name (assq 'test-name results)))
	  (display "Test begin:" log)
	  (newline log)
	  (if test-name (%test-write-result1 test-name log))
	  (if source-file (%test-write-result1 source-file log))
	  (if source-line (%test-write-result1 source-line log))
	  (if source-file (%test-write-result1 source-form log))))))

;;! test-on-test-end-simple
(define (test-on-test-end-simple runner)
  (let ((log (test-runner-aux-value runner))
	(kind (test-result-ref runner 'result-kind)))
    (if (memq kind '(fail xpass))
	(let* ((results (test-result-alist runner))
	       (source-file (assq 'source-file results))
	       (source-line (assq 'source-line results))
	       (test-name (assq 'test-name results)))
	  (if (or source-file source-line)
	      (begin
		(if source-file (display (cdr source-file)))
		(display ":")
		(if source-line (display (cdr source-line)))
		(display ": ")))
          (display "\033[00;31m") ;Display FAIL and XPASS in red
	  (display (if (eq? kind 'xpass) "XPASS" "FAIL"))
          (display "\033[00m") ;End color display
	  (if test-name
	      (begin
		(display " ")
		(display (cdr test-name))))
	  (newline)))
    (if (output-port? log)
	(begin
	  (display "Test end:" log)
	  (newline log)
	  (let loop ((list (test-result-alist runner)))
	    (if (pair? list)
		(let ((pair (car list)))
		  ;; Write out properties not written out by on-test-begin.
		  (if (not (memq (car pair)
				 '(test-name source-file source-line source-form)))
		      (%test-write-result1 pair log))
		  (loop (cdr list)))))))))

(define (%test-write-result1 pair port)
  (display "  " port)
  (display (car pair) port)
  (display ": " port)
  (write (cdr pair) port)
  (newline port))

;;! test-result-set!
(define (test-result-set! runner pname value)
  (let* ((alist (test-result-alist runner))
	 (p (assq pname alist)))
    (if p
	(set-cdr! p value)
	(test-result-alist! runner (cons (cons pname value) alist)))))

;;! test-result-clear
(define (test-result-clear runner)
  (test-result-alist! runner '()))

;;! test-result-remove
(define (test-result-remove runner pname)
  (let* ((alist (test-result-alist runner))
	 (p (assq pname alist)))
    (if p
	(test-result-alist! runner
                            (let loop ((r alist))
                              (if (eq? r p) (cdr r)
                                  (cons (car r) (loop (cdr r)))))))))

;;! test-result-kind
(define (test-result-kind . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-current))))
    (test-result-ref runner 'result-kind)))

;;! test-passed?
(define (test-passed? . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-get))))
    (memq (test-result-ref runner 'result-kind) '(pass xpass))))

(define (%test-report-result)
  (let* ((r (test-runner-get))
	 (result-kind (test-result-kind r)))
    (case result-kind
      ((pass)
       (test-runner-pass-count! r (+ 1 (test-runner-pass-count r))))
      ((fail)
       (test-runner-fail-count!	r (+ 1 (test-runner-fail-count r))))
      ((xpass)
       (test-runner-xpass-count! r (+ 1 (test-runner-xpass-count r))))
      ((xfail)
       (test-runner-xfail-count! r (+ 1 (test-runner-xfail-count r))))
      (else
       (test-runner-skip-count! r (+ 1 (test-runner-skip-count r)))))
    (%test-runner-total-count! r (+ 1 (%test-runner-total-count r)))
    ((test-runner-on-test-end r) r)))

;; Kept as reference. %test-source-line2 not implemented
;; (cond-expand
;;    (mzscheme
;;     (define-for-syntax (%test-syntax-file form)
;;       (let ((source (syntax-source form)))
;; 	(cond ((string? source) file)
;; 				((path? source) (path->string source))
;; 				(else #f)))))
;;    (kawa
;;     (define (%test-syntax-file form)
;;       (syntax-source form))))
(define (%test-source-line2 form) '())

(define (%test-on-test-begin r)
  (%test-should-execute r)
  ((test-runner-on-test-begin r) r)
  (not (eq? 'skip (test-result-ref r 'result-kind))))

(define (%test-on-test-end r result)
  (test-result-set! r 'result-kind
                    (if (eq? (test-result-ref r 'result-kind) 'xfail)
                        (if result 'xpass 'xfail)
                        (if result 'pass 'fail))))

;;! test-runner-test-name
(define (test-runner-test-name runner)
  (test-result-ref runner 'test-name ""))

(define (%test-approximate= error)
  (lambda (value expected)
    (let ((rval (real-part value))
          (ival (imag-part value))
          (rexp (real-part expected))
          (iexp (imag-part expected)))
      (and (>= rval (- rexp error))
           (>= ival (- iexp error))
           (<= rval (+ rexp error))
           (<= ival (+ iexp error))))))

;;!! Predicates

;;! test-apply
(define (test-apply first . rest)
  (define (reverse! lis)
    (let lp ((lis lis) (ans '()))
      (if (null? lis) ans
          (let ((tail (cdr lis)))
            (set-cdr! lis ans)
            (lp tail lis)))))
  (if (test-runner? first)
      (test-with-runner first (apply test-apply rest))
      (let ((r (test-runner-current)))
	(if r
	    (let ((run-list (%test-runner-run-list r)))
	      (cond ((null? rest)
		     (%test-runner-run-list! r (reverse! run-list))
		     (first)) ;; actually apply procedure thunk
		    (else
		     (%test-runner-run-list!
		      r
		      (if (eq? run-list #t) (list first) (cons first run-list)))
		     (apply test-apply rest)
		     (%test-runner-run-list! r run-list))))
	    (let ((r (test-runner-create)))
	      (test-with-runner r (apply test-apply first rest))
	      ((test-runner-on-final r) r))))))

(define (%test-match-nth n count)
  (let ((i 0))
    (lambda (runner)
      (set! i (+ i 1))
      (and (>= i n) (< i (+ n count))))))

(define (%test-match-all . pred-list)
  (lambda (runner)
    (let ((result #t))
      (let loop ((l pred-list))
	(if (null? l)
	    result
	    (begin
	      (if (not ((car l) runner))
		  (set! result #f))
	      (loop (cdr l))))))))
  
(define (%test-match-any . pred-list)
  (lambda (runner)
    (let ((result #f))
      (let loop ((l pred-list))
	(if (null? l)
	    result
	    (begin
	      (if ((car l) runner)
		  (set! result #t))
	      (loop (cdr l))))))))

;; Coerce to a predicate function:
(define (%test-as-specifier specifier)
  (cond ((procedure? specifier) specifier)
	((integer? specifier) (test-match-nth 1 specifier))
	((string? specifier) (test-match-name specifier))
	(else
	 (error "not a valid test specifier"))))

(define (test-match-name name)
  (lambda (runner)
    (equal? name (test-runner-test-name runner))))

(define (test-read-eval-string string)
  (let* ((port (open-input-string string))
	 (form (read port)))
    (if (eof-object? (read-char port))
	(eval form)
	(error "(not at eof)"))))
