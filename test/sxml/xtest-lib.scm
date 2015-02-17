

; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky

;==============================================================================
; Test generator doesn't not use any external code for its own purpose in order
; to avoid any interference with tested code.
; Some service functions provided in this section are renamed using "xtest-"
; prefix for the sake of name-clash prevention.
; NOTE!!!! Exceptions: cout, cerr, nl, pp

; Just a simplified SRFI-1 'filter'.
;@license
; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
; this code as long as you do not remove this copyright notice or
; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;     -Olin 
(define (xtest-filter pred lis)			
  (let recur ((lis lis))		
    (if (not (pair? lis)) lis			
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail)))	; Replicate the RECUR call so
		(if (eq? tail new-tail) lis
		    (cons head new-tail)))
	      (recur tail))))))			; this one can be a tail call.


; Borrowed from libmisc.scm
; Like pp, but symbols are quoted
(define (xtest-ppw obj . port)
  (let ((port (if (null? port) (current-output-port) (car port))))
    (begin
      (and (symbol? obj)
	   (display "'" port))
      (pp obj port))))
