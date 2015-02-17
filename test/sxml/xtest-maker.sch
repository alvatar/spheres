;; XP-style testing framework
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;==============================================================================
; Testing tools

; Makes a test case for `(,selector ,@params)
(define-macro (xtest-make selector . params)
	      `(cons 
		 (cond ((pair? ',selector) 
			(car ',selector))
		       (else ',selector))
		 (delay
		   (begin
		     (cout nl "; " ',selector nl 
			   "(xtest-assert ; Expected result:" nl "'")
		     (pp (,selector ,@params))
		     (cout "; <--- of:" nl ',selector nl "'")
		     (for-each xtest-ppw 
			       (list ,@params))
		     (cout ")" nl)))
		 ))


;=========================================================================
; DL:

(define-macro (xtest-quote selector . params)
  `(cons 
    (cond ((pair? ',selector) 
           (car ',selector))
          (else ',selector))
    (delay
      (begin
        (cout nl "; " ',selector nl 
              "(xtest-assert ; Expected result:" nl "'")
        (pp (,selector ,@params))
        (cout "; <--- of:" nl)
        (pp ',selector)
        (for-each pp  ; xtest-ppw
                  ,(list 'quote params))
        (cout ")" nl)))))


(define-macro 
  (xtest-line txt)
  `(cons 
    'smth
    (delay
      (begin
        (cout nl nl ";------------------------------------------------")
	(cout nl "; " ',txt nl)))))


(define-macro 
  (xtest-solid-line txt)
  `(cons 
    'smth
    (delay
      (begin
        (cout nl nl ";=========================================================================")
	(cout nl "; " ',txt nl)))))
