;; Copyright (c) 2007-2011, Florian Loitsch
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;    * Neither the name of the <organization> nor the
;;      names of its contributors may be used to endorse or promote products
;;      derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module utf-init-gen
   (include "utf-category-data.sch")
   (main main))

(define *utf-file* "utf.scm")

(define (untyped sym)
   (let* ((str (symbol->string sym))
	  (pos (string-index str #\:)))
      (string->symbol (if pos
			  (substring str 0 pos)
			  str))))

(define (find-def sym)
   (let ((p (open-input-file *utf-file*)))
      (unwind-protect
	 (let loop ()
	    (let ((expr (read p)))
	       (match-case expr
		  ((define (?name . ?-) . ?-)
		   (if (eq? (untyped name) sym)
		       expr
		       (loop)))
		  ((define-inline (?name . ?-) . ?-)
		   (if (eq? (untyped name) sym)
		       expr
		       (loop)))
		  ((? eof-object?)
		   (error 'utf-init-gen
			  "could not find expr"
			  sym))
		  (else (loop)))))
	 (close-input-port p))))

(define (replacement-symbol? expr)
   (and (symbol? expr)
	(let* ((str (symbol->string expr))
	       (len (string-length str)))
	   (and (>fx len 4)
		(char=? #\* (string-ref str 0))
		(char=? #\< (string-ref str 1))
		(char=? #\* (string-ref str (-fx len 1)))
		(char=? #\> (string-ref str (-fx len 2)))))))

(define (replace sym)
   (case sym
      ((*<categories>*) (list 'quote *categories*))
      ((*<category-ranges>*) (list 'quote *category-ranges*))
      (else
       (let* ((str (symbol->string sym))
	      (len (string-length str))
	      (unmangled (substring str 2 (-fx len 2)))
	      (target (string->symbol unmangled)))
	  (find-def target)))))

(define (sed-pair expr)
   (set-car! expr (sed (car expr)))
   (set-cdr! expr (sed (cdr expr)))
   expr)

(define (sed expr)
   (cond
      ((pair? expr) (sed-pair expr))
      ((replacement-symbol? expr)
       (replace expr))
      (else expr)))
	    
(define (sed-file in-p out-p)
   (let loop ()
      (let ((expr (read in-p)))
	 (unless (eof-object? expr)
	    (pp (sed expr) out-p)
	    (loop)))))

(define *in-file* #f)
(define *out-file* #f)

(define (main args)
   (args-parse (cdr args)
      (("-o" ?file)
       (set! *out-file* file))
      (else (set! *in-file* else)))
   (when (not (and *in-file* *out-file*))
      (with-output-to-port (current-error-port)
	 (lambda ()
	    (print "Usage: " (car args) " -o out-f in-f")
	    (exit -1))))
   (let ((in-p (open-input-file *in-file*))
	 (out-p (open-output-file *out-file*)))
      (unwind-protect
	 (begin
	    (with-output-to-port out-p
	       (lambda ()
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")
		  (print ";; GENERATED FILE ---- DON'T EDIT")))
	    (sed-file in-p out-p))
	 (begin
	    (close-input-port in-p)
	    (close-output-port out-p))))
   0)
