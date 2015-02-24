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

(module utf
   (include "utf-data.sch" "utf-category-data.sch")
   (export (inline utf8-char-length::long c::long)
	   (utf8-uc-iterator str::bstring)
	   (open-utf8-string-buffer size)
	   (utf8-buffer-uc-push! buf c::long)
	   (utf8-buffer-verbatim-push! buf c::char)
	   (close-utf8-buffer::bstring buf)
	   (inline utf16-char-length::long c::long)
	   (utf16-uc-iterator str::ucs2string)
	   (open-utf16-string-buffer size)
	   (utf16-buffer-uc-push! buf c::long)
	   (utf16-buffer-verbatim-push! buf c::ucs2)
	   (close-utf16-buffer::ucs2string buf)
	   (utf8-string->utf16-string::ucs2string str::bstring)
	   (utf16-string->utf8-string::bstring str::ucs2string)
	   (utf8-string-ref str::bstring i::long)
	   (utf8-string-set!::long str::bstring i::long val::long)
	   (utf16-string-ref str::ucs2string i::long)
	   (utf16-string-set!::long str::ucs2string i::long val::long)
	   ;; uc-lower, uc-upper, uc-title return either the corresponding
	   ;; char, or a negative long if the char is special-cased.
	   ;; the special-cased chars can then be retrieved using uc-special.
	   (uc-lower::long c::long)
	   (uc-upper::long c::long)
	   (uc-title::long c::long)
	   (uc-special::pair c::long)
	   (utf8-downcase::bstring str::bstring #!key (always-make-copy? #f) (ignore-error? #t))
	   (utf8-upcase::bstring str::bstring #!key (always-make-copy? #f) (ignore-error? #t))
	   (utf8-titlecase::bstring str::bstring #!key (always-make-copy? #f) (ignore-error? #t))
	   (utf16-downcase::ucs2string str::ucs2string #!key (always-make-copy? #f) (ignore-error? #t))
	   (utf16-upcase::ucs2string str::ucs2string #!key (always-make-copy? #f) (ignore-error? #t))
	   (utf16-titlecase::ucs2string str::ucs2string #!key (always-make-copy? #f) (ignore-error? #t))
	   (unchecked-uc-lower::long c::long)
	   (unchecked-uc-upper::long c::long)
	   (unchecked-uc-title::long c::long)
	   (uc-whitespace-chars::pair)
	   (uc-whitespace?::bool c::long)
	   (uc-lineterminator?::bool c::long)
	   (uc-categories::vector)
	   (uc-category-ranges::vector))) ;; for each category a list of unique codepoints or ranges (from to) (inclusive)

(define-macro (+fx+ x . L)
   (if (null? L)
       x
       `(+fx ,x (+fx+ ,@L))))

(define-inline (utf8-char-length::long c::long)
   (cond
      ((<=fx c #x7F) 1)
      ((<=fx c #x7FF) 2)
      ((<=fx c #xFFFF) 3)
      (else 4)))

;; returns -1 if this is a bad unicode-sequence.
(define (utf8-string-ref-no-error str::bstring i::long)
   (define (&<< x and-v shift-by)
      (bit-lsh (bit-and x and-v) shift-by))

   (define (get-safe-surrogate str j)
      (if (=fx j (string-length str))
	  -1
	  (let ((ci (char->integer (string-ref str j))))
	     (if (not (=fx #x80 (bit-and #xC0 ci)))
		 -1
		 ci))))

   (when (>=fx i (string-length str))
      (error "utf8-string-ref"
	     "index out of bounds"
	     #f))
   (let ((ci (char->integer (string-ref str i))))
      (cond
	 ((<fx ci 128)
	  ;; ascii-char.
	  (values ci (+fx i 1)))
	 ((=fx #xF0 (bit-and #xF8 ci))
	  ;; 4 char encoding
	  (let* ((b4 (get-safe-surrogate str (+fx i 3)))
		 (b3 (get-safe-surrogate str (+fx i 2)))
		 (b2 (get-safe-surrogate str (+fx i 1))))
	     (if (or (=fx -1 b2) (=fx -1 b3) (=fx -1 b4))
		 (values -1 (+fx i 1))
		 (values (+fx+ (&<< ci #x07 18)
			       (&<< b2 #x3F 12)
			       (&<< b3 #x3F 6)
			       (&<< b4 #x3F 0))
			 (+fx i 4)))))
	 ((=fx #xE0 (bit-and #xF0 ci))
	  ;; 3 char encoding
	  (let* ((b3 (get-safe-surrogate str (+fx i 2)))
		 (b2 (get-safe-surrogate str (+fx i 1))))
	     (if (or (=fx -1 b2) (=fx -1 b3))
		 (values -1 (+fx i 1))
		 (values (+fx+ (&<< ci #x0F 12)
			       (&<< b2 #x3F 6)
			       (&<< b3 #x3F 0))
			 (+fx i 3)))))
	 ((=fx #xC0 (bit-and #xE0 ci))
	  (let ((b (get-safe-surrogate str (+fx i 1))))
	     ;; 2 char encoding
	     (if (=fx -1 b)
		 (values -1 (+fx i 1))
		 (values (+fx (&<< ci #x1F 6)
			      (bit-and #x3F b))
			 (+fx i 2)))))
	 (else
	  -1))))
   
;; i must be < str-len.
(define (utf8-string-ref str::bstring i::long)
   (receive (c new-i)
      (utf8-string-ref-no-error str i)
      (if (=fx -1 c)
	  (error 'utf8-string-ref
		 "bad unicode-sequence"
		 str)
	  (values c new-i))))

;; returns the next "free" 'i'.
(define (utf8-string-set! str i c)
   (define (string-byte-set! str i c)
      (string-set! str i (integer->char c)))

   ;; in theory we do not need to test ourselves. Bigloo is doing this anyways.
   (when (>fx (+fx i (utf8-char-length c))
	      (string-length str))
      (error "utf8-string-set!"
	     "out of bounds"
	     i))
   (cond
      ((<=fx c #x7F)
       (string-byte-set! str i c)
       (+fx i 1))
      ((<=fx c #x7FF)
       (let ((b1 (+fx #xC0 (bit-ursh c 6)))
	     (b2 (+fx #x80 (bit-and c #x3F))))
	  (string-byte-set! str i b1)
	  (string-byte-set! str (+fx i 1) b2)
	  (+fx i 2)))
      ((<=fx c #xFFFF)
       (let ((b1 (+fx #xE0 (bit-ursh c 12)))
	     (b2 (+fx #x80
		      (bit-and (bit-ursh c 6) #x3F)))
	     (b3 (+fx #x80 (bit-and c #x3F))))
	  (string-byte-set! str i b1)
	  (string-byte-set! str (+fx i 1) b2)
	  (string-byte-set! str (+fx i 2) b3)
	  (+fx i 3)))
      (else
       (let ((b1 (+fx #xF0 (bit-ursh c 18)))
	     (b2 (+fx #x80
		      (bit-and (bit-ursh c 12) #x3F)))
	     (b3 (+fx #x80
		      (bit-and (bit-ursh c 6) #x3F)))
	     (b4 (+fx #x80 (bit-and c #x3F))))
	  (string-byte-set! str i b1)
	  (string-byte-set! str (+fx i 1) b2)
	  (string-byte-set! str (+fx i 2) b3)
	  (string-byte-set! str (+fx i 3) b4)
	  (+fx i 4)))))
   
(define (utf8-uc-iterator str::bstring)
   (let* ((str-len (string-length str))
	  (i 0))
      (lambda ()
	 (if (>= i str-len)
	     #f ;; eos
	     (receive (uc new-i)
		(utf8-string-ref str i)
		(set! i new-i)
		uc)))))

(define (open-utf8-string-buffer size)
   (cons 0 (make-string size)))

(define (utf8-buffer-uc-push! buf c::long)
   (let ((str (cdr buf))
	 (i (car buf)))
      (set-car! buf (utf8-string-set! str i c))))

(define (utf8-buffer-verbatim-push! buf c::char)
   (let ((str (cdr buf))
	 (i (car buf)))
      (string-set! str i c)
      (set-car! buf (+fx i 1))))

(define (close-utf8-buffer buf)
   (cdr buf))

(define-inline (utf16-char-length::long c::long)
   (if (>fx c #xFFFF)
       2
       1))

;; i must be < str-len
;; returns -1 when bad unicode sequence.
(define (utf16-string-ref-no-error str i)
   (define (&<< x and-v shift-by)
      (bit-lsh (bit-and x and-v) shift-by))

   (when (>=fx i (ucs2-string-length str))
      (error "utf16-string-ref"
	     "index out of bounds"
	     #f))
   (let ((ci (ucs2->integer (ucs2-string-ref str i))))
      (cond
	 ((=fx (bit-and ci #xFC00) #xD800)
	  ;; surrogate.
	  (if (=fx (+fx i 1) (ucs2-string-length str))
	      -1
	      (let* ((c2 (ucs2-string-ref str (+fx i 1)))
		     (ci2 (ucs2->integer c2)))
		 (if (not (=fx (bit-and ci2 #xFC00) #xFC00))
		     -1
		     (let ((t (+fx (bit-lsh (-fx ci #xD800) 10)
				   (-fx ci2 #xDC00))))
			(values (+fx t #x10000)
				(+fx i 2)))))))
	 ((=fx (bit-and ci #xFC00) #xDC00)
	  -1)
	 (else
	  (values ci (+fx i 1))))))
(define (utf16-string-ref str i)
   (receive (c new-i)
      (utf16-string-ref-no-error str i)
      (if (=fx c -1)
	  (error 'utf16-string-ref
		 "bad unicode sequence"
		 str)
	  (values c new-i))))

(define (utf16-string-set! str i c)
   ;; in theory we do not need to test ourselves. Bigloo is doing this anyways.
   (when (>fx (+fx i (utf16-char-length c))
	      (ucs2-string-length str))
      (error "utf16-string-set!"
	     "out of bounds"
	     i))
   (cond
      ((<fx c #xFFFF)
       ;; note: we do not verify if the given char yields an utf16
       ;; surrogate. For all we know this might be a hack to get
       ;; utf16 chars.
       ;; in other words: a valid unicode-char must not be in
       ;; range #xD800-#xDBFF or #xDC00-#xDFFF as these are used
       ;; for utf16 surrogates. We do not check (on purpose) if
       ;; the incoming unicode character is valid.
       (ucs2-string-set! str i (integer->ucs2-ur c))
       (+fx i 1))
      (else
       ;; surrogate
       (let* ((t (-fx c #x10000))
	      (p1 (+fx #xD800 (bit-rsh (bit-and t #xFFC00) 10)))
	      (p2 (+fx #xDC00 (bit-and t #x3FF))))
	  (ucs2-string-set! str i (integer->ucs2-ur p1))
	  (ucs2-string-set! str (+fx i 1) (integer->ucs2-ur p2))
	  (+fx i 2)))))
   
(define (utf16-uc-iterator str::ucs2string)
   (let* ((str-len (ucs2-string-length str))
	  (i 0))
      (lambda ()
	 (if (>=fx i str-len)
	     #f ;; eos
	     (receive (c new-i)
		(utf16-string-ref str i)
		(set! i new-i)
		c)))))

(define (open-utf16-string-buffer size)
   (cons 0 (make-ucs2-string size)))

(define (utf16-buffer-uc-push! buf c::long)
   (let ((str (cdr buf))
	 (i (car buf)))
      (set-car! buf (utf16-string-set! str i c))))

(define (utf16-buffer-verbatim-push! buf c::ucs2)
   (let ((str (cdr buf))
	 (i (car buf)))
      (ucs2-string-set! str i c)
      (set-car! buf (+fx i 1))))

(define (close-utf16-buffer buf)
   (cdr buf))

(define (utf8-string->utf16-string::ucs2string str::bstring)
   ;; start by counting the length of the resulting string
   (let loop ((i 0)
	      (target-len 0))
      (if (>=fx i (string-length str))
	  ;; we have the target-len now.
	  (let ((target-str (make-ucs2-string target-len)))
	     (let loop ((i 0)
			(j 0))
		(if (>=fx i (string-length str))
		    target-str
		    (receive (c new-i)
		       (utf8-string-ref str i)
		       (let ((new-j (utf16-string-set! target-str j c)))
			  (loop new-i new-j))))))
	  (receive (c new-i)
	     (utf8-string-ref str i)
	     (loop new-i
		   (+fx target-len (utf16-char-length c)))))))

(define (utf16-string->utf8-string::bstring str::ucs2string)
   ;; start by counting the length of the resulting string
   (let loop ((i 0)
	      (target-len 0))
      (if (>=fx i (ucs2-string-length str))
	  ;; we have the target-len now.
	  (let ((target-str (make-string target-len)))
	     (let loop ((i 0)
			(j 0))
		(if (>=fx i (ucs2-string-length str))
		    target-str
		    (receive (c new-i)
		       (utf16-string-ref str i)
		       (let ((new-j (utf8-string-set! target-str j c)))
			  (loop new-i new-j))))))
	  (receive (c new-i)
	     (utf16-string-ref str i)
	     (loop new-i
		   (+fx target-len (utf8-char-length c)))))))

(define (unchecked-uc-lower::long c::long)
   (let* ((i1 (bit-and #xFF c))
	  (i2 (bit-and #xFF (bit-rsh c 8)))
	  (i3 (bit-and #xFF (bit-rsh c 16)))
	  (tmp (vector-ref (vector-ref (vector-ref *lower-casing* i3) i2) i1)))
      (if (zerofx? tmp)
	  c
	  tmp)))
(define (unchecked-uc-upper::long c::long)
   (let* ((i1 (bit-and #xFF c))
	  (i2 (bit-and #xFF (bit-rsh c 8)))
	  (i3 (bit-and #xFF (bit-rsh c 16)))
	  (tmp (vector-ref (vector-ref (vector-ref *upper-casing* i3) i2) i1)))
      (if (zerofx? tmp)
	  c
	  tmp)))
(define (unchecked-uc-title::long c::long)
   (let* ((i1 (bit-and #xFF c))
	  (i2 (bit-and #xFF (bit-rsh c 8)))
	  (i3 (bit-and #xFF (bit-rsh c 16)))
	  (tmp (vector-ref (vector-ref (vector-ref *title-casing* i3) i2) i1)))
      (if (zerofx? tmp)
	  c
	  tmp)))

(define-inline (checked-convert-char::long c::long f::procedure)
   (cond
      ((or (<fx c 0)
	   (>fx c #x10FFFF))
       (error 'unicode-char
	      "bad unicode char"
	      c))
      (else (f c))))

(define (uc-lower::long c::long)
   (checked-convert-char c unchecked-uc-lower))
(define (uc-upper::long c::long)
   (checked-convert-char c unchecked-uc-upper))
(define (uc-title::long c::long)
   (checked-convert-char c unchecked-uc-title))

(define (uc-special c)
   (let ((len (vector-length *special-casing*))
	 (unbiased-index (-fx -1 c)))
      (when (or (>=fx c 0)
		(>=fx unbiased-index len))
	 (error 'unicode-char
		"bad special-case"
		c))
      (vector-ref *special-casing* unbiased-index)))


(define-inline (utf-string-change-case
		str change-case always-make-copy?
		make-str str-length str-ref str-set!
		utf-str-ref utf-str-set! utf-char-length)
   (define (count-len str change-case)
      (let loop ((i 0)
		 (target-len 0)
		 (identical? #t))
	 (if (=fx i (str-length str))
	     (values target-len identical?)
	     (receive (c new-i)
		(utf-str-ref str i)
		(if (=fx c -1) ;; bad unicode-sequence
		    (loop (+fx i 1)
			  (+fx target-len 1)
			  identical?)
		    (let ((cased (change-case c)))
		       (if (>=fx cased 0)
			   (loop new-i
				 (+fx (utf-char-length cased)
				      target-len)
				 (and identical? (=fx cased c)))
			   (let liip ((special (uc-special cased))
				      (char-len 0))
			      (if (null? special)
				  (loop new-i (+fx target-len char-len) #f)
				  (liip (cdr special)
					(+fx char-len
					     (utf-char-length (car special))))
				  )))))))))
   (define (copy-cased-string str target-str change-case)
      (let loop ((i 0)
		 (j 0))
	 (if (>=fx i (str-length str))
	     target-str
	     (receive (c new-i)
		(utf-str-ref str i)
		(if (=fx c -1) ;; bad unicode sequence
		    (begin
		       (str-set! target-str j (str-ref str i))
		       (loop (+fx i 1) (+fx j 1)))
		    (let ((cased (change-case c)))
		       (if (<fx cased 0)
			   (let liip ((special (uc-special cased))
				      (j j))
			      (cond
				 ((null? special)
				  (loop new-i j))
				 (else
				  (liip (cdr special)
					(utf-str-set! target-str j
						      (car special))))))
			   (let ((new-j (utf-str-set! target-str j cased)))
			      (loop new-i new-j)))))))))

   (receive (target-len identical?)
      (count-len str change-case)
      (if (and identical? (not always-make-copy?))
	  str
	  (copy-cased-string str (make-str target-len) change-case))))

(define (utf8-downcase::bstring str::bstring #!key
				(always-make-copy? #f) (ignore-error? #t))
   (if ignore-error?
       (utf-string-change-case str uc-lower always-make-copy?
			       make-string string-length string-ref string-set!
			       utf8-string-ref-no-error utf8-string-set!
			       utf8-char-length)
       (utf-string-change-case str uc-lower always-make-copy?
			       make-string string-length string-ref string-set!
			       utf8-string-ref utf8-string-set!
			       utf8-char-length)))
(define (utf8-upcase::bstring str::bstring #!key
			      (always-make-copy? #f) (ignore-error? #t))
   (if ignore-error?
       (utf-string-change-case str uc-upper always-make-copy?
			       make-string string-length string-ref string-set!
			       utf8-string-ref-no-error utf8-string-set!
			       utf8-char-length)
       (utf-string-change-case str uc-upper always-make-copy?
			       make-string string-length string-ref string-set!
			       utf8-string-ref utf8-string-set!
			       utf8-char-length)))
(define (utf8-titlecase::bstring str::bstring #!key
				 (always-make-copy? #f) (ignore-error? #t))
   (if ignore-error?
       (utf-string-change-case str uc-title always-make-copy?
			       make-string string-length string-ref string-set!
			       utf8-string-ref-no-error utf8-string-set!
			       utf8-char-length)
       (utf-string-change-case str uc-upper always-make-copy?
			       make-string string-length string-ref string-set!
			       utf8-string-ref utf8-string-set!
			       utf8-char-length)))
(define (utf16-downcase::ucs2string str::ucs2string #!key
				    (always-make-copy? #f) (ignore-error? #t))
   (if ignore-error?
       (utf-string-change-case str uc-lower always-make-copy?
			       make-ucs2-string ucs2-string-length
			       ucs2-string-ref ucs2-string-set!
			       utf16-string-ref-no-error utf16-string-set!
			       utf16-char-length)
       (utf-string-change-case str uc-lower always-make-copy?
			       make-ucs2-string ucs2-string-length
			       ucs2-string-ref ucs2-string-set!
			       utf16-string-ref utf16-string-set!
			       utf16-char-length)))
(define (utf16-upcase::ucs2string str::ucs2string #!key
				  (always-make-copy? #f) (ignore-error? #t))
   (if ignore-error?
       (utf-string-change-case str uc-upper always-make-copy?
			       make-ucs2-string ucs2-string-length
			       ucs2-string-ref ucs2-string-set!
			       utf16-string-ref-no-error utf16-string-set!
			       utf16-char-length)
       (utf-string-change-case str uc-upper always-make-copy?
			       make-ucs2-string ucs2-string-length
			       ucs2-string-ref ucs2-string-set!
			       utf16-string-ref utf16-string-set!
			       utf16-char-length)))
(define (utf16-titlecase::ucs2string str::ucs2string #!key
				     (always-make-copy? #f) (ignore-error? #t))
   (if ignore-error?
       (utf-string-change-case str uc-title always-make-copy?
			       make-ucs2-string ucs2-string-length
			       ucs2-string-ref ucs2-string-set!
			       utf16-string-ref-no-error utf16-string-set!
			       utf16-char-length)
       (utf-string-change-case str uc-title always-make-copy?
			       make-ucs2-string ucs2-string-length
			       ucs2-string-ref ucs2-string-set!
			       utf16-string-ref utf16-string-set!
			       utf16-char-length)))

(define-macro (uc-whitespace-macro)
   (load "utf-category-data.sch")
   (let ((categories (eval '*categories*))
	 (ranges (eval '*category-ranges*)))
      (let loop ((i 0))
	 (cond
	    ((eq? 'Zs (vector-ref categories i))
	     (let liip ((ws-ranges (vector-ref ranges i))
			(l '()))
		(cond
		   ((null? ws-ranges)
		    (let ((chars (cons* #x09 #x0B #x0C #x0A #x0D
					#x2028 #x2029 l)))
		       `(begin
			   (define *white-chars-generated* ',chars)
			   (define (uc-whitespace-generated::bool c::long)
			      (case c
				 (,chars #t)
				 (else #f))))))
		   ((fixnum? (car ws-ranges))
		    (liip (cdr ws-ranges) (cons (car ws-ranges) l)))
		   ((pair? (car ws-ranges))
		    (let luup ((from (car (car ws-ranges)))
			       (l l))
		       (if (>fx from (cadr (car ws-ranges)))
			   (liip (cdr ws-ranges) l)
			   (luup (+fx from 1) (cons from l)))))
		   (else (error 'uc-whitespace-case
				"bad range"
				(car ws-ranges))))))
	    (else (loop (+fx i 1)))))))

(uc-whitespace-macro)

(define (uc-whitespace?::bool c::long)
   (uc-whitespace-generated c))

(define (uc-whitespace-chars::pair)
   *white-chars-generated*)

(define (uc-lineterminator?::bool c::long)
   (case c
      ((#x0A #x0D #x2028 #x2029) #t)
      (else #f)))

(define (uc-categories) *categories*)
(define (uc-category-ranges) *category-ranges*)
