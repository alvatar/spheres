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

(module unicode-reader
   (main main))

(define (split-line l #!optional (delimiter #\;))
   ;; remove comments
   (let* ((sharp-pos (string-index l #\#))
	  (l (if sharp-pos
		 (substring l 0 sharp-pos)
		 l)))
      (let loop ((i 0)
		 (rev-res '()))
	 (if (>=fx i (string-length l))
	     (reverse! rev-res)
	     (let ((index (or (string-index l delimiter i)
			      (string-length l))))
		(let liip ((i i))
		   (cond
		      ((=fx i (string-length l))
		       (loop i (cons "" rev-res)))
		      ((char=? (string-ref l i) #\space)
		       (liip (+fx i 1)))
		      (else
		       (loop (+fx index 1)
			     (cons (substring l i index) rev-res))))))))))
(define (hex-str->number str)
   (let loop ((i 0)
	      (res 0))
      (if (=fx i (string-length str))
	  res
	  (let* ((c (string-ref str i))
		 (ci (char->integer c)))
	     (if (char=? c #\space)
		 (loop (+fx i 1) res)
		 (loop (+fx i 1)
		       (+fx (*fx res 16)
			    (case c
			       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
				(-fx ci (char->integer #\0)))
			       ((#\a #\b #\c #\d #\e #\f)
				(+fx 10 (-fx ci (char->integer #\a))))
			       ((#\A #\B #\C #\D #\E #\F)
				(+fx 10 (-fx ci (char->integer #\A))))
			       (else -1)))))))))

(define (hex-list-str->number-list str)
   (let ((numbers (split-line str #\space)))
      (map hex-str->number numbers)))

;; we add "no-category" (default) to categories.
(define *categories* '#(no-category Co Cs Zp Zl Nl Mc Me Mn Lm Lt Lo Pf No Cf
				    Pi So Ll Pc Sk Lu Nd Pd Sm Pe Ps Sc Po Zs
				    Cc))
(define *nb-categories* (vector-length *categories*))

(define (cat->index str)
   (let ((sym (string->symbol str)))
      (let loop ((res 0))
	 (if (eq? sym (vector-ref *categories* res))
	     res
	     (loop (+fx res 1))))))
(define (index->cat i)
   (vector-ref *categories* i))

(define (vector-hash vec)
   (let loop ((i 0)
	      (hash 0))
      (if (>=fx i (vector-length vec))
	  hash
	  (loop (+fx i 11)
		(remainderfx
		 (+fx (*fx hash 8) (get-hashnumber (vector-ref vec i)))
		 #x1000000)))))
(define (vector-equal? vec1 vec2)
   (let loop ((i 0))
      (if (=fx i (vector-length vec1))
	  #t
	  (and (eq? (vector-ref vec1 i) (vector-ref vec2 i))
	       (loop (+fx i 1))))))

(define (copy-subvector from-vec to-vec from-start to-start len)
   (let loop ((i 0))
      (unless (=fx i len)
	 (vector-set! to-vec (+fx to-start i)
		      (vector-ref from-vec (+fx from-start i)))
	 (loop (+fx i 1)))))

(define (share-entries vec small-vec-size)
   (let* ((res-size (/fx (vector-length vec) small-vec-size))
	  (res-vec (make-vector res-size))
	  (ht (create-hashtable :hash vector-hash
				:eqtest vector-equal?
				:max-bucket-length 30))
	  (tmp-vec (make-vector small-vec-size))
	  )
      (let loop ((i 0))
	 (when (<fx i res-size)
	    (copy-subvector vec tmp-vec (*fx i small-vec-size) 0
			    small-vec-size)
	    (let ((t (hashtable-get ht tmp-vec)))
	       (if t ;; already similary table in ht
		   (vector-set! res-vec i t)
		   (let ((gs (gensym 'utf))
			 (copied (copy-vector tmp-vec small-vec-size)))
		      (let ((t (hashtable-size ht)))
			 (hashtable-put! ht copied gs)
		      (vector-set! res-vec i gs)))))
	    (loop (+fx i 1))))
      (values res-vec ht)))

(define (print-optimized name vec)
   ;; let's share the first stage
   (receive (first-stage-vec first-stage-ht)
      (share-entries vec 256)
      (receive (second-stage-vec second-stage-ht)
	 (share-entries first-stage-vec 256)
	 (hashtable-for-each first-stage-ht
			     (lambda (vec var)
				(print `(define ,var ',vec))))
	 (hashtable-for-each second-stage-ht
			     (lambda (vec var)
				(print `(define ,var
					   (vector ,@(vector->list vec))))))
	 (print `(define ,name (vector ,@(vector->list second-stage-vec)))))))

;; there are some two-char and conditional special casing
;; conditional casings are language dependent, and for now we just ignore them.
(define (read-special-casing! upperc-v lowerc-v titlec-v)
   (define (parse-fields fields i res)
      (let ((cp (hex-str->number (list-ref fields 0)))
	    (lower (hex-list-str->number-list (list-ref fields 1)))
	    (title (hex-list-str->number-list (list-ref fields 2)))
	    (upper (hex-list-str->number-list (list-ref fields 3)))
	    (condition? (and (>=fx (length fields) 5)
			     (not (string-null? (list-ref fields 4))))))
	 (unless condition? ;; just ignore it
	    (when (>fx (length lower) 1)
	       (vector-set! lowerc-v cp (-fx -1 i))
	       (set! i (+fx i 1))
	       (set! res (cons lower res)))
	    (when (>fx (length title) 1)
	       (vector-set! titlec-v cp (-fx -1 i))
	       (set! i (+fx i 1))
	       (set! res (cons title res)))
	    (when (>fx (length upper) 1)
	       (vector-set! upperc-v cp (-fx -1 i))
	       (set! i (+fx i 1))
	       (set! res (cons upper res))))
	 (values i res)))

   (let ((p (open-input-file "data/SpecialCasing.txt")))
      (let loop ((res '())
		 (i 0))
	 (let ((l (read-line p)))
	    (if (eof-object? l)
		(list->vector (reverse! res))
		(let ((fields (split-line l)))
		   (if (null? fields) ;; probably comment line
		       (loop res i)
		       (receive (i res)
			  (parse-fields fields i res)
			  (loop res i)))))))))

(define (print-vectors category-v upperc-v lowerc-v titlec-v)
   (let ((special-vec (read-special-casing! upperc-v lowerc-v titlec-v)))
      (print-optimized '*category* category-v)
      (print-optimized '*upper-casing* upperc-v)
      (print-optimized '*lower-casing* lowerc-v)
      (print-optimized '*title-casing* titlec-v)
      (print `(define *special-casing* ',special-vec))))

(define (print-categories category-v)
   (let ((ranges (make-vector *nb-categories* '())))
      ;; from, to included.
      (define (add-range! cat from to)
	 (when (>=fx cat 0)
	    (if (=fx from to)
		;; only one char
		(vector-set! ranges cat (cons from (vector-ref ranges cat)))
		;; a range
		(vector-set! ranges cat (cons (list from to)
					      (vector-ref ranges cat))))))

      (let loop ((i 0)
		 (last-cat -1)
		 (from 0))
	 (cond
	    ((=fx i (vector-length category-v))
	     (add-range! last-cat from (-fx i 1))
	     ;; reverse the entries
	     (let liip ((j 0))
		(unless (=fx j (vector-length ranges))
		   (vector-set! ranges j (reverse! (vector-ref ranges j)))
		   (liip (+fx j 1))))
	     ;; and print them. (but not the 0-default category)
	     (print `(define *categories*
			',(list->vector (cdr (vector->list *categories*)))))
	     (print `(define *category-ranges*
			',(list->vector (cdr (vector->list ranges))))))
	    ((=fx (vector-ref category-v i) last-cat)
	     (loop (+fx i 1) last-cat from))
	    (else
	     (add-range! last-cat from (-fx i 1))
	     (loop (+fx i 1) (vector-ref category-v i) i))))))

(define *print-vectors?* #t)

(define (main args)
   (when (not (null? (cdr args)))
      (set! *print-vectors?* #f))
   
   (let ((p (open-input-file "data/UnicodeData.txt"))
	 (category-v (make-vector #x110000))
	 (upperc-v (make-vector #x110000))
	 (lowerc-v (make-vector #x110000))
	 (titlec-v (make-vector #x110000)))
      (define (vset! i cat up low title)
	 (unless (string-null? cat)
	    (vector-set! category-v i (cat->index cat)))
	 (unless (string-null? up)
	    (vector-set! upperc-v i (hex-str->number up)))
	 (unless (string-null? low)
	    (vector-set! lowerc-v i (hex-str->number low)))
	 (unless (string-null? title)
	    (vector-set! titlec-v i (hex-str->number title))))
      (let loop ((i 0))
	 (when (<fx i #x110000)
	    (vector-set! category-v i 0)
	    (vector-set! upperc-v i 0)
	    (vector-set! lowerc-v i 0)
	    (vector-set! titlec-v i 0)
	    (loop (+fx i 1))))
      (let loop ()
	 (let ((l (read-line p)))
	    (when (eof-object? l)
	       (if *print-vectors?*
		   (print-vectors category-v upperc-v lowerc-v titlec-v)
		   (print-categories category-v)))
	    (unless (eof-object? l)
	       (let* ((fields (split-line l))
		      (cp (hex-str->number (list-ref fields 0)))
		      (name (list-ref fields 1))
		      (category (list-ref fields 2))
		      (combining-class (list-ref fields 3))
		      (bidi-class (list-ref fields 4))
		      (decomposition (list-ref fields 5))
		      (numeric (list-ref fields 6))
		      (numeric2 (list-ref fields 7))
		      (numeric3 (list-ref fields 8))
		      (bidi-mirrored (list-ref fields 9))
		      (uc1-name (list-ref fields 10))
		      (iso-comment (list-ref fields 11))
		      (upper-case (list-ref fields 12))
		      (lower-case (list-ref fields 13))
		      (title-case (if (>=fx (length fields) 15)
				      (list-ref fields 14)
				      "")))
		  (if (string-contains name "First")
		      ;; get the next line to get the last codepoint of
		      ;; the range.
		      (let* ((ll (read-line p))
			     (ffs (split-line ll))
			     (ccp (hex-str->number (car ffs))))
			 (let liip ((i cp))
			    (when (<fx i ccp)
			       (vset! i
				      category
				      upper-case lower-case title-case)
			       (liip (+fx i 1)))))
		      (vset! cp category upper-case lower-case title-case))
	       (loop)))))))
