;;!!! Hilbert vectors are like vectors that grow as large as they need to.
;; That is, they can be indexed by arbitrarily large nonnegative integers.
;; The implementation allows for arbitrarily large gaps by arranging
;; the entries in a tree.
;; So-called because they live in an infinite-dimensional vector
;; space...
;;
;; .author Richard Kelsey and Jonathan Rees
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 1993-2007 by Richard Kelsey and Jonathan Rees.
;; Copyright (c) 1993-2006 Richard Kelsey and Jonathan Rees
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the authors may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define hilbert-log 8)
(define hilbert-node-size (arithmetic-shift 1 hilbert-log))
(define hilbert-mask (- hilbert-node-size 1))
(define minus-hilbert-log (- 0 hilbert-log))

(define-record-type sparse-vector
  (make-hilbert height root default)
  sparse-vector?
  (height hilbert-height set-hilbert-height!)
  (root hilbert-root set-hilbert-root!)
  (default hilbert-default set-hilbert-default!))

(define-structure hilbert-default value)

(define* (make-sparse-vector (default #f))
  (make-hilbert 1
                (make-vector hilbert-node-size (make-hilbert-default default))
                (make-hilbert-default default)))

(define (sparse-vector-set! hilbert index value)
  (vector-set!
   (let recur ((height (hilbert-height hilbert))
	       (index index))
     (if (= height 1)
	 (make-higher-if-necessary hilbert index)
	 (let ((index (arithmetic-shift index minus-hilbert-log)))
	   (make-node-if-necessary
	    (recur (- height 1) index)
	    (bitwise-and index hilbert-mask)
	    (hilbert-default hilbert)))))
   (bitwise-and index hilbert-mask)
   value))

(define (sparse-vector-ref1 hilbert index)
  (let recur ((height (hilbert-height hilbert))
	      (index index))
    (if (= height 1)
	(let ((root (hilbert-root hilbert)))
	  (if (< index (vector-length root))
	      (vector-ref root index)
	      (hilbert-default hilbert)))
	(let ((node (recur (- height 1)
			   (arithmetic-shift index minus-hilbert-log))))
	  (if (vector? node)
	      (vector-ref node (bitwise-and index hilbert-mask))
	      (hilbert-default hilbert))))))

(define (sparse-vector-ref hilbert index)
  (let ((val (sparse-vector-ref1 hilbert index)))
    (if (hilbert-default? val)
        (hilbert-default-value val)
        val)))

(define (make-higher-if-necessary hilbert index)
  (if (< index hilbert-node-size)
      (hilbert-root hilbert)
      (let ((new-root (make-vector hilbert-node-size (hilbert-default hilbert))))
	(vector-set! new-root 0 (hilbert-root hilbert))
	(set-hilbert-root! hilbert new-root)
	(set-hilbert-height! hilbert (+ (hilbert-height hilbert) 1))
	(let ((index (arithmetic-shift index minus-hilbert-log)))
	  (make-node-if-necessary (make-higher-if-necessary hilbert index)
				  (bitwise-and index hilbert-mask)
				  (hilbert-default hilbert))))))

(define (make-node-if-necessary node index default)
  (let ((v (vector-ref node index)))
    (if (vector? v) v
	(let ((new (make-vector hilbert-node-size default)))
	  (vector-set! node index new)
	  new))))

(define (sparse-vector->list h)
  (let recur ((node (hilbert-root h))
              (height (hilbert-height h))
              (more '()))
    (if (= height 0)
        (if (or (vector? node) (pair? more))
            (cons (if (hilbert-default? node) (hilbert-default-value node) node) more)
            '())
        (do ((i (- hilbert-node-size 1) (- i 1))
             (more more (recur (if (vector? node)
				   (let ((val (vector-ref node i)))
				     (if (hilbert-default? val)
					 (hilbert-default-value val)
					 val))
                                   (hilbert-default h))
                               (- height 1) more)))
            ((< i 0) more)))))
