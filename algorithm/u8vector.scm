;;!!! SRFI-66 Octet Vectors
;; .author Mikael More, 2008, 2011-2014
;; .author Michael Sperber, 2005
;; .author Alvaro Castro-Castilla, 2012-2014
;;
;; Part of the API is implemented natively by Gambit
;;
;;! Copies data from octet vector source to octet vector target. Source-start,
;; target-start, and n must be non-negative exact integers that satisfy
;; 0 <= source-start <= source-start + n <= (u8vector-length source)
;; 0 <= target-start <= target-start + n <= (u8vector-length target)
;; This copies the octets from source at indices [source-start, source-start + n)
;; to consecutive indices in target starting at target-index.
;; This must work even if the memory regions for the source and the target
;; overlap, i.e., the octets at the target location after the copy must be equal
;; to the octets at the source location before the copy.
;; The number of return values and the return values are unspecified.
;; However, the number of return values is such that it is accepted by a
;; continuation created by begin. Analogous to vector-ref.
;; .author Michael Sperber
;; (define (u8vector-copy! source source-start target target-start count)
;;   (if (>= source-start target-start)
;;       (do ((i 0 (+ i 1)))
;; 	  ((= i count))
;;         (u8vector-set! target
;;                        (+ target-start i) 
;;                        (u8vector-ref source (+ source-start i))))
;;       (do ((i (- count 1) (- i 1)))
;; 	  ((= i -1))
;;         (u8vector-set! target
;;                        (+ target-start i) 
;;                        (u8vector-ref source (+ source-start i))))))
;; .author Arthur T Smyles
(define (u8vector-copy! source source-start target target-start n)
  (subu8vector-move! source source-start (+ source-start n) target target-start))

;;; TODO: Pending fix for () in FFI functions
;; (define native-endianness
;;   (let ((ne (c-lambda () bool "
;; 	#ifdef ___BIG_ENDIAN 
;; 	   ___result=1; 
;; 	#else 
;; 	   ___result=0; 
;; 	#endif")))
;;     (lambda () (if (ne) 'big 'little))))

;;! Compares u8vector-1 and u8vector-2 and returns a value consistent with the
;; vector ordering specified in SRFI 67, i.e. -1 if u8vector-1 is smaller than
;; u8vector-2, 0 if they are equal, and 1 if u8vector-1 is greater than u8vector-2.
;; Shorter vectors are always smaller than longer ones, and vectors of equal length
;; are compared lexicographically.
;; .author Michael Sperber
(define (u8vector-compare u8vector-1 u8vector-2)
  (let ((length-1 (u8vector-length u8vector-1))
        (length-2 (u8vector-length u8vector-2)))
    (cond
     ((< length-1 length-2) -1)
     ((> length-1 length-2)  1)
     (else
      (let loop ((i 0))
        (if (= i length-1)
            0
            (let ((elt-1 (u8vector-ref u8vector-1 i))
                  (elt-2 (u8vector-ref u8vector-2 i)))
              (cond ((< elt-1 elt-2) -1)
                    ((> elt-1 elt-2)  1)
                    (else (loop (+ i 1)))))))))))

;;! Returns #t if u8vector-1 and u8vector-2 are equal---that is, if they
;; have the same length and equal elements at all valid indices.
;; .author Michael Sperber
;; (define (u8vector=? u8vector-1 u8vector-2)
;;   (let ((size (u8vector-length u8vector-1)))
;;     (and (= size (u8vector-length u8vector-2))
;; 	 (let loop ((i 0))
;; 	   (or (>= i size)
;; 	       (and (= (u8vector-ref u8vector-1)
;; 		       (u8vector-ref u8vector-2))
;; 		    (loop (+ 1 i))))))))
;; .author Arthur T Smyles
(define (u8vector=? u8vector-1 u8vector-2) (= (u8vector-compare u8vector-1 u8vector-2) 0))

;; U8vector manipulation / handling
;; Author: Marc Feeley 
;;
;; (apply-u8vector-append u8vector-list)
;;
;; (u8vector-reverse u8v) => u8vector
;; Makes a new u8vector with u8v's contents with reversed order on the contained bytes, and returns it.
;;
;; (u8vector-invert! u8v)
;; Makes a new u8vector with u8v's contents with each byte inversed (i.e. (- 255 b)), and returns it.
;;
;; (u8vector-pad-to-length u8v to-length #!optional (pad-byte 0))
;;
;; (u8vector-xor u8v with-byte)
(define (apply-u8vector-append u8vector-list)
  (call-with-output-u8vector
   '()
   (lambda (output-port)
     (let loop ((rest u8vector-list))
       (if (not (null? rest))
           (let ((v (car rest)))
             (if (not (u8vector? v)) (error "apply-u8vector-append got non-u8vector argument" v))
             (write-subu8vector v 0 (u8vector-length v) output-port)
             (loop (cdr rest))))))))

(define (u8vector-reverse v)
  (let* ((l (u8vector-length v))
         (r (make-u8vector l)))
    (let loop ((src-idx 0) (target-idx (- l 1)))
      (u8vector-set! r target-idx (u8vector-ref v src-idx))
      (if (> target-idx 0)
          (loop (+ src-idx 1) (- target-idx 1))))
    r))

(define (u8vector-invert! v)
  (let loop ((i (u8vector-length v)))
      (if (not (zero? i))
          (let ((i (- i 1)))
            (u8vector-set! v i (##fixnum.bitwise-xor
                                (u8vector-ref v i)
                                255))
            (loop i)))))

;; May return the originally passed |u8v| object reference.
(define* (u8vector-pad-to-length u8v to-length (pad-byte 0))
  (let ((u8v-length (u8vector-length u8v)))
    (if (fx> to-length u8v-length)
        (let ((new-u8v (make-u8vector to-length pad-byte)))
          (subu8vector-move! u8v 0 u8v-length new-u8v 0)
          new-u8v)
        u8v)))

;;!! u8vector-xor
(define (u8vector-xor u8vect1 u8vect2)
  (let* ((len (u8vector-length u8vect1))
         (result (make-u8vector len)))
    (let loop ((i (- len 1)))
      (if (>= i 0)
          (begin
            (u8vector-set! result i (fxxor (u8vector-ref u8vect1 i)
                                           (u8vector-ref u8vect2 i)))
            (loop (- i 1)))
          result))))

;;!! u8vector-xor with a single byte
(define (u8vector-xor/byte u8v with-byte)
  (let* ((length (u8vector-length u8v))
         (new-u8v (make-u8vector length)))
    (let loop ((at 0))
      (if (##not (##eq? length at))
          (begin
            (##u8vector-set! new-u8v at (##fxxor (##u8vector-ref u8v at)
                                                 with-byte))
            (loop (##fx+ at 1)))))
    new-u8v))

