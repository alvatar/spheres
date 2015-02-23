;;!!! Bit vectors
;;
;; Bit-vectors provide an abstract interface to bitwise operations
;; typically done with integers.  They may in fact be implemented as
;; integers on implementations with bignums, or they may be implemented
;; by other means such as vectors which may be more efficient.  These
;; vectors are meant to be used as flags and sets, not integer values,
;; and thus are operations are ones-complement (there are no negative
;; bit-vectors).
;;
;; The following procedures can be used to create and test bit-vectors:
;;
;; (make-bit-vector size)    ; a 'zero' bit-vector, with a hint that we
;;                           ; wish to use SIZE bits
;; (make-bit-vector size #t) ; same as above but initialize the all bit
;;                           ; elements to #t (= the integer 2^SIZE-1)
;; (integer->bit-vector n)   ; create a bit-vector with bits initialized
;;                           ; to the corresponds bits in fixnum N
;; (bit-vector-copy bv)      ; make a copy of the bit-vector BV
;;
;; (bit-vector? obj)         ; #t iff OBJ is a valid bit-vector, which
;;                           ; is not necessarily a disjoint type
;; (bit-vector-empty? bv)    ; #t iff BV has no bits set (the bit-vector
;;                           ; equivalent of the ZERO? procedure)
;; (bit-vector-full? bv to)  ; #t iff BV has all bits lower than TO set
;;                           ; (the low end is 2^i-1)
;;
;; The individual bits in the vector are accessed and set as boolean
;; values:
;;
;; (bit-vector-ref bv i)     ; #t if the I-th bit in BV is set, else #f
;; (bit-vector-set bv i x)   ; return a new copy of BV with the I-th bit
;;                           ; set to boolean x (off iff X is #f)
;; (bit-vector-set! bv i x)  ; in-place update version of the above, is
;;                           ; allowed but not required to mutate BV
;;
;; The following procedures are direct analogues of the corresponding
;; SRFI-33 bitwise operations:
;;
;; (bit-vector-length bv)    ; integer-length
;;                           ;   the index of the largest bit set in BV
;; (bit-vector-count bv)     ; bit-count
;;                           ;   the number of set bits in BV
;; (bit-vector-shift bv n)   ; arithmetic-shift
;; (bit-vector-and bv ...)   ; bitwise-and
;; (bit-vector-ior bv ...)   ; bitwise-ior
;; (bit-vector-xor bv ...)   ; bitwise-xor
;; (bit-vector-eqv bv ...)   ; bitwise-eqv
;; (bit-vector-nand bv ...)  ; bitwise-nand
;; (bit-vector-nor bv ...)   ; bitwise-nor
;;
;; The following in-place update equivalents are also available, which
;; are allowed, but not required, to mutate their first argument only:
;;
;; (bit-vector-shift! bv n)
;; (bit-vector-and! bv ...)
;; (bit-vector-ior! bv ...)
;; (bit-vector-xor! bv ...)
;; (bit-vector-eqv! bv ...)
;; (bit-vector-nand! bv ...)
;; (bit-vector-nor! bv ...)


(cond-expand
 (gambit
  (declare (fixnum)))
 (else (void)))


;;-------------------------------------------------------------------------------
;;!! bitwise-operators (single-byte specific versions)
;; these all return non-negative values in [0..255]

(define (byte-not x)
  (- #b11111111 x))

(define (byte-eqv a b)
  (byte-not (fxxor a b)))

(define (byte-nand a b)
  (byte-not (fxand a b)))

(define (byte-nor a b)
  (byte-not (fxior a b)))


;;-------------------------------------------------------------------------------
;;!! bit-vectors (pass & return #t/#f instead of 1/0)

(define (make-bit-vector size . o)
  (let* ((fill (if (and (pair? o) (car o)) #b11111111 0))
         (len (quotient (+ size 7) 8))
         (res (make-u8vector len fill)))
    (if (zero? fill)
        res
        (let ((off (remainder size 8)))
          (if (not (zero? off))
              (u8vector-set! res (- len 1) (- (arithmetic-shift 1 off) 1)))
          res))))

(define (integer->bit-vector n)
  (let lp ((n n) (ls '()))
    (if (zero? n)
        (list->u8vector (reverse ls))
        (lp (quotient n 256) (cons (remainder n 256) ls)))))

(define (bit-vector-copy vec)
  (let ((len (u8vector-length vec)))
    (if (zero? len)
        (u8vector)
        (subu8vector vec 0 len))))

;; grow to allow setting the i-th element (size = i+1)
(define (bit-vector-grow! vec i)
  (let ((bytes (quotient (+ i 8) 8))
        (len (u8vector-length vec)))
    (if (<= bytes len)
        vec
        (let ((res (make-u8vector bytes 0)))
          (u8vector-copy! res 0 vec)
          res))))

(define (bit-vector-grow vec i)
  (let ((bytes (quotient (+ i 8) 8))
        (len (u8vector-length vec)))
    (if (<= bytes len)
        (bit-vector-copy vec)
        (let ((res (make-u8vector bytes 0)))
          (u8vector-copy! res 0 vec)
          res))))

(define bit-vector? u8vector?)

(define (bit-vector-ref vec i)
  (let ((byte (quotient i 8))
        (off (remainder i 8)))
    (and (< byte (u8vector-length vec))
         (not (zero? (bitwise-and (u8vector-ref vec byte)
                                  (arithmetic-shift 1 off)))))))

(define (bit-vector-set! vec i x)
  (let ((byte (quotient i 8))
        (off (remainder i 8))
        (len (u8vector-length vec)))
    (cond
     ((< byte len)
      (u8vector-set! vec byte
                     (if x
                         (bitwise-ior (u8vector-ref vec byte)
                                      (arithmetic-shift 1 off))
                         (bitwise-and (u8vector-ref vec byte)
                                      (bitwise-not (arithmetic-shift 1 off)))))
      vec)
     ((not x) vec)
     (else (bit-vector-set! (bit-vector-grow vec i) i x)))))

(define (bit-vector-set vec i x)
  (if (or (not x) (< (quotient i 8) (u8vector-length vec)))
      (bit-vector-set! (bit-vector-copy vec) i x)
      (bit-vector-set! (bit-vector-grow vec i) i x)))

(define (bit-vector-length vec)
  (let lp ((i (- (u8vector-length vec) 1)))
    (if (negative? i)
        0
        (let ((x (u8vector-ref vec i)))
          (if (zero? x)
              (lp (- i 1))
              (+ (* 8 i) (integer-length x)))))))

(define (bit-vector-count vec)          ; # of 1's
  (let lp ((i (- (u8vector-length vec) 1))
           (acc 0))
    (if (< i 0)
        acc
        (lp (- i 1) (+ acc (bit-count (u8vector-ref vec i)))))))

;; internal
(define-macro u8vector-map2!
  (lambda (proc a b len)
    `(do ((i 0 (fx+ i 1)))
         ((fx= i ,len) ,a)
       (u8vector-set! ,a i (,proc (u8vector-ref ,a i) (u8vector-ref ,b i))))))

;; internal
(define-macro u8vector-map!
  (lambda (proc pad a rest)
    `(cond
      ((not (u8vector ,a))
       (error "u8vector-map!: not a u8vector" ,a))
      (else
       (let lp ((a ,a)
                (a-len (u8vector-length ,a))
                (ls ,rest))
         (cond
          ((null? ls) a)
          ((not (u8vector (car ls)))
           (error "u8vector-map!: not a u8vector" (car ls)))
          (else
           (let* ((b (car ls))
                  (b-len (u8vector-length b)))
             (cond
              ((fx> b-len a-len)
               (let ((a2 (make-u8vector b-len 0)))
                 (u8vector-copy! a2 0 a)
                 (lp a2 b-len ls)))
              (else
               (u8vector-map2! ,proc a b b-len)
               (if (fx< b-len a-len) (,pad a b-len a-len))
               (lp a a-len (cdr ls))))))))))))

(define (bit-vector-and! a . args)
  (u8vector-map! fxand (lambda (a lo hi) (u8vector-fill! a 0 lo hi)) a args))

(define (bit-vector-and a . args)
  (apply bit-vector-and! (bit-vector-copy a) args))

(define (bit-vector-ior! a . args)
  (u8vector-map! fxior (lambda (a lo hi) #f) a args))

(define (bit-vector-ior a . args)
  (apply bit-vector-ior! (bit-vector-copy a) args))

(define (bit-vector-xor! a . args)
  (u8vector-map! fxxor (lambda (a lo hi) #f) a args))

(define (bit-vector-xor a . args)
  (apply bit-vector-xor! (bit-vector-copy a) args))

(define (bit-vector-eqv! a . args)
  (define (u8vector-not! v lo hi)
    (do ((i lo (+ i 1)))
        ((= i hi))
      (u8vector-set! v i (byte-not (u8vector-ref v i)))))
  (u8vector-map! byte-eqv u8vector-not! a args))

(define (bit-vector-eqv a . args)
  (apply bit-vector-eqv! (bit-vector-copy a) args))

(define (bit-vector-nand! a . args)
  (u8vector-map! byte-nand (lambda (a lo hi) (u8vector-fill! a 0 lo hi)) a args))

(define (bit-vector-nand a . args)
  (apply bit-vector-nand! (bit-vector-copy a) args))

(define (bit-vector-nor! a . args)
  (define (u8vector-not! v lo hi)
    (do ((i lo (+ i 1)))
        ((= i hi))
      (u8vector-set! v i (byte-not (u8vector-ref v i)))))
  (u8vector-map! byte-nor u8vector-not! a args))

(define (bit-vector-nor a . args)
  (apply bit-vector-nor! (bit-vector-copy a) args))

;; shift in place w/o resizing
(define (bit-vector-shift-in-place! vec n)
  (if (not (zero? n))
      (let ((len (u8vector-length vec)))
        (cond
         ((= len 1)
          (u8vector-set! vec 0 (bitwise-and
                                #b11111111
                                (arithmetic-shift (u8vector-ref vec 0) n))))
         ((positive? n)
          (let* ((byte (quotient n 8))
                 (off (remainder n 8))
                 (start (- len byte 1)))
            (cond
             ((zero? off)
              (u8vector-shift! vec byte))
             ((>= byte len)
              (u8vector-fill! vec 0))
             (else
              (let* ((lo-mask (- (arithmetic-shift 1 (- 8 off)) 1))
                     (lo-shift off)
                     (hi-mask (byte-not lo-mask))
                     (hi-shift (- off 8))
                     (get-lo
                      (lambda (x)
                        (arithmetic-shift (bitwise-and x lo-mask) lo-shift)))
                     (get-hi
                      (lambda (x)
                        (arithmetic-shift (bitwise-and x hi-mask) hi-shift))))
                (u8vector-set! vec (- len 1) (get-lo (u8vector-ref vec start)))
                (let lp ((from (- start 1)) (to (- len 1)))
                  (if (negative? from)
                      (if (>= to 0)
                          (u8vector-fill! vec 0 0 to))
                      (let* ((from-val (u8vector-ref vec from))
                             (lo (get-lo from-val))
                             (hi (get-hi from-val)))
                        (if (positive? to)
                            (u8vector-set! vec (- to 1) lo))
                        (u8vector-set! vec to (bitwise-ior hi (u8vector-ref vec to)))
                        (lp (- from 1) (- to 1))))))))))
         (else
          (let* ((byte (quotient (- n) 8))
                 (off (remainder (- n) 8))
                 (s-byte (+ byte 1))
                 (s-off (- 8 off))
                 (save (u8vector-ref vec byte)))
            ;; shift negative by 1+bytes
            (u8vector-shift! vec (- s-byte))
            ;; shift positive by complement of off
            (bit-vector-shift-in-place! vec s-off)
            ;; reset lo byte
            (u8vector-set!
             vec
             0
             (bitwise-ior (u8vector-ref vec 0)
                          (bitwise-and
                           (arithmetic-shift save (- off))
                           (- (arithmetic-shift 1 s-off) 1)))))))))
  ;; return the vector for convenience
  vec)

(define (bit-vector-shift! vec n)
  (if (positive? n)
      (bit-vector-shift-in-place!
       (bit-vector-grow! vec (+ -1 n (bit-vector-length vec)))
       n)
      (bit-vector-shift-in-place! vec n)))

(define (bit-vector-shift vec n)
  (if (positive? n)
      (bit-vector-shift-in-place!
       (bit-vector-grow vec (+ -1 n (bit-vector-length vec)))
       n)
      (bit-vector-shift-in-place! (bit-vector-copy vec) n)))

(define (range->bit-vector start end)
  (make-bit-vector (+ 1 (- end start)) #t))

(define (bit-vector-empty? vec)
  (let lp ((i (- (u8vector-length vec) 1)))
    (or (< i 0)
        (and (zero? (u8vector-ref vec i))
             (lp (- i 1))))))

(define (bit-vector-full? vec to)
  (let ((limit (quotient to 8))
        (off (remainder to 8))
	(len (u8vector-length vec)))
    (let lp ((i 0))
      (if (= i limit)
	  (or (zero? off)
	      (and (< i len)
		   (let ((mask (- (arithmetic-shift 1 off) 1)))
		     (= mask (bitwise-and mask (u8vector-ref vec i))))))
	  (and (< i len)
	       (= #b11111111 (u8vector-ref vec i))
	       (lp (+ i 1)))))))

;; debugging aid
;; (define (bit-vector->string vec)
;;   (with-output-to-string
;;     (lambda ()
;;       (let ((len (u8vector-length vec)))
;;         (do ((i 0 (+ i 1)))
;;             ((= i len))
;;           (display (number->string (u8vector-ref vec i) 2)))))))
