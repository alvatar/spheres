;;!!! Byte integer and IEEE floating-point conversions.
;; .author Aubrey Jaffer, 2003
;; .author Alvaro Castro-Castilla, 2015. Gambit compatiblity.

;; Copyright (C) 2003 Aubrey Jaffer
;;
;; Permission to copy ; this software, to modify it, to redistribute it,
;; to distribute modified versions, and to use it for any purpose is
;; granted, subject to the following restrictions and understandings.
;;
;; 1.  Any copy made of ; this software must include ; this copyright notice
;; in full.
;;
;; 2.  I have made no warranty or representation that the operation of
;; this software will be error-free, and I am under no obligation to
;; provide any services, by way of maintenance, update, or otherwise.
;;
;; 3.  In conjunction with products arising from the use of ; this
;; material, there shall be no use of my name in any advertising,
;; promotional, or sales literature without prior written consent in
;; each case.


;; Implementation notes: bitwise-if/bitwise-merge use masks that are interpreted
;; in the opposite way as SRFI-60, leading to non-obvious confusion.


;;! Calculates and returns the value of the provided u8vector interpreted as a
;; big-endian IEEE 4-byte (32-bit) number.
;;
;;  S EEEEEEE E FFFFFFF FFFFFFFF FFFFFFFF
;;  ========= ========= ======== ========
;;  0 1       8 9                      31
;;
;; (u8vector->ieee-float '#u8(0 0 0 0))
;; ==> 0.0
;; (u8vector->ieee-float '#u8(#x80 0 0 0))
;; ==> -0.0
;; (u8vector->ieee-float '#u8(#x40 0 0 0))
;; ==> 2.0
;; (u8vector->ieee-float '#u8(#x40 #xd0 0 0))
;; ==> 6.5
;; (u8vector->ieee-float '#u8(#xc0 #xd0 0 0))
;; ==> -6.5
;;
;; (u8vector->ieee-float '#u8(0 #x80 0 0))
;; ==> 11.754943508222875e-39
;; (u8vector->ieee-float '#u8(0 #x40 0 0))
;; ==> 5.877471754111437e-39
;; (u8vector->ieee-float '#u8(0 0 0 1))
;; ==> 1.401298464324817e-45
;;
;; (u8vector->ieee-float '#u8(#xff #x80 0 0))
;; ==> -inf.0
;; (u8vector->ieee-float '#u8(#x7f #x80 0 0))
;; ==> +inf.0
;; (u8vector->ieee-float '#u8(#x7f #x80 0 1))
;; ==> +nan.0
;; (u8vector->ieee-float '#u8(#x7f #xc0 0 0))
;; ==> +nan.0
(define (u8vector->ieee-float bytes)
  (define zero 0.0)
  (define one 1.0)
  (define len (u8vector-length bytes))
  (define S (logbit? 7 (u8vector-ref bytes 0)))
  (define E (+ (ash (logand #x7F (u8vector-ref bytes 0)) 1)
	       (ash (logand #x80 (u8vector-ref bytes 1)) -7)))
  (if (not (eqv? 4 len))
      (error 'u8vector->ieee-double "Wrong length: u8vector must have 4 elements"))
  (do ((F (u8vector-ref bytes (+ -1 len))
	  (+ (u8vector-ref bytes idx) (/ F 256)))
       (idx (+ -2 len) (+ -1 idx)))
      ((<= idx 1)
       (set! F (/ (+ (logand #x7F (u8vector-ref bytes 1)) (/ F 256)) 128))
       (cond ((< 0 E 255) (* (if S (- one) one) (expt 2 (- E 127)) (+ 1 F)))
	     ((zero? E)
	      (if (zero? F)
		  (if S (- zero) zero)
		  (* (if S (- one) one) (expt 2 -126) F)))
	     ;; E must be 255
	     ((not (zero? F)) (/ zero zero))
	     (else (/ (if S (- one) one) zero))))))


;;! Calculates and returns the value of the u8vector interpreted as a big-endian
;; IEEE 8-byte (64-bit) number.
;;  S EEEEEEE EEEE FFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF
;;  ========= ========= ======== ======== ======== ======== ======== ========
;;  0 1         11 12                                                      63

;; (u8vector->ieee-double '#u8(0 0 0 0 0 0 0 0))
;; ==> 0.0
;; (u8vector->ieee-double '#u8(#x80 0 0 0 0 0 0 0))
;; ==> -0.0
;; (u8vector->ieee-double '#u8(#x40 0 0 0 0 0 0 0))
;; ==> 2.0
;; (u8vector->ieee-double '#u8(#x40 #x1A 0 0 0 0 0 0))
;; ==> 6.5
;; (u8vector->ieee-double '#u8(#xC0 #x1A 0 0 0 0 0 0))
;; ==> -6.5
;; (u8vector->ieee-double '#u8(0 8 0 0 0 0 0 0))
;; ==> 11.125369292536006e-309
;; (u8vector->ieee-double '#u8(0 4 0 0 0 0 0 0))
;; ==> 5.562684646268003e-309
;; (u8vector->ieee-double '#u8(0 0 0 0 0 0 0 1))
;; ==> 4.0e-324
;; (u8vector->ieee-double (list->u8vector '(127 239 255 255 255 255 255 255)))
;; ==> 179.76931348623157e306
;; (u8vector->ieee-double '#u8(#xFF #xF0 0 0 0 0 0 0))
;; ==> -inf.0
;; (u8vector->ieee-double '#u8(#x7F #xF0 0 0 0 0 0 0))
;; ==> +inf.0
;; (u8vector->ieee-double '#u8(#x7F #xF8 0 0 0 0 0 0))
;; ==> +nan.0
(define (u8vector->ieee-double bytes)
  (define zero 0.0)
  (define one  1.0)
  (define len (u8vector-length bytes))
  (define S (logbit? 7 (u8vector-ref bytes 0)))
  (define E (+ (ash (logand #x7F (u8vector-ref bytes 0)) 4)
	       (ash (logand #xF0 (u8vector-ref bytes 1)) -4)))
  (if (not (eqv? 8 len))
      (error 'u8vector->ieee-double "Wrong length: u8vector must have 4 elements"))
  (do ((F (u8vector-ref bytes (+ -1 len))
	  (+ (u8vector-ref bytes idx) (/ F 256)))
       (idx (+ -2 len) (+ -1 idx)))
      ((<= idx 1)
       (set! F (/ (+ (logand #x0F (u8vector-ref bytes 1)) (/ F 256)) 16))
       (cond ((< 0 E 2047) (* (if S (- one) one) (expt 2 (- E 1023)) (+ 1 F)))
	     ((zero? E)
	      (if (zero? F)
		  (if S (- zero) zero)
		  (* (if S (- one) one) (expt 2 -1022) F)))
	     ;; E must be 2047
	     ((not (zero? F)) (/ zero zero))
	     (else (/ (if S (- one) one) zero))))))

;;! Returns an u8vector encoding the IEEE single-precision floating-point
;; (u8vector->list (ieee-float->u8vector  0.0))
;; ==> (0 0 0 0)
;; (u8vector->list (ieee-float->u8vector -0.0))
;; ==> (128 0 0 0)
;; (u8vector->list (ieee-float->u8vector  2.0))
;; ==> (64 0 0 0)
;; (u8vector->list (ieee-float->u8vector  6.5))
;; ==> (64  208 0 0)
;; (u8vector->list (ieee-float->u8vector -6.5))
;; ==> (192 208 0 0)
;; (u8vector->list (ieee-float->u8vector 11.754943508222875e-39))
;; ==> (0 128 0 0)
;; (u8vector->list (ieee-float->u8vector  5.877471754111438e-39))
;; ==> (0 64 0 0)
;; (u8vector->list (ieee-float->u8vector  1.401298464324817e-45))
;; ==> (0 0 0 1)
;; (u8vector->list (ieee-float->u8vector -inf.0))
;; ==> (255 128 0 0)
;; (u8vector->list (ieee-float->u8vector +inf.0))
;; ==> (127 128 0 0)
;; (u8vector->list (ieee-float->u8vector +nan.0))
;; (127 192 0 0)
(define (ieee-float->u8vector flt)
  (define byts (make-u8vector 4 0))
  (define S (and (real? flt) (negative? (if (zero? flt) (/ flt) flt))))
  (define (scale flt scl)
    (cond ((zero? scl)
           (out (/ flt 2) scl))
          ((>= flt 16)
           (let ((flt/16 (/ flt 16)))
             (cond ((= flt/16 flt)
                    (u8vector-set! byts 0 (if S #xFF #x7F))
                    (u8vector-set! byts 1 #x80)
                    byts)
                   (else
                    (scale flt/16 (+ scl 4))))))
          ((>= flt 2)             (scale (/ flt 2) (+ scl 1)))
          ((and (>= scl 4)
                (< (* 16 flt) 1)) (scale (* flt 16) (+ scl -4)))
          ((< flt 1)
           (scale (* flt 2) (+ scl -1)))
          (else
           (out (+ -1 flt) scl))))
  (define (out flt scl)
    (let ((flt (inexact->exact flt)))
     (do ((flt (* 128 flt) (* 256 (- flt val)))
          (val (floor (* 128 flt))
               (floor (* 256 (- flt val))))
          (idx 1 (+ 1 idx)))
         ((> idx 3)
          (u8vector-set!
           byts 1 (cond-expand
                   (gambit
                    (##bitwise-merge #x80 (u8vector-ref byts 1) (ash scl 7)))
                   (else
                    (bitwise-if #x80 (ash scl 7) (u8vector-ref byts 1)))))
          (u8vector-set! byts 0 (+ (if S 128 0) (ash scl -1)))
          byts)
       (u8vector-set! byts idx val))))
  (set! flt (magnitude flt))
  (cond ((zero? flt) (if S (u8vector-set! byts 0 #x80)) byts)
        ((or (not (real? flt))
             (not (= flt flt)))
         (u8vector-set! byts 0 (if S #xFF #x7F))
         (u8vector-set! byts 1 #xC0)
         byts)
        (else (scale flt 127))))

;;! Returns a 8-element u8vector encoding the IEEE double-precision
;; floating-point of the argument.
;;
;; (u8vector->list (ieee-double->u8vector  0.0))
;; ==> (0 0 0 0 0 0 0 0)
;; (u8vector->list (ieee-double->u8vector -0.0))
;; ==> (128   0 0 0 0 0 0 0)
;; (u8vector->list (ieee-double->u8vector  2.0))
;; ==> (64 0 0 0 0 0 0 0)
;; (u8vector->list (ieee-double->u8vector  6.5))
;; ==> (64 26 0 0 0 0 0 0)
;; (u8vector->list (ieee-double->u8vector -6.5))
;; ==> (192  26 0 0 0 0 0 0)
;; (u8vector->list (ieee-double->u8vector 11.125369292536006e-309))
;; ==> (0 8 0 0 0 0 0 0)
;; (u8vector->list (ieee-double->u8vector  5.562684646268003e-309))
;; ==> (0 4 0 0 0 0 0 0)
;; (u8vector->list (ieee-double->u8vector  4.0e-324))
;; ==> (0 0 0 0 0 0 0 1)
;; (u8vector->list (ieee-double->u8vector -inf.0))
;; ==> (255 240 0 0 0 0 0 0)
;; (u8vector->list (ieee-double->u8vector +inf.0))
;; ==> (127 240 0 0 0 0 0 0)
;; (u8vector->list (ieee-double->u8vector  +nan.0))
;; ==> (127 248 0 0 0 0 0 0)
(define (ieee-double->u8vector flt)
  (define byts (make-u8vector 8 0))
  (define S (and (real? flt) (negative? (if (zero? flt) (/ flt) flt))))
  (define (scale flt scl)
    (cond ((zero? scl)
           (out (/ flt 2) scl))
          ((>= flt 16)
           (let ((flt/16 (/ flt 16)))
             (cond ((= flt/16 flt)
                    (u8vector-set! byts 0 (if S #xFF #x7F))
                    (u8vector-set! byts 1 #xF0)
                    byts)
                   (else
                    (scale flt/16 (+ scl 4))))))
          ((>= flt 2)
           (scale (/ flt 2) (+ scl 1)))
          ((and (>= scl 4)
                (< (* 16 flt) 1)) (scale (* flt 16) (+ scl -4)))
          ((< flt 1)
           (scale (* flt 2) (+ scl -1)))
          (else
           (out (+ -1 flt) scl))))
  (define (out flt scl)
    (do ((flt (* 16 flt) (* 256 (- flt val)))
         (val (inexact->exact (floor (* 16 flt)))
              (inexact->exact (floor (* 256 (- flt val)))))
         (idx 1 (+ 1 idx)))
        ((> idx 7)
         (u8vector-set!
          byts 1 (cond-expand
                  (gambit
                   (##bitwise-merge #xf0 (u8vector-ref byts 1) (ash scl 4)))
                  (else
                   (bitwise-if #xf0 (ash scl 4) (u8vector-ref byts 1)))))
         (u8vector-set! byts 0 (+ (if S 128 0) (ash scl -4)))
         byts)
      (u8vector-set! byts idx val)))
  (set! flt (magnitude flt))
  (cond ((zero? flt) (if S (u8vector-set! byts 0 #x80)) byts)
        ((or (not (real? flt))
             (not (= flt flt)))
         (u8vector-set! byts 0 #x7F)
         (u8vector-set! byts 1 #xF8)
         byts)
        (else (scale flt 1023))))

;;! Modifies the u8vector so that string<? ordering of IEEE floating-point
;; byte-vectors matches numerical order.
(define (ieee-byte-collate! byte-vector)
  (cond ((logtest #x80 (u8vector-ref byte-vector 0))
	 (do ((idx (+ -1 (u8vector-length byte-vector)) (+ -1 idx)))
	     ((negative? idx))
	   (u8vector-set! byte-vector idx
                          (logxor #xFF (u8vector-ref byte-vector idx)))))
	(else
	 (u8vector-set! byte-vector 0 (logxor #x80 (u8vector-ref byte-vector 0)))))
  byte-vector)

;;! Given a modified u8vector by ieee-byte-collate!, reverses the modifications
(define (ieee-byte-decollate! byte-vector)
  (cond ((not (logtest #x80 (u8vector-ref byte-vector 0)))
	 (do ((idx (+ -1 (u8vector-length byte-vector)) (+ -1 idx)))
	     ((negative? idx))
	   (u8vector-set! byte-vector idx
                          (logxor #xFF (u8vector-ref byte-vector idx)))))
	(else
	 (u8vector-set! byte-vector 0 (logxor #x80 (u8vector-ref byte-vector 0)))))
  byte-vector)
