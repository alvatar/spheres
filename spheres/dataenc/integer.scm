;;!!! Byte integer conversions
;; .author Aubrey Jaffer, 2003
;; .author Alvaro Castro-Castilla, 2015

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


;;! The multi-byte sequences produced and used by numeric conversion
;; routines are always big-endian. Endianness can be changed during
;; reading and writing bytes using read-bytes and write-bytes.
;;
;; The sign of the length argument to bytes/integer conversion
;; procedures determines the signedness of the number.
;;
;; Converts the first n bytes of big-endian array to an integer.
;; If n is negative then the integer coded by the bytes are treated as
;; two's-complement (can be negative).
;;
;; (u8vector->integer (bytes   0   0   0  15) -4)
;; ==> 15
;; (u8vector->integer (bytes   0   0   0  15)  4)
;; ==> 15
;; (u8vector->integer (bytes 255 255 255 255) -4)
;; ==> -1
;; (u8vector->integer (bytes 255 255 255 255)  4)
;; ==> 4294967295
;; (u8vector->integer (bytes 128   0   0   0) -4)
;; ==> -2147483648
;; (u8vector->integer (bytes 128   0   0   0)  4)
;; ==>  2147483648
(define (u8vector->integer bytes n)
  (define cnt (abs n))
  (cond ((zero? n) 0)
	((and (negative? n) (> (u8vector-ref bytes 0) 127))
	 (do ((lng (- 255 (u8vector-ref bytes 0))
		   (+ (- 255 (u8vector-ref bytes idx)) (* 256 lng)))
	      (idx 1 (+ 1 idx)))
	     ((>= idx cnt) (- -1 lng))))
	(else
	 (do ((lng (u8vector-ref bytes 0)
		   (+ (u8vector-ref bytes idx) (* 256 lng)))
	      (idx 1 (+ 1 idx)))
	     ((>= idx cnt) lng)))))

;;! Converts the integer n to an u8vector.  If n and len are both negative,
;; then the bytes in the returned array are coded two's-complement.
;;
;; (u8vector->list (integer->u8vector 15 -4))
;; ==> (0 0 0 15)
;; (u8vector->list (integer->u8vector 15 4))
;; ==> (0 0 0 15)
;; (u8vector->list (integer->u8vector -1 -4))
;; ==> (255 255 255 255)
;; (u8vector->list (integer->u8vector 4294967295 4))
;; ==> (255 255 255 255)
;; (u8vector->list (integer->u8vector -2147483648 -4))
;; ==> (128 0 0 0)
;; (u8vector->list (integer->u8vector 2147483648 4))
;; ==> (128 0 0 0)
(define (integer->u8vector n len)
  (define bytes (make-u8vector (abs len)))
  (cond ((and (negative? n) (negative? len))
	 (do ((idx (+ -1 (abs len)) (+ -1 idx))
	      (res (- -1 n) (quotient res 256)))
	     ((negative? idx) bytes)
	   (u8vector-set! bytes idx (- 255 (modulo res 256)))))
	(else
	 (do ((idx (+ -1 (abs len)) (+ -1 idx))
	      (res n (quotient res 256)))
	     ((negative? idx) bytes)
	   (u8vector-set! bytes idx (modulo res 256))))))

;;! Modifies sign bit of the argument u8vector so that string<? ordering of
;; two's-complement byte-vectors matches numerical order, and is its own
;; functional inverse.
(define (integer-byte-collate! byte-vector)
  (u8vector-set! byte-vector 0 (logxor #x80 (u8vector-ref byte-vector 0)))
  byte-vector)
