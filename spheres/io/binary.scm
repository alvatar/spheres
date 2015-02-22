;; binary-io.scm - Binary parsing and writing.
;; .author Alex Shinn, 2004.2005
;; .author Marco Benelli, 2011
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 2011 by Marco Benelli. All rights reserved.
;;
;; Based of srfi-56 by Alex Shinn.
;;
;; === Original copyright notice ===
;;
;; Copyright (c) 2004-2005 by Alex Shinn. All rights reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;; constants
(define *byte-size* 8)
(define *byte-magnitude* (expt 2 *byte-size*))
(define *byte-mask* (- *byte-magnitude* 1))
(define *byte-right-shift* (* -1 *byte-size*))

;; XXXX default endianess (platform specific, these defaults are for x86)
(define *default-endian* 'little-endian)
(define *default-float-endian* 'little-endian)

(define (default-endian) *default-endian*)
(define (default-float-endian) *default-float-endian*)


;; don't differentiate between binary and character ports
(define (port? x) (or (input-port? x) (output-port? x)))
(define binary-port? port?)
(define character-port? port?)
(define open-binary-input-file open-input-file)
(define open-binary-output-file open-output-file)
(define call-with-binary-input-file call-with-input-file)
(define call-with-binary-output-file call-with-output-file)
(define with-input-from-binary-file with-input-from-file)
(define with-output-to-binary-file with-output-to-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library code.  Below here only optimization changes should be needed.

;; basic reading

(define (combine . bytes)
  (combine-ls bytes))

(define (combine-ls bytes)
  (let loop ((b bytes) (acc 0))
    (if (null? b) acc
        (loop (cdr b) (+ (arithmetic-shift acc 8) (car b))))))

(define* (read-binary-uint size (port (current-input-port))
			  (endian *default-endian*))
  (let loop ((i size) (ls '()))
    (if (zero? i)
	(combine-ls (if (eq? endian 'big-endian) (reverse ls) ls))
	(let ((b (read-u8 port)))
	  (if (eof-object? b) b (loop (- i 1) (cons b ls)))))))


(define* (read-binary-sint size (port (current-input-port))
                           (endian *default-endian*))
  (let loop ((i size) (ls '()))
    (if (zero? i)
	(let ((bytes (if (eq? endian 'big-endian) (reverse ls) ls)))
	  (if (> (car bytes) 127)
	      (* -1 (+ 1 (combine-ls (map (lambda (b) (- 255 b)) bytes))))
	      (combine-ls bytes)))
	(let ((b (read-u8 port)))
	  (if (eof-object? b) b (loop (- i 1) (cons b ls)))))))

(define* (read-binary-uint8 (p (current-input-port))
                           (e *default-endian*))
  (read-u8 p))

(define* (read-binary-uint16 (p (current-input-port))
                            (e *default-endian*))
  (read-binary-uint 2 p e))

(define* (read-binary-uint32 (p (current-input-port))
                           (e *default-endian*))
  (read-binary-uint 4 p e))

(define* (read-binary-uint64 (p (current-input-port))
                           (e *default-endian*))
  (read-binary-uint 8 p e))

(define* (read-binary-sint8 (p (current-input-port))
                           (e *default-endian*))
  (read-binary-sint 1 p e))

(define* (read-binary-sint16 (p (current-input-port))
                           (e *default-endian*))
  (read-binary-sint 2 p e))

(define* (read-binary-sint32 (p (current-input-port))
                           (e *default-endian*))
  (read-binary-sint 4 p e))

(define* (read-binary-sint64 (p (current-input-port))
                           (e *default-endian*))
  (read-binary-sint 8 p e))


;;-------------------------------------------------------------------------------
;;!! Basic writing

(define* (write-binary-uint size int (port (current-output-port))
                            (endian *default-endian*))
  (let loop ((i size) (n int) (ls '()))
    (if (zero? i)
        (for-each
         (lambda (b) (write-u8 b port))
         (if (eq? endian 'big-endian)
             ls
             (reverse ls)))
        (loop (- i 1) (arithmetic-shift n *byte-right-shift*)
              (cons (bitwise-and n *byte-mask*) ls)))))

(define* (write-binary-sint size int (port (current-output-port))
                            (endian *default-endian*))
  (let loop ((i size) (n (if (negative? int) (- -1 int) int)) (ls '()))
    (if (zero? i)
	(for-each
         (lambda (b) (write-u8 b port))
         ((if (negative? int)
              (lambda (ls) (map (lambda (x) (- 255 x)) ls))
              (lambda (x) x))
          (if (eq? endian 'big-endian)
              ls
              (reverse ls))))
	(loop (- i 1) (arithmetic-shift n *byte-right-shift*)
	      (cons (bitwise-and n *byte-mask*) ls)))))

(define* (write-binary-uint8 n (p (current-output-port))
                            (e *default-endian*))
  (write-u8 n p))

(define* (write-binary-uint16 n (p (current-output-port))
                             (e *default-endian*))
  (write-binary-uint 2 n p e))

(define* (write-binary-uint32 n (p (current-output-port))
                             (e *default-endian*))
  (write-binary-uint 4 n p e))

(define* (write-binary-uint64 n (p (current-output-port))
                             (e *default-endian*))
  (write-binary-uint 8 n p e))

(define* (write-binary-sint8 n (p (current-output-port))
                            (e *default-endian*))
  (write-binary-sint 1 n p e))

(define* (write-binary-sint16 n (p (current-output-port))
                             (e *default-endian*))
  (write-binary-sint 2 n p e))

(define* (write-binary-sint32 n (p (current-output-port))
                             (e *default-endian*))
  (write-binary-sint 4 n p e))

(define* (write-binary-sint64 n (p (current-output-port))
                              (e *default-endian*))
  (write-binary-sint 8 n p e))


;;-------------------------------------------------------------------------------
;;! nextwork encodings

;; XXXX these may be defined as aliases for the binary equivalents if
;; your architecture is big-endian.

(define (/opt o) (and (pair? o) (car o)))

(define* (read-network-uint16 (p (current-input-port)))
  (read-binary-uint 2 p 'big-endian))

(define* (read-network-uint32 (p (current-input-port)))
  (read-binary-uint 4 p 'big-endian))

(define* (read-network-uint64 (p (current-input-port)))
  (read-binary-uint 8 p 'big-endian))

(define* (read-network-sint16 (p (current-input-port)))
  (read-binary-sint 2 p 'big-endian))

(define* (read-network-sint32 (p (current-input-port)))
  (read-binary-sint 4 p 'big-endian))

(define* (read-network-sint64 (p (current-input-port)))
  (read-binary-sint 8 p 'big-endian))

(define* (write-network-uint16 x (p (current-output-port)))
  (write-binary-uint 2 x p 'big-endian))

(define* (write-network-uint32 x (p (current-output-port)))
  (write-binary-uint 4 x p 'big-endian))

(define* (write-network-uint64 x (p (current-output-port)))
  (write-binary-uint 8 x p 'big-endian))

(define* (write-network-sint16 x (p (current-output-port)))
  (write-binary-sint 2 x p 'big-endian))

(define* (write-network-sint32 x (p (current-output-port)))
  (write-binary-sint 4 x p 'big-endian))

(define* (write-network-sint64 x (p (current-output-port)))
  (write-binary-sint 8 x p 'big-endian))


;;-------------------------------------------------------------------------------
;;!! Bignum encodings -- Basic Encoding Rules (BER) from X.209

;; A BER compressed integer is an unsigned integer in base 128, most
;; significant digit first, where the high bit is set on all but the
;; final (least significant) byte.  Thus any size integer can be
;; encoded, but the encoding is efficient and small integers don't take
;; up any more space than they would in normal char/short/int encodings.

(define* (read-ber-integer (port (current-input-port)))
  (let loop ((acc 0))
    (let ((byte (read-u8 port)))
      (cond
       ((eof-object? byte) byte)        ; fail on eof
       ((< byte 128) (+ acc byte))      ; final byte is < 128
       (else
        (loop (arithmetic-shift (+ acc (bitwise-and byte 127)) 7)))))))

(define* (write-ber-integer number (port (current-output-port)))
  (let loop ((n (arithmetic-shift number -7))
	     (ls (list (bitwise-and number 127))))
    (if (zero? n)
	(for-each (lambda (b) (write-u8 b port)) ls)
	(loop (arithmetic-shift n -7)
	      (cons (+ 128 (bitwise-and n 127)) ls)))))


;;-------------------------------------------------------------------------------
;;!! Reading floating point numbers

;; Inspired by Oleg's implementation from
;;   http://okmij.org/ftp/Scheme/reading-IEEE-floats.txt
;; but removes mutations and magic numbers and allows for manually
;; specifying the endianess.
;;
;; See also
;;   http://www.cs.auckland.ac.nz/~jham1/07.211/floats.html
;; and
;;   http://babbage.cs.qc.edu/courses/cs341/IEEE-754references.html
;; as references to IEEE 754.

(define* (read-ieee-float32 (port (current-input-port))
                            (endian *default-float-endian*))
  (define (mantissa expn b2 b3 b4)
    (case expn                   ; recognize special literal exponents
      ((255)
       ;;(if (zero? (combine b2 b3 b4)) +/0. 0/0.) ; XXXX for SRFI-70
       #f)
      ((0)                              ; denormalized
       (exact->inexact (* (expt 2.0 (- 1 (+ 127 23))) (combine b2 b3 b4))))
      (else
       (exact->inexact
        (* (expt 2.0 (- expn (+ 127 23)))
           (combine (+ b2 128) b3 b4)))))) ; hidden bit
  (define (exponent b1 b2 b3 b4)
    (if (> b2 127)                  ; 1st bit of b2 is low bit of expn
	(mantissa (+ (* 2 b1) 1) (- b2 128) b3 b4)
	(mantissa (* 2 b1) b2 b3 b4)))
  (define (sign b1 b2 b3 b4)
    (if (> b1 127)                      ; 1st bit of b1 is sign
	(cond ((exponent (- b1 128) b2 b3 b4) => -) (else #f))
	(exponent b1 b2 b3 b4)))
  (let* ((b1 (read-u8 port))
	 (b2 (read-u8 port))
	 (b3 (read-u8 port))
	 (b4 (read-u8 port)))
    (if (eof-object? b4)
	b4
	(if (eq? endian 'big-endian)
	    (sign b1 b2 b3 b4)
	    (sign b4 b3 b2 b1)))))

(define* (read-ieee-float64 (port (current-input-port))
                            (endian *default-float-endian*))
  (define (mantissa expn b2 b3 b4 b5 b6 b7 b8)
    (case expn                   ; recognize special literal exponents
      ((255) #f)                 ; won't handle NaN and +/- Inf
      ((0)                       ; denormalized
       (exact->inexact (* (expt 2.0 (- 1 (+ 1023 52)))
			  (combine b2 b3 b4 b5 b6 b7 b8))))
      (else
       (exact->inexact
        (* (expt 2.0 (- expn (+ 1023 52)))
           (combine (+ b2 16) b3 b4 b5 b6 b7 b8)))))) ; hidden bit
  (define (exponent b1 b2 b3 b4 b5 b6 b7 b8)
    (mantissa (bitwise-ior (arithmetic-shift b1 4)     ; 7 bits
			   (extract-bit-field 4 4 b2)) ; + 4 bits
	      (extract-bit-field 4 0 b2) b3 b4 b5 b6 b7 b8))
  (define (sign b1 b2 b3 b4 b5 b6 b7 b8)
    (if (> b1 127)                      ; 1st bit of b1 is sign
	(cond ((exponent (- b1 128) b2 b3 b4 b5 b6 b7 b8) => -)
	      (else #f))
	(exponent b1 b2 b3 b4 b5 b6 b7 b8)))
  (let* ((b1 (read-u8 port))
	 (b2 (read-u8 port))
	 (b3 (read-u8 port))
	 (b4 (read-u8 port))
	 (b5 (read-u8 port))
	 (b6 (read-u8 port))
	 (b7 (read-u8 port))
	 (b8 (read-u8 port)))
    (if (eof-object? b8)
	b8
	(if (eq? endian 'big-endian)
	    (sign b1 b2 b3 b4 b5 b6 b7 b8)
	    (sign b8 b7 b6 b5 b4 b3 b2 b1)))))


;;-------------------------------------------------------------------------------
;;!! Writing floating point numbers
;;
;; Underflow rounds down to zero as in IEEE-754, and overflow gets
;; written as +/- Infinity.
;;
;; Break a real number down to a normalized mantissa and exponent.
;; Default base=2, mant-size=23 (52), exp-size=8 (11) for IEEE singles
;; (doubles).
;;
;; Note: This should never be used in practice, since it can be
;; implemented much faster in C.  See decode-float in ChezScheme or
;; Gauche.

;(define (call-with-mantissa&exponent num . opt)
;;  (define (last ls)
;;    (if (null? (cdr ls)) (car ls) (last (cdr ls))))
;;  (define (with-last&rest ls proc)
;;    (let lp ((ls ls) (res '()))
;;      (if (null? (cdr ls))
;;        (proc (car ls) (reverse res))
;;        (lp (cdr ls) (cons (car ls) res)))))
;;  (cond
;;    ((negative? num) (apply call-with-mantissa&exponent (- num) opt))
;;    ((zero? num) ((last opt) 0 0))
;;    (else
;;     (with-last&rest opt
;;       (lambda (proc params)
;;         (let-params* params ((base 2) (mant-size 23) (exp-size 8))
;;           (let* ((bot (expt base mant-size))
;;                  (top (* base bot)))
;;             (let loop ((n (exact->inexact num)) (e 0))
;;               (cond
;;                 ((>= n top)
;;                  (loop (/ n base) (+ e 1)))
;;                 ((< n bot)
;;                  (loop (* n base) (- e 1)))
;;                 (else
;;                  (proc (inexact->exact (round n)) e)))))))))))
;
;(define (write-ieee-float32 num . opt)
;;  (assert (real? num))
;;  (let-params* opt ((port (current-output-port))
;;                    (endian *default-float-endian*))
;;    (define (bytes)
;;      (call-with-mantissa&exponent num
;;        (lambda (f e)
;;          (let ((e0 (+ e 127 23)))
;;            (cond
;;              ((negative? e0)
;;               (let* ((f1 (inexact->exact (round (* f (expt 2 (- e0 1))))))
;;                      (b2 (extract-bit-field 7 16 f1))        ; mant:16-23
;;                      (b3 (extract-bit-field 8 8 f1))         ; mant:8-15
;;                      (b4 (extract-bit-field 8 0 f1)))        ; mant:0-7
;;                 (list (if (negative? num) 128 0) b2 b3 b4)))
;;              ((> e0 255) ; XXXX here we just write infinity
;;               (list (if (negative? num) 255 127) 128 0 0))
;;              (else
;;               (let* ((b0 (arithmetic-shift e0 -1))
;;                      (b1 (if (negative? num) (+ b0 128) b0)) ; sign + exp:1-7
;;                      (b2 (bitwise-ior
;;                           (if (odd? e0) 128 0)               ; exp:0
;;                           (extract-bit-field 7 16 f)))       ;   + mant:16-23
;;                      (b3 (extract-bit-field 8 8 f))          ; mant:8-15
;;                      (b4 (extract-bit-field 8 0 f)))         ; mant:0-7
;;                 (list b1 b2 b3 b4))))))))
;;    (for-each
;;     (lambda (b) (write-byte b port))
;
;;     (cond ((zero? num) '(0 0 0 0))
;;           ((eq? endian 'big-endian) (bytes))
;;           (else (reverse (bytes)))))))
;
;(define (write-ieee-float64 num . opt)
;;  (assert (real? num))
;;  (let-params* opt ((port (current-output-port))
;;                    (endian *default-float-endian*))
;;    (define (bytes)
;;      (call-with-mantissa&exponent num 2 52 11
;;        (lambda (f e)
;;          (let ((e0 (+ e 1023 52)))
;;            (cond
;;              ((negative? e0)
;;               (let* ((f1 (inexact->exact (round (* f (expt 2 (- e0 1))))))
;;                      (b2 (extract-bit-field 4 48 f1))
;;                      (b3 (extract-bit-field 8 40 f1))
;;                      (b4 (extract-bit-field 8 32 f1))
;;                      (b5 (extract-bit-field 8 24 f1))
;;                      (b6 (extract-bit-field 8 16 f1))
;;                      (b7 (extract-bit-field 8 8 f1))
;;                      (b8 (extract-bit-field 8 0 f1)))
;;                 (list (if (negative? num) 128 0) b2 b3 b4 b5 b6 b7 b8)))
;;              ((> e0 4095) ; infinity
;;               (list (if (negative? num) 255 127) 224 0 0 0 0 0 0))
;;              (else
;;               (let* ((b0 (extract-bit-field 7 4 e0))
;;                      (b1 (if (negative? num) (+ b0 128) b0))
;;                      (b2 (bitwise-ior (arithmetic-shift
;;                                        (extract-bit-field 4 0 e0)
;;                                        4)
;;                                       (extract-bit-field 4 48 f)))
;;                      (b3 (extract-bit-field 8 40 f))
;;                      (b4 (extract-bit-field 8 32 f))
;;                      (b5 (extract-bit-field 8 24 f))
;;                      (b6 (extract-bit-field 8 16 f))
;;                      (b7 (extract-bit-field 8 8 f))
;;                      (b8 (extract-bit-field 8 0 f)))
;;                 (list b1 b2 b3 b4 b5 b6 b7 b8))))))))
;;    (for-each
;;     (lambda (b) (write-byte b port))
;;     (cond ((zero? num) '(0 0 0 0 0 0 0 0))
;;           ((eq? endian 'big-endian) (bytes))
;;           (else (reverse (bytes)))))))
;;
;;

(define* (binary-io:call-with-mantissa&exponent num fn (base 2) (mant-size 23) (exp-size 8))
  (cond
   ((negative? num) (binary-io:call-with-mantissa&exponent
                     (- num) fn base mant-size
                     exp-size))
   ((zero? num) (fn 0 0))
   (else
    (let* ((bot (expt base mant-size))
           (top (* base bot)))
      (let loop ((n (exact->inexact num)) (e 0))
        (cond
         ((>= n top) (loop (/ n base) (+ e 1)))
         ((< n bot) (loop (* n base) (- e 1)))
         (else (fn (inexact->exact (round n)) e))))))))

(define* (write-ieee-float32 num (port (current-output-port)) (endian *default-float-endian*))
  (define (bytes)
    (binary-io:call-with-mantissa&exponent
     num
     (lambda (f e)
       (let ((e0 (+ e 127 23)))
         (cond
          ((negative? e0)
           (let* ((f1 (inexact->exact (round (* f (expt 2 (- e0 1))))))
                  (b2 (extract-bit-field 7 16 f1)) ; mant:16-23
                  (b3 (extract-bit-field 8 8 f1))  ; mant:8-15
                  (b4 (extract-bit-field 8 0 f1))) ; mant:0-7
             (list (if (negative? num) 128 0) b2 b3 b4)))
          ((> e0 255)               ; XXXX here we just write infinity
           (list (if (negative? num) 255 127) 128 0 0))
          (else
           (let* ((b0 (arithmetic-shift e0 -1))
                  (b1 (if (negative? num) (+ b0 128) b0)) ; sign + exp:1-7
                  (b2 (bitwise-ior
                       (if (odd? e0) 128 0)         ; exp:0
                       (extract-bit-field 7 16 f))) ;   + mant:16-23
                  (b3 (extract-bit-field 8 8 f))    ; mant:8-15
                  (b4 (extract-bit-field 8 0 f)))   ; mant:0-7
             (list b1 b2 b3 b4))))))))
  (for-each
   (lambda (b) (write-u8 b port))

   (cond ((zero? num) '(0 0 0 0))
         ((eq? endian 'big-endian) (bytes))
         (else (reverse (bytes))))))

(define* (write-ieee-float64 num (port (current-output-port))
                             (endian *default-float-endian*))
  (define (bytes)
    (binary-io:call-with-mantissa&exponent
     num
     (lambda (f e)
       (let ((e0 (+ e 1023 52)))
         (cond
          ((negative? e0)
           (let* ((f1 (inexact->exact (round (* f (expt 2 (- e0 1))))))
                  (b2 (extract-bit-field 4 48 f1))
                  (b3 (extract-bit-field 8 40 f1))
                  (b4 (extract-bit-field 8 32 f1))
                  (b5 (extract-bit-field 8 24 f1))
                  (b6 (extract-bit-field 8 16 f1))
                  (b7 (extract-bit-field 8 8 f1))
                  (b8 (extract-bit-field 8 0 f1)))
             (list (if (negative? num) 128 0) b2 b3 b4 b5 b6 b7 b8)))
          ((> e0 4095)                  ; infinity
           (list (if (negative? num) 255 127) 224 0 0 0 0 0 0))
          (else
           (let* ((b0 (extract-bit-field 7 4 e0))
                  (b1 (if (negative? num) (+ b0 128) b0))
                  (b2 (bitwise-ior (arithmetic-shift
                                    (extract-bit-field 4 0 e0)
                                    4)
                                   (extract-bit-field 4 48 f)))
                  (b3 (extract-bit-field 8 40 f))
                  (b4 (extract-bit-field 8 32 f))
                  (b5 (extract-bit-field 8 24 f))
                  (b6 (extract-bit-field 8 16 f))
                  (b7 (extract-bit-field 8 8 f))
                  (b8 (extract-bit-field 8 0 f)))
             (list b1 b2 b3 b4 b5 b6 b7 b8))))))
     2 52 11))
  (for-each
   (lambda (b) (write-u8 b port))
   (cond ((zero? num) '(0 0 0 0 0 0 0 0))
         ((eq? endian 'big-endian) (bytes))
         (else (reverse (bytes))))))
