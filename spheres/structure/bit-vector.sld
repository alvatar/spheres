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

(define-library (spheres/structure bit-vector)
  (export byte-not
          byte-eqv
          byte-nand
          byte-nor
          make-bit-vector
          integer->bit-vector
          bit-vector-copy
          bit-vector-grow!
          bit-vector?
          bit-vector-ref
          bit-vector-set!
          bit-vector-set
          bit-vector-length
          bit-vector-count
          bit-vector-and!
          bit-vector-and
          bit-vector-ior!
          bit-vector-ior
          bit-vector-xor!
          bit-vector-xor
          bit-vector-eqv!
          bit-vector-eqv
          bit-vector-nand!
          bit-vector-nand
          bit-vector-nor!
          bit-vector-nor
          bit-vector-shift-in-place!
          bit-vector-shift!
          bit-vector-shift
          range->bit-vector
          bit-vector-empty?
          bit-vector-full?)
  (import (spheres/algorithm u8vector))

  (include "bit-vector.scm"))
