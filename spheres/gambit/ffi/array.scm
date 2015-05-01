;;!!! FFI arrays
;; .author Álvaro Castro Castilla, 2013-2015. All Rights Reserved.

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

;;------------------------------------------------------------------------------

(include "macros.scm")
(include "types.scm")

;;! C arrays

(c-declare "#include <stdint.h>")
(c-declare "#include <stdlib.h>")

;;! char
(c-define-sizeof char)
(c-define-array char
                scheme-vector: s8)

;; ;;! unsigned char
(c-define-sizeof unsigned-char c-type: "unsigned char")
(c-define-array unsigned-char
                c-type: "unsigned char"
                scheme-vector: u8)

;;! short
(c-define-sizeof short)
(c-define-array short
                scheme-vector: s16)

;;! unsigned short
(c-define-sizeof unsigned-short c-type: "unsigned short")
(c-define-array unsigned-short
                c-type: "unsigned short"
                scheme-vector: u16)

;;! int
(c-define-sizeof int)
(c-define-array int
                scheme-vector: s32)

;;! unsigned int
(c-define-sizeof unsigned-int c-type: "unsigned int")
(c-define-array unsigned-int
                c-type: "unsigned int"
                scheme-vector: u32)

;;! long
(c-define-sizeof long)
(c-define-array long
                scheme-vector: s64)

;;! unsigned long
(c-define-sizeof unsigned-long c-type: "unsigned long")
(c-define-array unsigned-long
                c-type: "unsigned long"
                scheme-vector: u64)

;;! float
(c-define-sizeof float)
(c-define-array float
                scheme-vector: f32)

;;! double
(c-define-sizeof double)
(c-define-array double
                scheme-vector: f64)
