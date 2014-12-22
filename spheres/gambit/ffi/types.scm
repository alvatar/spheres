;;!!! FFI Type definitions
;; .author Alvaro Castro-Castill, 2013-2015. All rights reserved.

;;! size-t
(c-define-type size-t unsigned-int) ;; This should be more than enough, keep in mind

;;! void*
(c-define-type* void)
(c-define-type void** (pointer void*))

;;! char*
(c-define-type* char)
(c-define-type char** (pointer char*))

;;! unsigned char*
(c-define-type* unsigned-char)
(c-define-type unsigned-char** (pointer unsigned-char*))

;;! short*
(c-define-type* short)
(c-define-type short** (pointer short*))

;;! unsigned short*
(c-define-type* unsigned-short)
(c-define-type unsigned-short** (pointer unsigned-short*))

;;! int*
(c-define-type* int)
(c-define-type int** (pointer int*))

;;! unsigned int*
(c-define-type* unsigned-int)
(c-define-type unsigned-int** (pointer unsigned-int*))

;;! long*
(c-define-type* long)
(c-define-type long** (pointer long*))

;;! unsigned long*
(c-define-type* unsigned-long)
(c-define-type unsigned-long** (pointer unsigned-long*))

;;! float*
(c-define-type* float)
(c-define-type float** (pointer float*))

;;! double*
(c-define-type* double)
(c-define-type double** (pointer double*))

;;! int8
(c-define-type* int8)
(c-define-type int8** (pointer int8*))

;;! unsigned-int8
(c-define-type* unsigned-int8)
(c-define-type unsigned-int8** (pointer unsigned-int8*))

;;! int16
(c-define-type* int16)
(c-define-type int16** (pointer int16*))

;;! unsinged-int16
(c-define-type* unsigned-int16)
(c-define-type unsigned-int16** (pointer unsigned-int16*))

;;! int32
(c-define-type* int32)
(c-define-type int32** (pointer int32*))

;;! unsigned-int32
(c-define-type* unsigned-int32)
(c-define-type unsigned-int32** (pointer unsigned-int32*))

;;! int64
(c-define-type* int64)
(c-define-type int64** (pointer int64*))

;;! unsigned-int64
(c-define-type* unsigned-int64)
(c-define-type unsigned-int64** (pointer unsigned-int64*))

;;! float32
(c-define-type* float32)
(c-define-type float32** (pointer float32*))

;;! float64
(c-define-type* float64)
(c-define-type float64** (pointer float64*))
