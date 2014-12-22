;;!!! FFI object serialization
;; .author Álvaro Castro Castilla, 2013-2014. All Rights Reserved.

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

;;------------------------------------------------------------------------------

(include "macros.scm")

;;!! C Types: readers/writers
;;
;; Use like this:
;; (call-with-output-file
;;      "f64test"
;;    (lambda (port)
;;      (write-f64 -1.5 port)
;;      (write-f64 +inf.0 port)
;;      (write-f64 3.1415926 port)))
;;
;; (call-with-input-file
;;      "f64test"
;;    (lambda (port)
;;      (let* ((a (read-f64 port))
;;             (b (read-f64 port))
;;             (c (read-f64 port))
;;             (d (read-f64 port)))
;;        (pp (list a b c d)))))

;;!! C Types: readers/writers generation

(define u8vector-subtype (##subtype (u8vector)))
(define s8vector-subtype (##subtype (s8vector)))
(define u16vector-subtype (##subtype (u16vector)))
(define s16vector-subtype (##subtype (s16vector)))
(define u32vector-subtype (##subtype (u32vector)))
(define s32vector-subtype (##subtype (s32vector)))
(define u64vector-subtype (##subtype (u64vector)))
(define s64vector-subtype (##subtype (s64vector)))
(define f32vector-subtype (##subtype (f32vector)))
(define f64vector-subtype (##subtype (f64vector)))

;;! u8vector reader/writer
;; Already defined in Gambit

;;! s8vector reader/writer
(define-writer write-s8 s8vector)
(define-reader read-s8 s8vector s8vector-subtype s8vector-ref 0)

;;! u16vector reader/writer
(define-writer write-u16 u16vector)
(define-reader read-u16 u16vector u16vector-subtype u16vector-ref 0)

;;! s16vector reader/writer
(define-writer write-s16 s16vector)
(define-reader read-s16 s16vector s16vector-subtype s16vector-ref 0)

;;! u32vector reader/writer
(define-writer write-u32 u32vector)
(define-reader read-u32 u32vector u32vector-subtype u32vector-ref 0)

;;! s32vector reader/writer
(define-writer write-s32 s32vector)
(define-reader read-s32 s32vector s32vector-subtype s32vector-ref 0)

;;! u64vector reader/writer
(define-writer write-u64 u64vector)
(define-reader read-u64 u64vector u64vector-subtype u64vector-ref 0)

;;! s64vector reader/writer
(define-writer write-s64 s64vector)
(define-reader read-s64 s64vector s64vector-subtype s64vector-ref 0)

;;! f32vector reader/writer
(define-writer write-f32 f32vector)
(define-reader read-f32 f32vector f32vector-subtype f32vector-ref 0.0)

;;! f64vector reader/writer
(define-writer write-f64 f64vector)
(define-reader read-f64 f64vector f64vector-subtype f64vector-ref 0.0)


;;------------------------------------------------------------------------------

;;!! FFI types serialization

;;! Adds ability to properly serialize FFI types on (write) and properly deserialize them on (read).
;; .author Mikael More. MIT License.
;; ref: https://mercure.iro.umontreal.ca/pipermail/gambit-list/2013-March/006510.html
;;
;; Usage:
;; (ffi-write-transformer-add!
;;  'name-of-your-ffi-type  
;;  (lambda (v) `(name-of-constructor-procedure-to-create-an-instance-of-this-ffi-type
;;                [ arguments needed to constructor to produce an instance exactly like
;;                the one in the v variable ])))
;; Example:
;;
;; (c-declare #<<end-of-c-declare
;; #include <stdlib.h>
;; typedef struct { int x, y; } point;
;; point *make_point( int x, int y ) {
;;   point *p = ___CAST(point*, malloc(sizeof(point)));
;;   p->x = x;
;;   p->y = y;
;;   return p;
;; }
;; int point_x( point* p ) { return p->x; }
;; int point_y( point* p ) { return p->y; }
;; end-of-c-declare
;; )
;;
;; (c-define-type point "point")
;; (c-define-type point* (pointer point))
;; (define make-point (c-lambda (int int) point* "make_point"))
;; (define point-x (c-lambda (point*) int "point_x"))
;; (define point-y (c-lambda (point*) int "point_y"))
;;
;; (ffi-write-transformer-add! 'point* (lambda (v) `(make-point ,(point-x v) ,(point-y v))))
;; REPL will show:
;; #.(make-point 2 1)
;; Instead of
;; #<point* #2 0x1160a90>

;; Serialize:
;; (object->string (make-point 3 4)) 
;; Deserialize:
;; (string->object "#.(make-point 2 1)")

(define *ffi-writer-transformers* (make-table test: eq?))
(define *writer-default* #f)

(define (ffi-write-transformer-add! type serializer-proc)
  (table-set! *ffi-writer-transformers* type serializer-proc))

(define (%%sexp-ext:wr we obj)
  (if (##foreign? obj)
      (let* ((name (let ((v (foreign-tags obj)))
                     (and (pair? v) (car v))))
             (transformer (table-ref *ffi-writer-transformers* name #f)))
        (if transformer
            (let ((transformed-to (transformer obj)))
              (##wr-str we "#.")
              ;; (##wr-pair we transformed-to) - transformed-to may be sth else for instance a symbol
              ;; so instead go with the universal:
              (*writer-default* we transformed-to))
            (##wr-foreign we obj)))
      (*writer-default* we obj)))

;;! Initialize FFI serialization extension
(define (%%sexp-ext-install!)
  (and (not (eq? *writer-default* ##wr))
       (begin
         (set! *writer-default* ##wr)
         (set! ##wr %%sexp-ext:wr)
         (let* ((port (repl-input-port))
                (rt (input-port-readtable port)))
           ;; (##readtable-char-sharp-handler-set! rt #\< sexp-ext:read-sharp-less)
           (input-port-readtable-set! port (readtable-eval-allowed?-set rt #t))
           (void)))))
;;! Call to initialize FFI serialization extension
(%%sexp-ext-install!)

;;! Object from string
(define (string->object s)
  (call-with-input-string
   s
   (lambda (port)
     (input-port-readtable-set!
      port (readtable-eval-allowed?-set (input-port-readtable port) #t))
     (read port))))

