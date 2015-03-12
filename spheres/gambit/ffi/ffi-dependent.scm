;;!!! FFI Dependent types: FFI types that contain other FFI types, creating a
;; chain of dependencies and memory management
;; .author Estevo U.C. Castro, 2013
;;
;; The MIT License (MIT)
;;
;; Copyright (c) 2013 Estevo U.C. Castro
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(##namespace ("ffi-types-impl#"))
(##include "~~/lib/gambit#.scm")
(##include "ffi-dependent#.scm")


(define refs (make-table test: =))

(define (cleanup-dependencies!)
  (declare (not interrupts-enabled))
  (let ((to-remove '()))
    (table-for-each
     (lambda (k v)
       (if (not (table-ref ##serial-number-to-object-table k #f))
           (set! to-remove (cons (cons k v) to-remove))))
     refs)
    (for-each
     (lambda (pair)
       (let ((serial-number (car pair))
             (roots (cdr pair)))
         (for-each root-decref! roots)
         (table-set! refs serial-number)))
     to-remove)))

(##add-gc-interrupt-job! cleanup-dependencies!)

(define (register-root! object incref! decref!)
  (declare (not interrupts-enabled))
  (let ((serial (object->serial-number object)))
    (if (not (table-ref refs serial #f))
        (begin
          (incref!)
          (table-set! refs serial (list (make-root incref! decref!)))))))

(define (register-rc-root! object)
  (let ((void* (object->void* object)))
    (register-root!
     object
     (lambda () (addref-rc! void*))
     (lambda () (release-rc! void*)))))

(define (register-dependency! dependent object)
  (declare (not interrupts-enabled))
  (let* ((dependent-serial (object->serial-number dependent))
         (object-serial (object->serial-number object))
         (roots (table-ref refs object-serial #f)))
    (if roots
        (begin
          (for-each root-incref! roots)
          (table-set! refs
                      dependent-serial
                      (append roots
                              (table-ref refs dependent-serial '()))))
        (error (list "No such root: " object)))))

(define make-root cons)

(define (root-incref! root)
  ((car root)))

(define (root-decref! root)
  ((cdr root)))

(define object->void*
  (c-lambda (scheme-object) (pointer void #f)
            "___result_voidstar = (void*)___FIELD(___arg1, ___FOREIGN_PTR);"))

(define addref-rc!
  (c-lambda ((pointer void)) void
            "___EXT(___addref_rc)(___arg1_voidstar);"))

(define release-rc!
  (c-lambda ((pointer void)) void
            "___EXT(___release_rc)(___arg1_voidstar);"))
