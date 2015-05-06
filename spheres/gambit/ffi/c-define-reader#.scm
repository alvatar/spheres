;;!!! C Types: readers/writers generation

;;! Macros for defining readers and writers.
;; .author Marco Benelli
;; ref: https://mercure.iro.umontreal.ca/pipermail/gambit-list/2009-June/003671.html
(define-macro (define-writer name vtype)
  `(define (,name x #!optional (port (current-output-port)))
     (let ((v (,vtype x)))
       (##subtype-set! v u8vector-subtype)
       (write-subu8vector v 0 (u8vector-length v) port))))

(define-macro (define-reader name vtype vsubtype vtype-ref init)
  `(define (,name #!optional (port (current-input-port)))
     (let ((v (,vtype ,init)))
       (##subtype-set! v u8vector-subtype)
       (let ((n (read-subu8vector v 0 (u8vector-length v) port)))
         (if (= n (u8vector-length v))
             (begin
               (##subtype-set! v ,vsubtype)
               (,vtype-ref v 0))
             #!eof)))))
