;;!!! SRFI-41: Streams (primitives)
;; .author Philip L. Bewig (2007). All Rights Reserved.
;; Modifications:
;; .author Kon Lovett 2009
;; .author Álvaro Castro-Castilla 2012-2014


(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

;;!! Streams primitives

(cond-expand
 (gambit
  (define-type stream-type
    constructor: make-stream
    predicate: stream?
    (promise stream-promise stream-promise!)))
 (else
  (error "TODO: implement stream type")))

(define (stream-eager expr)
  (make-stream
   (cons 'eager expr)))

(define (stream-force promise)
  (let ((content (stream-promise promise)))
    (case (car content)
      ((eager) (cdr content))
      ((lazy)  (let* ((promise* ((cdr content)))
                      (content  (stream-promise promise)))
                 (if (not (eqv? (car content) 'eager))
                     (begin (set-car! content (car (stream-promise promise*)))
                            (set-cdr! content (cdr (stream-promise promise*)))
                            (stream-promise! promise* content)))
                 (stream-force promise))))))

(define stream-null (stream-delay (cons 'stream 'null)))

(cond-expand
 (gambit
  (define-type stream-pare
    constructor: make-stream-pare
    predicate: stream-pare?
    read-only: (kar stream-kar)
    read-only: (kdr stream-kdr)))
 (else
  (error "TODO: implement stream type")))

(define (stream-pair? obj)
  (and (stream? obj) (stream-pare? (stream-force obj))))

(define (stream-null? obj)
  (and (stream? obj)
       (eqv? (stream-force obj)
             (stream-force stream-null))))

(define (stream-car strm)
  (cond ((not (stream? strm)) (error 'stream-car "non-stream"))
        ((stream-null? strm) (error 'stream-car "null stream"))
        (else (stream-force (stream-kar (stream-force strm))))))

(define (stream-cdr strm)
  (cond ((not (stream? strm)) (error 'stream-cdr "non-stream"))
        ((stream-null? strm) (error 'stream-cdr "null stream"))
        (else (stream-kdr (stream-force strm)))))
