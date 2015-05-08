;;!!! A very simple fixed-size circular vector
;; .author Per Eckerdal
;; .author Alvaro Castro-Castilla, 2015

(define-type circular-vector
  constructor: make-circular-vector-internal
  (vec read-only:)
  pos)

(define* (make-circular-vector size (init 0))
  (make-circular-vector-internal
   (make-vector size init)
   0))

(define (circular-vector-length v)
  (vector-length
   (circular-vector-vec v)))

(define (circular-vector-fill! v val)
  (vector-fill! (circular-vector-vec v)
                val))

(define (circular-vector-push! v val)
  (let* ((vec (circular-vector-vec v))
         (new-pos (modulo (+ 1 (circular-vector-pos v))
                          (vector-length vec))))
    (circular-vector-pos-set! v new-pos)
    (vector-set! vec new-pos val)))

(define (circular-vector-ref v pos)
  (let ((vec (circular-vector-vec v)))
    (vector-ref vec
                (modulo pos
                        (vector-length vec)))))

(define (circular-vector-first v)
  (circular-vector-ref v 0))

(define (circular-vector-last v)
  (circular-vector-ref v -1))
