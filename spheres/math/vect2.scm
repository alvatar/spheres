;;!!! Vector (dimension 2)
;; .author Alvaro Castro-Castilla, 2012-2015



;;! Default accuracy for inexact comparisons
(define equal-accuracy 0.000001)

;;!! vect2 type
(define-structure vect2 x y)

;;! Vector addition
(define (vect2:+vect2 v1 v2)
  (make-vect2 (+ (vect2-x v1)
                 (vect2-x v2))
              (+ (vect2-y v1)
                 (vect2-y v2))))

;; (define-syntax vect2+
;;   (syntax-rules ()
;;     ((_ v)
;;      v)
;;     ((_ v1 v2)
;;      (vect2:+vect2 v1 v2))
;;     ((_ v1 v2 v3 ...)
;;      (vect2+ (vect2:+vect2 v1 v2) v3 ...))))

;;! Vector substraction
(define (vect2:-vect2 v1 v2)
  (make-vect2 (- (vect2-x v1)
                 (vect2-x v2))
              (- (vect2-y v1)
                 (vect2-y v2))))

;;! Vector dot product
(define (vect2:*vect2 v1 v2)
  (make-vect2 (* (vect2-x v1)
                 (vect2-x v2))
              (* (vect2-y v1)
                 (vect2-y v2))))

;;! Vector * scalar
(define (vect2:*scalar v a)
  (make-vect2 (* (vect2-x v) a)
              (* (vect2-y v) a)))

;;! Vector / scalar
(define (vect2:/scalar v a)
  (make-vect2 (/ (vect2-x v) a)
              (/ (vect2-y v) a)))

;;! Equality predicate
(define (vect2:=vect2 v1 v2)
  (and (= (vect2-x v1)
          (vect2-x v2))
       (= (vect2-y v1)
          (vect2-y v2))))

;;! Are these vectors proportional?
(define (vect2:proportional-vect2? v1 v2)
  (let ((v1x (vect2-x v1)) (v1y (vect2-y v1))
        (v2x (vect2-x v2)) (v2y (vect2-y v2)))
    (cond ((zero? v2x) (zero? v1x))
          ((zero? v2y) (zero? v1y))
          (else (= (/ v1x v2x)
                   (/ v1y v2y))))))

;;! Zero vector
(define (vect2:make-zero)
  (make-vect2 0 0))

;;! Calculate squared vector length
(define (vect2:squared-magnitude vec)
  (+ (square (vect2-x vec))
     (square (vect2-y vec))))

;;! Calculate the symmetric vector
(define (vect2:symmetric vec)
  (make-vect2 (- (vect2-x vec))
              (- (vect2-y vec))))

;;! Calculate x projection
(define (vect2:x-projection vec)
  (make-vect2 (vect2-x vec)
              0))

;;! Calculate y projection
(define (vect2:y-projection vec)
  (make-vect2 0
              (vect2-y vec)))

;;! Get the component that is max
(define (vect2:max-component vec)
  (max (vect2-x vec)
       (vect2-y vec)))

;;! Get the component that is min
(define (vect2:min-component vec)
  (min (vect2-x vec)
       (vect2-y vec)))

;;; Calculate x/y ratio
(define (vect2:x/y vec)
  (/ (vect2-x vec)
     (vect2-y vec)))

;;! Calculate y/x ratio
(define (vect2:y/x vec)
  (/ (vect2-y vec)
     (vect2-x vec)))

;;! Absolute vector
(define (vect2:abs vec)
  (make-vect2 (abs (vect2-x vec))
              (abs (vect2-y vec))))

;;! Make a vector with each component becoming its inverse
(define (vect2:inverses vec)
  (make-vect2 (##inverse (vect2-x vec))
              (##inverse (vect2-y vec))))

;;! Clamp to low and high vect2
(define (vect2:clamp-vect2 vec lo-vec hi-vec)
  (make-vect2 (max (vect2-x lo-vec)
                   (min (vect2-x vec)
                        (vect2-x hi-vec)))
              (max (vect2-y lo-vec)
                   (min (vect2-y vec)
                        (vect2-y hi-vec)))))

;;! Clamp to low and high scalars
(define (vect2:clamp-scalars vec lo hi)
  (make-vect2 (max lo (min (vect2-x vec) hi))
              (max lo (min (vect2-y vec) hi))))


;-------------------------------------------------------------------------------
;!! Inexact operations

;;! Exact conversion
(define (vect2:inexact->exact v)
  (make-vect2 (inexact->exact (vect2-x v))
              (inexact->exact (vect2-y v))))

;;! Inexact conversion
(define (vect2:exact->inexact v)
  (make-vect2 (exact->inexact (vect2-x v))
              (exact->inexact (vect2-y v))))

;;! Equality predicate
(define* (vect2:~= v1 v2 (accuracy: equal-accuracy))
  (define (~= a b equal-accuracy)
    (and (>= a (- b equal-accuracy))
         (<= a (+ b equal-accuracy))))
  (and (~= (vect2-x v1)
           (vect2-x v2)
           accuracy)
       (~= (vect2-y v1)
           (vect2-y v2)
           accuracy)))

;;! Make an inexact zero vector
(define (vect2:~zero)
  (make-vect2 0.0 0.0))

;;! Square root function of a vector
(define (vect2:~sqrt vec)
  (make-vect2 (sqrt (vect2-x vec))
              (sqrt (vect2-y vec))))

;;! Calculate vector length
(define (vect2:~magnitude vec)
  (define (square x) (* x x))
  (sqrt (+ (square (vect2-x vec))
           (square (vect2-y vec)))))

;;! Normalize vector
(define (vect2:~normalize vec)
  (let ((div (vect2:~magnitude vec)))
    (make-vect2 (/ (vect2-x vec) div)
                (/ (vect2-y vec) div))))

;;! Utility procedure to invert each one of the components of the vector
(define (vect2:~inverses vec)
  (make-vect2 (/ 1.0 (vect2-x vec))
              (/ 1.0 (vect2-y vec))))


;;-------------------------------------------------------------------------------
;;!! Random

;;! Random exact vector (range 0 -> 1)
(define (vect2:random)
  (make-vect2 (inexact->exact (random-real))
              (inexact->exact (random-real))))

;;! Random exact vector (range -1 -> 1)
(define (vect2:random-symmetric)
  (make-vect2 (+ -1 (* (inexact->exact (random-real)) 2))
              (+ -1 (* (inexact->exact (random-real)) 2))))

;;! Random exact vector (range 0 -> 1)
(define (vect2:~random)
  (make-vect2 (random-real)
              (random-real)))

;;! Random vect2 vector (inexact, range -1.0 -> 1.0)
(define (vect2:~random-symmetric)
  (make-vect2 (fl+ -1.0 (fl* (random-real) 2.0))
              (fl+ -1.0 (fl* (random-real) 2.0))))
