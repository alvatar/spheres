;; Smith Waterman string matching algorithm Scheme version (direct
;; translation of the Erlang version)
;;
;; Orginal Erlang version by Alexander Jean-Claude Bottema
;; (alexb@csd.uu.se)

(include "bench.scm")

(define (alpha-beta-penalty a b)
  (max (- a 4)
       (- b 1)))

(define (generate-sequence len r)
  (if (= len 0)
      '()
      (cons (remainder r 10)
            (generate-sequence (- len 1)
                               (remainder
                                (+ (* r 11) 1237501) 10067)))))

(define (generate-sequences n len r)
  (if (= n 0)
      '()
      (cons
       (generate-sequence len r)
       (generate-sequences (- n 1) len (+ r 1)))))

(define (match-entry top side upper-left upper left)
  (let ((me-left (alpha-beta-penalty (vector-ref left 2)
                                     (vector-ref left 0)))
        (me-upper (alpha-beta-penalty (vector-ref upper 2)
                                      (vector-ref upper 1))))
    (let ((me-upper-left
           (max me-left
                me-upper
                (+ (vector-ref upper-left 2) (if (= top side) 1 0))
                0)))
      (vector me-left
              me-upper
              me-upper-left
              (max me-upper-left
                   (vector-ref left 3)
                   (vector-ref upper 3)
                   (vector-ref upper-left 3))))))

(define (match-zero-entry top side v)
  (let ((left       (vector-ref v 0))
        (upper-left (vector-ref v 2))
        (max*       (vector-ref v 3)))
   (let* ((e-left (alpha-beta-penalty upper-left left))
          ;; (weight (max (- 1 (abs (- side top))) 0))
          (e-upper-left (max e-left (- 1 (abs (- side top))) 0))
          (e-max (max max* e-upper-left 0)))
     (vector e-left -1 e-upper-left e-max))))

(define (match* tops side prev upper-left left)
  (match0 tops side prev upper-left left '() 'none))

(define (match0 tops side prev upper-left left acc last)
  (if (null? tops)
      (cons acc last)
      (let ((top (car tops))
            (tops (cdr tops)))
        (if (eq? prev 'none)
            (let ((e (match-zero-entry top side left)))
              (match0 tops side 'none upper-left e (cons e acc) e))
            (let ((upper (car prev))
                  (prev (cdr prev)))
              (let ((e (match-entry top side upper-left upper left)))
                (match0 tops side prev upper e (cons e acc) e)))))))

(define (match-two-seq side top prev)
  (match-two-seq0 side top prev 'none))

(define (match-two-seq0 side top prev acc)
  (if (null? side)
      acc
      (let ((s (car side))
            (side (cdr side)))
        (let ((tmp (match* top s prev (vector 0 0 0 0) (vector 0 0 0 0))))
          (let ((row (car tmp))
                (result (cdr tmp)))
            (match-two-seq0 side top row result))))))

(define (match-sequences tops side)
  (match-sequences0 tops side -9999999))

(define (match-sequences0 tops side current-result)
  (if (null? tops)
      current-result
      (let ((top (car tops))
            (tops (cdr tops)))
        (let ((result (vector-ref (match-two-seq top side 'none) 3)))
          (match-sequences0 tops side (max current-result result))))))

(define (main n)
  (let ((n (string->number n)))
    (let ((tops (generate-sequences n 32 1))
          (side (generate-sequence 32 0)))
      (write `(smith termite ,n ,(time* (match-sequences tops side))))
      (newline))))
