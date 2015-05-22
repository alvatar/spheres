;;!!! Procedures for numerical interpolation
;; .author Alvaro Castro-Castilla, 2012-2015


;;-------------------------------------------------------------------------------
;;!! Integer interpolation

;; '(0 4 8 6 2 5) -> '(0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 3 4 5)
(define (range-expand l)
  (let recur ((rest l))
    (if (null? (cdr rest))
        (list (car rest))
        (let* ((first (car rest))
               (second (cadr rest))
               (generator
                (if (< first second)
                    (lambda (x) (+ x 1))
                    (lambda (x) (- x 1)))))
          (assure (and (integer? first) (integer? second))
                  (error "Element in list is not an integer number"))
          (let generate ((n first))
            (if (= n second)
                (recur (cdr rest))
                (cons n (generate
                         (generator n)))))))))

;; '(0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 3 4 5) -> (0 8 2 5)
(define (range-extract lst)
  (if (null? lst)
      (error "range-extract: list needs to be of length 1 at least"))
  (let recur ((range (list (car lst)))
              (prev (car lst))
              (lst (cdr lst)))
    (if (null? lst)
        (reverse (cons prev range))
        (let ((head (car lst)))
          (if (> head prev)
              ;; previous value is lower
              (if (> head (car range))
                  (recur range
                         head
                         (cdr lst))
                  ;; capture new local max
                  (recur (cons prev range)
                         head
                         (cdr lst)))
              ;; previous value is higher
              (if (< head (car range))
                  (recur range
                         head
                         (cdr lst))
                  ;; capture new local max
                  (recur (cons prev range)
                         head
                         (cdr lst))))))))


;;-------------------------------------------------------------------------------
;;!! Uni-dimensional interpolation

;; Rounds off the index k
(define (interpolate/nearest vl k)
  (let ((ref-proc (cond ((vector? vl) vector-ref)
                        ((list? vl) list-ref)
                        (else (error "interpolate/nearest: input should be a list or vector")))))
    (ref-proc vl (inexact->exact (round k)))))

;; Interpolates linearly an inexact index k
(define (interpolate/linear vl k)
  (let ((ref-proc (cond ((vector? vl) vector-ref)
                        ((list? vl) list-ref)
                        (else (error "interpolate/linear: input should be a list or vector")))))
    (let ((previous (ref-proc vl (inexact->exact (floor k))))
          (next (ref-proc vl (inexact->exact (ceiling k))))
          (decimal-part (- k (floor k))))
      (if (zero? decimal-part)
          previous
          (+ previous (* decimal-part (- next previous)))))))
