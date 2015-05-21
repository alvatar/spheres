;;!!! Statistics procedures
;; .author Alvaro Castro-Castilla, 2015

;;! mean
(define (mean sequence)
  (if (null? sequence)
      0
      (/ (fold + 0 sequence) (length sequence))))

;;! median
(define (median sequence)
  (percentile sequence 50))

;;! mode
(define (mode sequence)
  (if (null? sequence)
      (error "mode: null sequence")
      (let ((count-table (make-hash-table eqv?))
            (modes '())
            (mode-count 0))
        (for-each (lambda (item)
                    (hash-table-set! count-table
                                     item
                                     (+ 1 (hash-table-ref count-table item (lambda () 0)))))
                  sequence)
        (for-each (lambda (key)
                    (let ((val (hash-table-ref count-table key (lambda () #f))))
                      (cond ((> val mode-count) ; keep mode
                             (set! modes (list key))
                             (set! mode-count val))
                            ((= val mode-count) ; store multiple modes
                             (set! modes (cons key modes))))))
                  (hash-table-keys count-table))
        (values modes mode-count))))

;;! geometric-mean
(define* (geometric-mean sequence (base 10))
  (expt base (mean (map (lambda (x) (/ (log x)
                                  (log base)))
                        sequence))))

;;! range
(define (range sequence)
  (if (null? sequence)
      0
      (- (fold max (car sequence) (cdr sequence))
         (fold min (car sequence) (cdr sequence)))))

;;! percentile
(define (percentile sequence percent)
  (if (null? sequence)
      (error "percentile: null sequence")
      (let* ((sorted-vec (apply vector (sort sequence <)))
             (n (vector-length sorted-vec))
             (k (* n (/ percent 100)))
             (floor-k (floor k)))
        (if (= k floor-k)
            (/ (+ (vector-ref sorted-vec k)
                  (vector-ref sorted-vec (- k 1)))
               2)
            (vector-ref sorted-vec floor-k)))))

;;! variance
(define (variance sequence)
  (if (< (length sequence) 2)
      (error "variance: sequence must contain at least two elements")
      (let ((mean1 (mean sequence)))
        (/ (fold + 0 (map (lambda (x) (square (- mean1 x))) sequence))
           (- (length sequence) 1)))))

;;! standard-deviation
(define (standard-deviation sequence)
  (sqrt (variance sequence)))

;;! coefficient-of-variation
(define (coefficient-of-variation sequence)
  (* 100 (/ (standard-deviation sequence)
            (mean sequence))))

;;! standard-error-of-the-mean
(define (standard-error-of-the-mean sequence)
  (/ (standard-deviation sequence)
     (sqrt (length sequence))))
