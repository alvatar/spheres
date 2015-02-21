;;; this is a distribution that correspond to expected intervals 
;;; between events with a poisson distribution
;;; l is lambda that is the mean of the distribution
;;; If for example you want to simulate the event arrival of random
;;; indipendent events, like the requests to a web server 
;;; with a mean of 4 request per second
;;; you can simulate it by generating this, delaing for this time
;;; and create a new request.

(define current-case-limit (make-parameter 100))

(define pi 3.14159265)

(define (memoize fn size)
  (let((cache (make-vector size #!void)))
    (lambda (j)
      (let((v0 (vector-ref cache j)))
        (if (eq? v0 #!void)
            (let((v (fn j)))
              (vector-set! cache j v)
              v)
            v0)))))

(define (range j)
  (let range ((j j) (rs '()))
    (if (= 0 j) rs
        (let((j (- j 1)))
          (range j (cons j rs))))))

(define (fold f i es)
  (let fold ((i i) (es es))
    (if (null? es) i
        (fold (f i (car es)) (cdr es)))))

(define (sum fn n)
  (fold (lambda (i e) (+ i (fn e)))
        0
        (range n)))

(define beta (/ (sqrt pi) 2))

(define (quantile:normal #!optional (number-of-terms 10))
  (let((calc-c #f)
       (calc-weight #f)
       (weight #f)
       (term #f)
       (ks #f)
       (c #f))
    (set! calc-c 
          (lambda (k)
            (if (< k 2) 1
                (sum (lambda (m) 
                       (/ (* (c m) (c (- k 1 m)))
                          (* (+ m 1) (+ (* 2 m) 1))))
                     k))))
  
    (set! calc-weight 
          (lambda (k) 
            (/ (c k) (+ (* 2 k) 1))))

    (set! term
          (lambda (k p)
            (expt (* beta p) (+ (* 2 k) 1))))
    
    (set! weight (memoize calc-weight number-of-terms))
    (set! c (memoize calc-c number-of-terms))
    
    (lambda (p) 
      (sum (lambda (k) (* (weight k) (term k (- p 0.5)))) number-of-terms))))

(define (quantile:exponential l)
  (lambda (p)
    (- (/ (log (- 1 p)) l))))

(define (quantile:uniform p) p)

(define (real<< #!key 
                (distribution quantile:uniform) 
                (cases 100))
  (lambda (yield)
    (let real ((c 0))
      (if (< c cases)
          (begin
            (yield (distribution (random-real)))
            ;; (yield (distribution (/ (random-integer 100000) 100000)))
            (real (+ c 1)))))))

(define (normal<< #!key 
                  (cases 100)
                  (term-count 5))
  (real<< distribution: (quantile:normal term-count)
          cases: cases))

(define (exponential<< #!key
                       (mean 1) 
                       (cases (current-case-limit)))
  (real<< distribution: (quantile:exponential (/ 1 mean))
        cases: cases))


(define (a-real #!key 
                (distribution quantile:uniform) 
                (cases 100))
  (generate! (real<< distribution: distribution
                     cases: cases)))

(define (a-normal #!key 
                (cases 100)
                (term-count 5))
  (generate! (normal<< cases: cases
                       term-count: term-count)))

(define (an-exponential #!key 
                        (mean 1) 
                        (cases (current-case-limit)))
  (generate! (exponential<< mean: mean
                            cases: cases)))
