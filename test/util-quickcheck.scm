(load '(spheres/util quickcheck))


(println "**************\nTest fail\n\n")

(define (fail)
  (let((a (any-of 0 2 4 5 6 10 12 13 14 16 18 20 21)))
    (assert! (even? a))))

(test! (fail))


;;-------------------------------------------------------------------------------

(display "\n\n**************\nTest sort\n\n")

(define (mergesort lt? es)
  (define (split es)
    (let split ((es es) (as '()) (bs '()))
      (if (null? es) (cons as bs)
          (split (cdr es)
                 bs
                 (cons (car es) as)))))
  (define (merge lt? as bs)
    (let merge ((as as) (bs bs) (rs '()))
      (cond
       ((and (null? as) (null? bs))
        (reverse rs))
       ((null? as)
        (merge as (cdr bs) (cons (car bs) rs)))
       ((null? bs)
        (merge (cdr as) bs (cons (car as) rs)))
       (else
        (let((a (car as))
             (b (car bs)))
          (if (lt? a b)
              (merge (cdr as) bs (cons a rs))
              (merge as (cdr bs) (cons b rs))))))))
  (let mergesort ((es es))
    (cond
     ((null? es) es)
     ((null? (cdr es)) es)
     (else (let*((ab (split es))
                 (a (mergesort  (car ab)))
                 (b (mergesort (cdr ab))))
             (merge lt? a b))))))

(define (quicksort lt? lst)

  (define (filter test? ls)
    (let filter ((ls ls) (rs '()))
      (cond
       ((null? ls) (reverse rs))
       ((test? (car ls)) (filter (cdr ls) (cons (car ls) rs)))
       (else (filter (cdr ls) rs)))))
  (let quicksort ((lst lst))
    (if (null? lst) '()
        (let*((pivot (car lst))
              (left (filter (lambda (j) (lt? j pivot)) (cdr lst)))
              (right (filter (lambda (j) (not (lt? j pivot))) (cdr lst))))
          (append
           (quicksort left)
           (list pivot)
           (quicksort right))))))


(random-source-randomize! default-random-source)

(define (a-random-list #!key (cases 100))
  (let((len (in-range stop: cases)))
    (let loop ((j 0) (rs '()))
      (if (>= j len) rs
          (loop (+ j 1) (cons (random-integer (* 10 len))
                              rs))))))

(define (test-sort fn)
  (let*((unord (or (a-random-list cases: 100)
                   (any-of '(0))))
        (ord (fn < unord)))
    (assert! (all? (lambda (x) (member x ord)) unord))
    (assert! (all? (lambda (x) (member x unord)) ord))
    (let*((len (length ord))
	  (i (in-range stop: len))
	  (j (in-range stop: len)))
      (assert! (=> (>= i j)
		   (>= (list-ref ord i)
		       (list-ref ord j)))))))

(test! (test-sort mergesort))


;;-------------------------------------------------------------------------------

(println "\n\n**************\nTest reals\n\n")

(random-source-randomize! default-random-source)

(define table (make-vector 20 0))
(define cases 100000)
(define _sum 0)

(define (test-mean)
  (let*((val (+ 9.5 (* 18 (a-normal term-count: 5 cases: cases))))
        (j  (inexact->exact (floor val)))
        (j (min (max j 0) 19)))
    (vector-set! table j (+ 1 (vector-ref table j)))
    (set! _sum (+ _sum val))))

(define (test-exponential)
  (let*((val (an-exponential mean: 5 cases: cases))
        (j  (inexact->exact (floor val))))
    (if (< val 20)
        (vector-set! table j (+ 1 (vector-ref table j))))
    (set! _sum (+ _sum val))))

(pp (test! (test-mean)))

(define (show-bar len)
  (let show ((len len))
    (if (<= len 1) (newline)
        (begin
          (display "#")
          (show (- len 1))))))

(define (test fn)
  (set! _sum 0)
  (set! table (make-vector 20 0))
  (let((mx '())
       (gamma '()))

    (pp (test! (fn)))

    (set! mx (apply max (vector->list table)))

    (set! gamma (/ 150 mx))

    (let loop ((j 0))
      (if (>= j (vector-length table)) 'ok
	  (begin
	    (show-bar (* gamma (vector-ref table j)))
	    (loop (+ j 1)))))


    (pp (/ _sum cases))
    (pp table)))

(test test-mean)

(test test-exponential)


;;-------------------------------------------------------------------------------

(println "\n\n**************\nTest strings\n\n")

(random-source-randomize! default-random-source)

(test! (let((a (a-string grammar: kant)))
         (display a)
	 (newline)))
(newline)

(test! (let((a (a-string))) ;; default kaiku
         (display a)
	 (newline)))
(newline)

(test! (let((a (a-string grammar: insult)))
         (display a)
	 (newline)))
(newline)
