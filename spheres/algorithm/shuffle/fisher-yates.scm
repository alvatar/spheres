;;!!! Shuffle algorithms
;; .author Taylor Campbell
;; .author Alvaro Castro-Castilla, 2014

;;! Fisher-Yates O(n) Random-Access Shuffle
(define (Fisher-Yates-shuffler sequence-exchange!)
  (lambda (sequence start end)
    (do ((i start (+ i 1)))
        ((>= i end))
      (let ((j (+ start (random-integer (+ 1 (- i start))))))
        (if (not (= i j))
            (sequence-exchange! sequence i j))))))

(define (sequence-exchanger sequence-ref sequence-set!)
  (lambda (sequence i j)
    (let ((elt-i (sequence-ref sequence i))
          (elt-j (sequence-ref sequence j)))
      (sequence-set! sequence j elt-i)
      (sequence-set! sequence i elt-j))))

;;! Shuffle a vector, using Fisher-Yates
(define shuffle-vector!
  (Fisher-Yates-shuffler (sequence-exchanger vector-ref vector-set!)))

;;! Shuffle a string, using Fisher-Yates
(define shuffle-string!
  (Fisher-Yates-shuffler (sequence-exchanger string-ref string-set!)))
