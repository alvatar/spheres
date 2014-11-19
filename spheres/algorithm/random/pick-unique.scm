;;!!! Reservoir sampling, random selection
;; .author Alvaro Castro-Castilla, 2012-2014

;;! Pick a number of random elements without repetition
(define (random-pick-unique n l)
  (let recur ((l l)
              (n n)
              (picked '()))
    (if (or (null? l) (zero? n))
        picked
        (receive (fore aft)
                 (split-at l (random-integer (length l)))
                 (recur (append fore (cdr aft))
                        (-- n)
                        (cons (car aft) picked))))))
