;;!!! Reservoir sampling, random selection
;; .author Alvaro Castro-Castilla, 2012-2014

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

;;! Pick a random element
;; .parameter l input list
;; .parameter len (optional) give the length of the list, if known in advance
(define* (random-pick l (len #f))
  (list-ref l (random-integer (or len (length l)))))

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

;;! Pick a random element and return also the list without that element
;; .parameter l input list
;; .parameter len (optional) give the length of the list, if known in advance
(define* (random-extract l (len #f))
  (let ((random-pick+rember/length
         (lambda (l len)
           (let ((rnd (random-integer len)))
             (let recur ((rest l)
                         (i 0))
               (if (fx= i rnd)
                   (values (car rest) (cdr rest))
                   (receive (head tail)
                            (recur (cdr rest) (fx+ 1 i))
                            (values head
                                    (cons (car rest)
                                          tail)))))))))
    (random-pick+rember/length l (or len (length l)))))

;;! Reservoir Sampling. Get a number of random elements from a list or stream
(define random-sample 
  (match-lambda*
   ((size (? list? input))
    (random-sample size (list->stream input)))
   ((size (? stream? input))
    (let ((first-part (stream-take size input))
          (second-part (stream-drop size input))
          (pool (make-vector size)))
      (stream-for-each (match-lambda ((i val)
                                      (vector-set! pool i val)))
                       (stream-zip (stream-from 0) first-part))
      (stream-for-each (match-lambda ((i val)
                                      (let ((index (random-integer i)))
                                        (and (< index size)
                                             (vector-set! pool index val)))))
                       (stream-zip (stream-from size) second-part))
      (vector->list pool)))))

;;! Ordered Reservoir Sampling. Get the results in the original order
(define random-sample-ordered
  (match-lambda*
   ((size (? list? input))
    (random-sample-ordered size (list->stream input)))
   ((size (? stream? input))
    (let* ((enumerated-input (stream-zip input (stream-from 0)))
           (results (random-sample size enumerated-input)))
      (map first (sort results (lambda (a b) (< (cadr a) (cadr b)))))))))
