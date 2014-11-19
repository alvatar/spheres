;;!!! Reservoir sampling, random selection
;; .author Alvaro Castro-Castilla, 2012-2014

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
