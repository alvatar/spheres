;;!!! Reservoir sampling, random selection
;; .author Alvaro Castro-Castilla, 2012-2014

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

;;! Ordered Reservoir Sampling. Get the results in the original order
(define random-sample-ordered
  (match-lambda*
   ((size (? list? input))
    (random-sample-ordered size (list->stream input)))
   ((size (? stream? input))
    (let* ((enumerated-input (stream-zip input (stream-from 0)))
           (results (random-sample size enumerated-input)))
      (map first (sort results (lambda (a b) (< (cadr a) (cadr b)))))))))
