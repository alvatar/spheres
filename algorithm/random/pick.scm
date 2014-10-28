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
