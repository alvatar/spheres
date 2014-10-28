;;!!! Reservoir sampling, random selection
;; .author Alvaro Castro-Castilla, 2012-2014

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

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
