;;;! Environments

(cond-expand
 ;; Optimize Environment
 (optimize
  (declare (standard-bindings)
           (extended-bindings)
           (not safe)
           (block))
  (define-macro (check-arg pred val caller)
    #!void)
  (println "-- optimize environment --"))
 ;; Debug Environment
 (debug
  (declare (safe)
           (debug)
           (debug-location)
           (debug-source)
           (debug-environments))
  (define-macro (check-arg pred val caller)
    `(let lp ((val ,val))
       (if (,pred val)
           val
           (lp (error "Bad argument" val pred ,caller)))))
  (println "-- debug environment --"))
 ;; Null Environment
 (else
  (define-macro (check-arg pred val caller)
    #!void)))
