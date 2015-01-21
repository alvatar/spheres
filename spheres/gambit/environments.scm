;;;! Environments

(define-syntax check-arg
    (syntax-rules ()
      ((_ . ?ignore) #!void)))

(define-syntax assert
  (syntax-rules ()
    ((_ . ?ignore) #!void)))

(cond-expand
 ;; Optimize Environment
 (optimize
  (declare (standard-bindings)
           (extended-bindings)
           (not safe)
           (block))
  (println "-- environment: optimize --"))
 ;; Debug Environment
 (debug
  (declare (safe)
           (debug)
           (debug-location)
           (debug-source)
           (debug-environments))
  (println "-- environment: debug --")
  (%load-library '(spheres/core assert)))
 ;; Null Environment
 (else #!void))
