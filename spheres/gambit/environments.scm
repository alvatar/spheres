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
  (println "-- environment: optimize -- preloaded libraries: <none>"))
 ;; Debug Environment
 (debug
  (declare (safe)
           (debug)
           (debug-location)
           (debug-source)
           (debug-environments))
  (println "-- environment: debug -- preloaded libraries: (spheres/core meta) (spheres/core assert)")
  (%load-library '(spheres/core assert) verbose: #f))
 ;; Null Environment
 (else #!void))
