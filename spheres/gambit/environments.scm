;;;! Environments

(cond-expand
 ;; Optimize Environment
 (optimize
  (declare (standard-bindings)
           (extended-bindings)
           (not safe)
           (block))
  (define-syntax check-arg
    (syntax-rules ()
      ((check-arg . ?any)
       #!void)))
  (println "-- optimize environment --"))
 ;; Debug Environment
 (debug
  (declare (safe)
           (debug)
           (debug-location)
           (debug-source)
           (debug-environments))
  (define-syntax check-arg
    (syntax-rules ()
      ((check-arg ?pred ?val ?caller)
       (let ((val ?val))
         (if (?pred val)
             val
             (begin
               (println (string-append "Value: " (object->string val)))
               (println (string-append "Predicate: " (object->string '?pred)))
               (error "Failed argument check in" '?caller)))))
      ((check-arg ?pred ?val ?caller ?reason)
       (let ((val ?val))
         (if (?pred val)
             val
             (begin
               (println (string-append "Value: " (object->string val)))
               (println (string-append "Predicate: " (object->string '?pred)))
               (println (string-append "Reason: " ?reason))
               (error "Failed argument check in" '?caller)))))))
  (println "-- debug environment --"))
 ;; Null Environment
 (else
  (define-syntax check-arg
    (syntax-rules ()
      ((check-arg . ?any)
       #!void)))))
