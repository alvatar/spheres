(define-task (compile library) ()
  (ssrun#compile-library library
                         cond-expand-features: '(debug)))

(define-task clean ()
  (ssrun#clean-all))
