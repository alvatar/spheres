(define libraries
  '((spheres/core assert)
    (spheres/core base)
    (spheres/core functional)
    (spheres/core match)
    (spheres/core meta)
    (spheres/core minimal)
    (spheres/gambit/ffi array)
    (spheres/gambit/ffi macros)
    (spheres/gambit/ffi memory)
    (spheres/gambit/ffi serialization)
    (spheres/gambit/ffi types)))

(define features
  '(((spheres/core match) only-macros)
    ((spheres/core meta) only-macros)
    ((spheres/gambit/ffi array) gambit)
    ((spheres/gambit/ffi macros) gambit only-macros)
    ((spheres/gambit/ffi memory) gambit)
    ((spheres/gambit/ffi serialization) gambit)
    ((spheres/gambit/ffi types) gambit only-macros)))

(define-task (compile library) ()
  (define (compile-library lib)
    (let ((ftrs (or (assoc lib features) '())))
      (if (not (memq 'only-macros ftrs))
          (ssrun#compile-library
           lib
           cond-expand-features: '(debug)
           expander: (if (memq 'gambit ftrs)
                         'gambit
                         'syntax-case)))))
  (if library
      (compile-library library)
      (for-each compile-library libraries)))

(define-task clean ()
  (ssrun#clean-libraries libraries))
