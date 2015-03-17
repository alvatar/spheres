(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (println "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (println "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                     'tested-expression expected produced)))))))
