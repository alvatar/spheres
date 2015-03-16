(define (test-equal actual expected . comments)
  (if (not (equal? actual expected))
    (begin
      (newline)
      (if comments
        (begin
          (print "FAILED: ")
          (write comments)
          (newline)))
      (println "expected:")
      (write expected)
      (newline)
      (println "actual:")
      (write actual)
      (newline)
      (error 'test-equal-failure))))

