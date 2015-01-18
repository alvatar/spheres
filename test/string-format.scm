(##spheres-load core: testing)

(##spheres-load format)


(define-test test-format
  (equal? "Hello, World!"
          (format "Hello, ~a" "World!"))
  (equal? "Error, list is too short: (one \"two\" 3)\n"
          (format "Error, list is too short: ~s~%" '(one "two" 3))))

(define-test test-srfi-29-extension
  (equal? "It's 12:00, Fred."
          (format "It's ~a, ~a." "12:00" "Fred"))
  (equal? "Fred, c'est 12:00."
          (format "~1@*~a, c'est ~a." "12:00" "Fred"))

  ;; Check index with more than one digit.
  (equal? "12"
          (format "~12@*~a" 0 1 2 3 4 5 6 7 8 9 10 11 12))

  ;; An argument retrieved using ~@* should survive a until the next argument
  ;; is used.  This is for convenience.
  (equal? ">x~x1,0,1"
          (format ">~1@*x~~x~a,~a,~a" 0 1))

  ;; The index used with ~@* should be always with respect to the original
  ;; list to make writing error messages easier.
  (equal? "3|0|3|1|3|2|3|3|3"
          (format "~3@*~a|~a|~3@*~a|~a|~3@*~a|~a|~3@*~a|~a|~3@*~a" 0 1 2 3)))
