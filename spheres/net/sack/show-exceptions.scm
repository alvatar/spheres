(define (simple-sack-response thunk)
  (let ((ret #t))
    (lambda ()
      (and
       ret
       (begin
         (set! ret #f)
         (with-output-to-u8vector
          (list char-encoding: 'UTF-8)
          thunk))))))

(define (show-exceptions app)
  (lambda (env)
    (with-exception/continuation-catcher
     (lambda (e)
       (values 500
               '(("Content-Type" . "text/plain; charset=UTF-8"))
               (simple-sack-response
                (lambda ()
                  (display "500 Internal Server Error\n\n")
                  (display
                   (exception/continuation->string e))))))
     (lambda ()
       (app env)))))
