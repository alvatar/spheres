
;; utility
(define (fold kons knil lst)
  (let loop ((acc knil)
             (lst lst))
    (if (null? lst)
        acc
        (loop (kons (car lst) acc)
              (cdr lst)))))

(define (parser-error . args)
  (for-each
   (lambda (x)
     (if (not (string? x))
         (write x (current-error-port))
         (display x (current-error-port))))
   args)
  (newline (current-error-port))
  #f)

(define-macro (assert test . result)
  `(let ((res ,test))
     (if (not ,(if (pair? result)
                   `(equal? res ,(car result))
                   'res))
         (begin
           (parser-error " *** ASSERTION FAILURE *** ")
           (parser-error ',test)
           (parser-error res " != "
                         ,(if (pair? result)
                              (car result)
                              #t))
           (exit 1)))))
