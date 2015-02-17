(define-macro (time* . code)
  (let ((t0 (gensym)))
    `(let ((,t0 (time->seconds (current-time))))
       (begin ,@code)
       (inexact->exact
        (round (* 1000 (- (time->seconds (current-time)) ,t0)))))))
