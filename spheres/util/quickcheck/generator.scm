(define (call/gen f #!key 
                  (max-branches +inf.0) 
                  (max-depth +inf.0) 
                  (on-fail (lambda (r) (raise 'not-found))))
  (call/cc 
   (lambda (fl) 
     (let ((fl (lambda (r) (fl (on-fail))))
           (vc 0))
       (f (lambda (gen)
            (call/cc
             (lambda (ret)
               (let((_fl fl) (hc 0) (_vc vc))
                 (set! vc (+ vc 1))
                 (gen (lambda (v)
                        (call/cc (lambda (k)
                                   (set! hc (+ hc 1))
                                   (if (or (> hc max-branches) (> vc max-depth))
                                       (begin (set! vc _vc) (_fl #f))
                                       (set! fl k))
                                   (ret v)))))
                 (set! vc _vc) 
                 (_fl 'fl))))))))))

   

  