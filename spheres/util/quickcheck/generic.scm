(define (in-set<< lst)
  (lambda (yield)
    (for-each yield lst)))

;; catenate 2 generators
(define (alt<< . gs)
  (lambda (yield)
    (for-each (lambda (g) (g yield)) gs)))

(define (in-set vs)
  (generate! (in-set<< vs)))

(define (any-of . vs) (in-set vs))

(define (all? test? lst)
  (let all ((lst lst))
    (or (null? lst)
        (and (test? (car lst))
             (all (cdr lst))))))

(define (exists? test? lst)
  (let exists ((lst lst))
    (and (pair? lst)
         (or (test? (car lst))
             (exists (cdr lst))))))

