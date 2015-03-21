;;!!! A-list utilities
;; .author Per Eckerdal, 2008
;; .author Mikael More, 2008
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure a-list)
  (export al-get
          al-getq
          al
          al-set!
          al-set!-dfl
          al-helper)

  (define-macro (al . args)
    (let ((nargs
           (let loop ((a args))
             (cond
              ((null? a) '())
              ((pair? (car a))
               `(',(caar a)
                 (lambda ,(cdar a) ,(cadr a))
                 ,@(loop (cddr a))))
              (else `(',(car a) ,(cadr a) ,@(loop (cddr a))))))))
      `(al-helper
        ,@(map (lambda (x)
                 (make-syntactic-closure env '() x))
               nargs))))

  (include "a-list.scm"))
