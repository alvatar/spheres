(include "~~persistent/vector#.scm")

(define-macro (assert exp)
  (let ((x (gensym 'x)))
    `(let ((,x ,exp))
       (if ,x
	   (pp (quote (SUCCESS ,(list 'assert exp))))
	   (pp (quote (FAIL! ,(list 'assert exp))))))))
  
(define (test-create)
  (let ((v (make-persistent-vector 10 init: (lambda (x) (+ x 1)))))
    (assert (equal? (persistent-vector->list v)
		    '(1 2 3 4 5 6 7 8 9 10)))))

(define (test-ref)
  (let ((v (make-persistent-vector 10 init: (lambda (x) (+ x 1)))))
    (assert (equal? (persistent-vector-ref v 4) (+ 4 1)))))

(define (test-set)
  (let* ((v (make-persistent-vector 10 init: (lambda (x) (+ x 1))))
	 (v1 (persistent-vector-set v 4 (+ 4 10))))
    (assert (not (equal? (persistent-vector-ref v 4) (+ 4 10))))
    (assert (equal? (persistent-vector-ref v1 4) (+ 4 10)))))

(test-create)
(test-ref)
(test-set)
