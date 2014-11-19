;;!!! Binary shuffle algorithms
;; .author Taylor Campbell
;; .author Alvaro Castro-Castilla, 2014

;;! Binary Shuffle
;; Go through the list, collecting a left list and a right list by
;; randomly choosing which list to put successive elements on.
;; Recursively the left and right lists, and then concatenate them.
(define (binary-shuffle-list list)
  (define (flip-coin)
    (zero? (random-integer 2)))
  (define (bifurcate list left right)
    (if (null-list? list)
        (values left right)
        (let ((item (car list))
              (list (cdr list)))
          (if (flip-coin)
              (bifurcate list (cons item left) right)
              (bifurcate list left (cons item right))))))
  (let shuffle ((list list) (tail '()))
    (cond ((null-list? list)
           tail)
          ((null-list? (cdr list))
           (cons (car list) tail))
          ((null-list? (cddr list))
           (if (flip-coin)
               (cons (car list) (cons (cadr list) tail))
               (cons (cadr list) (cons (car list) tail))))
          (else
           (receive (left right) (bifurcate list '() '())
                    (shuffle left (shuffle right tail)))))))

(define (binary-shuffle-list! list)
  (define (flip-coin)
    (zero? (random-integer 2)))
  (define (bifurcate! list left right)
    (if (null-list? list)
        (values left right)
        (let ((item (car list))
              (next (cdr list)))
          (if (flip-coin)
              (begin (set-cdr! list left)
                     (bifurcate! next list right))
              (begin (set-cdr! list right)
                     (bifurcate! next left list))))))
  (let shuffle! ((list list) (tail '()))
    (cond ((null-list? list)
           tail)
          ((null-list? (cdr list))
           (set-cdr! list tail)
           list)
          ((null-list? (cddr list))
           ;; LIST is (A B), so...
           (if (flip-coin)
               (let ((next (cdr list)))
                 ;; ...set it to (B A . tail).
                 (set-cdr! list tail)
                 (set-cdr! next list)
                 next)
               (begin
                 ;; ...set it to (A B . tail).
                 (set-cdr! (cdr list) tail)
                 list)))
          (else
           (receive (left right) (bifurcate! list '() '())
                    (shuffle! left (shuffle! right tail)))))))
