;;!!! Shuffle algorithms
;; .author Taylor Campbell
;; .author Alvaro Castro-Castilla, 2014

;;! Insertion Shuffle
(define (insertion-shuffle-list list)
  (define (insert list position item)
    (if (zero? position)
        (cons item list)
        (cons (car list)
              (insert (cdr list) (- position 1) item))))
  (if (null-list? list)
      '()
      (let loop ((in (cdr list)) (count 1) (out (cons (car list) '())))
        (let ((count (+ count 1))
              (item (car in))
              (next (cdr in)))
          (let ((out (insert out (random-integer count) item)))
            (if (null-list? next)
                out
                (loop next count out)))))))

(define (insertion-shuffle-list! list)
  (define (insert! list lag position cell)
    (let ((position (- position 1)))
      (if (zero? position)
          (begin (set-cdr! lag cell)
                 (set-cdr! cell list))
          (insert! (cdr list) list position cell))))
  (if (null-list? list)
      '()
      (let ((in (cdr list)))
        (set-cdr! list '())
        (let loop ((in in) (count 1) (out list))
          (if (null-list? in)
              out
              (let ((next (cdr in))
                    (count (+ count 1)))
                (loop next
                      count
                      (let ((position (random-integer count)))
                        (if (zero? position)
                            (begin (set-cdr! in out)
                                   in)
                            (begin (insert! (cdr out) out position in)
                                   out))))))))))
