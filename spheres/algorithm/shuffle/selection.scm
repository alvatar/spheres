;;!!! Shuffle algorithms
;; .author Taylor Campbell
;; .author Alvaro Castro-Castilla, 2014

;;! Selection Shuffle
(define (selection-shuffle-list list)
  (define (select list position)
    (if (zero? position)
        (values (car list) (cdr list))
        (receive (item tail)
                 (select (cdr list) (- position 1))
                 (values item (cons (car list) tail)))))
  (if (null-list? list)
      '()
      (let loop ((in list) (out '()) (len (length list)))
        (receive (item list) (select in (random-integer len))
                 (let ((out (cons item out)))
                   (if (null-list? list)
                       out
                       (loop list
                             (cons item out)
                             (- len 1))))))))

(define (selection-shuffle-list! list)
  (define (select! list lag position)
    (if (zero? position)
        (begin (set-cdr! lag (cdr list))
               list)
        (select! (cdr list) list (- position 1))))
  (if (null-list? list)
      '()
      (let loop ((in list) (out '()) (len (length list)))
        (let ((position (random-integer len)))
          (receive (cell next)
                   (if (zero? position)
                       (values in (cdr in))
                       (values (select! (cdr in) in (- position 1))
                               in))
                   (set-cdr! cell out)
                   (if (null-list? next)
                       cell
                       (loop next cell (- len 1))))))))
