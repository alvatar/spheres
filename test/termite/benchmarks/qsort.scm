(include "bench.scm")

(define (make-random-list n)
  (if (= n 0)
      '()
      (cons (random-integer 1000000)
            (make-random-list (- n 1)))))

(define (qsort lst)
  (define (partition lst pivot k)
    (let loop ((lst lst)
               (smaller '())
               (greater '()))
      (cond
       ((null? lst)
        (k smaller greater))
       ((< (car lst) pivot)
        (loop (cdr lst)
              (cons (car lst) smaller)
              greater))
       (else
        (loop (cdr lst)
              smaller
              (cons (car lst) greater))))))

  (define (qs lst sorted)
    (if (null? lst)
        sorted
        (let ((pivot (car lst))
              (rest (cdr lst)))
          (partition rest
                     pivot
                     (lambda (smaller greater)
                       (qs smaller
                           (cons pivot
                                 (qs greater sorted))))))))

  (qs lst '()))

(random-source-randomize! default-random-source)

(define (main n)
  (let ((n (string->number n)))
    (let ((lst (make-random-list n)))
      (write `(qsort termite ,n ,(time* (qsort lst))))
      (newline)
      (force-output))))
