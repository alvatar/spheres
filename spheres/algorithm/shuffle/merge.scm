;;!!! Shuffle algorithms
;; .author Taylor Campbell
;; .author Alvaro Castro-Castilla, 2014

;;! Merge Shuffle
;; Partition the list into two equal halves; shuffle the two halves,
;; and then merge them by randomly choosing which half to select the
;; next element from.
(define (merge-shuffle-list list)
  (define (flip-coin)
    (zero? (random-integer 2)))
  (define (merge a b)
    (cond ((not (pair? a)) b)
          ((not (pair? b)) a)
          (else
           (if (flip-coin)
               (cons (car a) (merge (cdr a) b))
               (cons (car b) (merge a (cdr b)))))))

  (define (partition list a b)
    (let ((next (cdr list))
          (a b)
          (b (cons (car list) a)))
      (if (null-list? next)
          (values a b)
          (partition next a b))))

  (if (null-list? list)
      '()
      (let shuffle ((list list))
        (if (null-list? (cdr list))
            list
            (receive (a b) (partition list '() '())
                     (merge (shuffle a) (shuffle b)))))))

(define (merge-shuffle-list! list)
  (define (flip-coin)
    (zero? (random-integer 2)))
  (define (merge! a b)
    (cond ((null-list? a)       b)
          ((null-list? b)       a)
          ((flip-coin)          (%merge! a b) a)
          (else                 (%merge! b a) b)))
  (define (%merge! a b)
    (cond ((null-list? (cdr a))
           (set-cdr! a b))
          ((flip-coin)
           (%merge! (cdr a) b))
          (else
           (%merge! b (let ((next (cdr a)))
                        (set-cdr! a b)
                        next)))))
  (define (partition! list a b)
    (let ((next (cdr list)))
      (set-cdr! list a)
      (if (null-list? next)
          (values list b)
          (partition! next b list))))
  (if (null-list? list)
      '()
      (let shuffle! ((list list))
        (if (null-list? (cdr list))
            list
            (receive (a b) (partition! list '() '())
                     (merge! (shuffle! a) (shuffle! b)))))))
