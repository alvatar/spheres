;;!!! Bignum encoding

;;! Encode a bignum as a list of fixnums
;; .author Marc Feeley
(define (bignum->fixnum-list x radix-minus-1)
  (let* ((big-radix
          (+ radix-minus-1 1))
         (square-series
          (let loop ((square big-radix)
                     (square-list (list big-radix)))
            (let ((new-square
                   (* square square)))
              (if (< x new-square)
                  square-list
                  (loop new-square
                        (cons new-square square-list)))))))
    (define (convert n square-series tail)
      (if (pair? square-series)
          (let* ((q (quotient n (car square-series)))
                 (r (remainder n (car square-series)))
                 (new-square-series (cdr square-series)))
            (convert r
                     new-square-series
                     (convert q
                              new-square-series
                              tail)))
          (let ((d n))
            (if (and (null? tail) ;; avoid leading zeroes
                     (= d 0))
                tail
                (cons d tail)))))
    (convert x square-series '())))

;;! Decode a bignum from a list of fixnums
;; .author Marc Feeley
(define (fixnum-list->bignum digit-list radix-minus-1)
  ;; Note: a divide-and-conquer algorithm would be faster for large numbers.
  (let ((big-radix (+ radix-minus-1 1)))
    (let loop ((n 0) (lst (reverse digit-list)))
      (if (pair? lst)
          (loop (+ (* n big-radix) (car lst))
                (cdr lst))
          n))))

;;! Decode a bignum from a list of fixnums
;; .author Marc Feeley
(define (bignum->u8vector n)
  (list->u8vector (reverse (bignum->fixnum-list n 255))))

(define (u8vector->bignum u8vect)
  (fixnum-list->bignum (reverse (u8vector->list u8vect)) 255))
