;;!!! Procedures for countable discrete structures and combinatorial problems
;; .author Alvaro Castro-Castilla, 2012-2015

;;! Binomial coefficient
(define (binomial-coefficient n k)
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

;;! Permutations of a list
(define (permutations l)
  (define (insert l n e)
    (if (= 0 n)
        (cons e l)
        (cons (car l) 
              (insert (cdr l) (- n 1) e))))
  (define (seq start end)
    (if (= start end)
        (list end)
        (cons start (seq (+ start 1) end))))
  (let permute ((l l))
    (if (null? l)
        '(())
        (apply append (map (lambda (p)
                             (map (lambda (n)
                                    (insert p n (car l)))
                                  (seq 0 (length p))))
                           (permute (cdr l)))))))

;;! Combinations of a list
(define (combinations m lst)
  (let comb ((m m)
             (lst lst))
    (cond ((= m 0) '(()))
          ((null? lst) '())
          (else (append (map (lambda (y) (cons (car lst) y))
                             (comb (- m 1) (cdr lst)))
                        (comb m (cdr lst)))))))

;;! Non-continuous sequences found in a list
(define (non-continuous-sequences lst)
  (let recurse ((s 0)
                (lst lst))
    (if (null? lst)
        (if (>= s 3)
            '(())
            '())
        (let ((x (car lst))
              (xs (cdr lst)))
          (if (even? s)
              (append
               (map (lambda (ys) (cons x ys))
                    (recurse (+ s 1) xs))
               (recurse s xs))
              (append
               (map (lambda (ys) (cons x ys))
                    (recurse s xs))
               (recurse (+ s 1) xs)))))))

;;! Power Set: the set of all subsets
(define (power-set set)
  (if (null? set)
      '(())
      (let ((rest (power-set (cdr set))))
        (append (map (lambda (element) (cons (car set) element))
                     rest)
                rest))))

