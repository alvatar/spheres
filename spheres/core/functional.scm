;;!!! Functional programming procedures
;; .author Alvaro Castro-Castilla, 2012-2014. All rights reserved.


;;------------------------------------------------------------------------------
;;!! Functional combinators

;;! U-combinator
(define U
  (lambda (f) (f f)))

;;! Y-combinator
;; .notes The applicative-order imperative y-combinator (by Peter Landin)
;; (define Y
;;   (lambda (f)
;;     (letrec
;;         ((h (f (lambda (arg) (h arg)))))
;;       h)))
(define Y
  (lambda (X)
    (U (lambda (proc)
         (X (lambda (arg) ((U proc) arg)))))))

;;! Function composition
;; (define *10sum (compose (lambda (a) (* 10 a)) +))
;; (*10sum 2 5 7) ==> 140
(define (compose f . rest)
  (if (null? rest)
      f
      (let ((g (apply compose rest)))
        (lambda args
          (call-with-values (lambda () (apply g args)) f)))))

;;! Function composition (recursive implementation)
;; .author Taylor Campbell
(define (recursive-compose . fs)
  (let recur ((fs fs))
    (if (null? fs)
        values
        (let ((f (car fs))
              (g (recur (cdr fs))))
          (lambda args
            (call-with-values
                (lambda () (apply g args))
              f))))))

;;! Function composition (tail-recursive implementation)
;; .author Taylor Campbell
(define (tail-recursive-compose . fs)
  (let loop ((g values) (fs fs))
    (if (null? fs)
        g
        (loop (lambda args
                (call-with-values
                    (lambda () (apply (car fs) args))
                  g))
              (cdr fs)))))

;;! Left section
;; .author Philip L. Bewig.
(define (left-section fn . args)
  (lambda xs (apply fn (append args xs))))

;;! Right Section
;; .author Philip L. Bewig.
;; (reverse (append (reverse args) (reverse xs))) = (append xs args)
(define (right-section fn . args)
  (lambda xs (apply fn (append xs args))))

;;! Curry (left section)
(define curry left-section)

;;! Uncurrying
;; (uncurry (lambda (a) (lambda (b) (lambda (c) (+ a b c)))) 5 2 1)
(define (uncurry f . arglist)
  (if (null? arglist) f
      (apply uncurry (f (car arglist)) (cdr arglist))))

;;! Complement a function (negate)
(define (complement f)
  (lambda args (not (apply f args))))

;;! Create a function with the order of the arguments reversed
;; (compose fn reverse list)
(define (reversed fn)
  (lambda xs (apply fn (reverse xs))))

;;! AND combinator
(define (andf . args)
  (let loop ((args args) (prev #t))
    (if (null? args) prev
        (let ((cur (car args)))
          (and cur
               (loop (cdr args) cur))))))

;;! OR combinator
(define (orf . args)
  (let loop ((args args))
    (and (not (null? args))
         (or (car args)
             (loop (cdr args))))))

;;! arguments-chainf
;; ((arguments-chainf f g) arg...) -> (apply f (apply g arg...))
;; ((arguments-chainf f) arg...) -> (apply f arg...)
;; ((arguments-chainf) arg...) -> (list arg...)
;; .author Kon Lovett
(define (arguments-chainf . fns)
  (define (chain-recur fns xs)
    ;; assume the length of fns is << so recursion depth is also <<
    (let recur ((fns fns))
      (if (null? fns) xs
          (apply (car fns) (recur (cdr fns))))))
  (define (chain-func fns)
    (cond
     ((null? fns)
      (lambda (x) x))
     ((null? (cdr fns))
      (let ((f (car fns)))
        (lambda (xs) (apply f xs))))
     (else
      (lambda (xs) (chain-recur fns xs)))))
  (let ((fn (chain-func fns)))
    (lambda xs (fn xs))))

;;! arguments-eachf
;; ((arguments-eachf f g h) a b c d e) -> (list (f a) (g b) (h c) (f d) (g e))
;; ((arguments-eachf) arg...) -> (list arg...)
;; .author Kon Lovett
(define (arguments-eachf . fns)
  (define (circular-list val1 . vals)
    (define (last-pair lis)
      (check-arg pair? lis last-pair)
      (let lp ((lis lis))
        (let ((tail (cdr lis)))
          (if (pair? tail) (lp tail) lis))))
    (let ((ans (cons val1 vals)))
      (set-cdr! (last-pair ans) ans)
      ans))
  (define (each-func fns)
    (cond
     ((null? fns)
      (lambda (x) x))
     ((null? (cdr fns))
      (let ((f (car fns)))
        (lambda (xs) (map (cut f <>) xs))))
     (else
      (let ((fns (apply circular-list fns)))
        (lambda (xs) (map (cut <> <>) fns xs))))))
  (let ((fn (each-func fns)))
    (lambda xs (fn xs))))

;;! arguments-allf
;; ((arguments-allf f g h) a b c) -> (list (f a b c) (g a b c) (h a b c))
;; ((arguments-allf) arg...) -> (list arg...)
;; .author Kon Lovett
(define (arguments-allf . fns)
  (define (all-func fns)
    (cond
     ((null? fns)
      (lambda (x) x))
     ((null? (cdr fns))
      (let ((f (car fns)))
        (lambda (xs) (list (apply f xs)))))
     (else
      (lambda (xs) (map (cut apply <> xs) fns)))))
  (let ((fn (all-func fns)))
    (lambda xs (fn xs))))


;;------------------------------------------------------------------------------
;;!! Memoization

;;! Function computation memoization specifying a key generation procedure
;; This procedure will be applied to the parameters to construct the key
(define (memoize/key-gen key-gen f)
  (let ((cache (make-table)))
    (lambda args
      (let ((key (apply key-gen args)))
        (apply
         values
         (let ((v (table-ref cache key #f)))
           (or v
               (call-with-values (lambda () (apply f args))
                 (lambda results
                   (table-set! cache key results)
                   results)))))))))

;;! Function computation memoization with default key generation
(define (memoize f)
  (let ((cache (make-table)))
    (lambda k
      (let ((v (table-ref cache k #f)))
	(or v
            (let ((res (apply f k)))
	      (table-set! cache k res)
	      res))))))
