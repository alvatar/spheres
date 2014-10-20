;;!!! Functional programming procedures
;; .author Alvaro Castro-Castilla, 2012-2014. All rights reserved.

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))


;;------------------------------------------------------------------------------
;;!! Functional operators

;;! U-combinator
(define U
  (lambda (f) (f f)))

;;! Y-combinator
(define Y
  (lambda (X)
    (U (lambda (proc)
         (X (lambda (arg) ((U proc) arg)))))))

;;! The applicative-order imperative y-combinator
;; .author Peter Landin
(define Y!
  (lambda (f)
    (letrec
        ((h (f (lambda (arg) (h arg)))))
      h)))

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


;;! Complement a function (negate)
(define (complement f)
  (lambda args (not (apply f args))))

;;! Create a function with the order of the 2 arguments swapped
(define (flip func)
  (lambda (arg1 arg2) (func arg2 arg1)))


;;------------------------------------------------------------------------------
;;!! Currying / uncurrying

;;! Right section
(define (rsec proc . args)
  (lambda x (apply proc
              (reverse
               (append (reverse args) (reverse x))))))

;;! Curry or left section
(define (curry proc . args)
  (lambda x (apply proc (append args x))))

;;! Uncurrying
;; (uncurry (lambda (a) (lambda (b) (lambda (c) (+ a b c)))) 5 2 1)
(define (uncurry f . arglist)
  (if (null? arglist) f
      (apply uncurry (f (car arglist)) (cdr arglist))))


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
