;; Copyright (c) 2005 Sebastian Egner and Jens Axel S{\o}gaard.
;; Copyright (c) 2012 Álvaro Castro-Castilla

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; ``Software''), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; -----------------------------------------------------------------------

;;
;; Important Notice: the test is incomplete. See SRFI for the complete test
;;

(##spheres-load energy: testing)
(##spheres-load algorithm/comprehension)
(##spheres-load algorithm/compare)

(test-begin "SRFI-67: Compare Procedures")

(define ci integer-compare)

(define my-booleans   '(#f #t))
(define my-chars      '(#\a #\b #\c))
(define my-chars-ci   '(#\a #\B #\c #\D))
(define my-strings    '("" "a" "aa" "ab" "b" "ba" "bb"))
(define my-strings-ci '("" "a" "aA" "Ab" "B" "bA" "BB"))
(define my-symbols    '(a aa ab b ba bb))

(define my-reals
  (append-ec (:range xn -6 7) 
             (:let x (/ xn 3))
             (list x (+ x (exact->inexact (/ 1 100))))))

(define my-rationals
  (list-ec (:list x my-reals)
           (and (exact? x) (rational? x))
           x))

(define my-integers
  (list-ec (:list x my-reals)
           (if (and (exact? x) (integer? x)))
           x))

(define my-complexes
  (list-ec (:list re-x my-reals)
           (if (inexact? re-x))
           (:list im-x my-reals)
           (if (inexact? im-x))
           (make-rectangular re-x im-x)))

(define my-lists
  '(() (1) (1 1) (1 2) (2) (2 1) (2 2)))

(define my-vector-as-lists
  (map list->vector my-lists))

(define my-list-as-vectors
  '(() (1) (2) (1 1) (1 2) (2 1) (2 2)))

(define my-vectors
  (map list->vector my-list-as-vectors))

(define my-null-or-pairs 
  '(()
    (1) (1 1) (1 2) (1 . 1) (1 . 2) 
    (2) (2 1) (2 2) (2 . 1) (2 . 2)))

(define my-objects
  (append my-null-or-pairs
          my-booleans
          my-chars
          my-strings
          my-symbols
          my-integers
          my-vectors))

; =============================================================================

(define-syntax test-equal-if2
  (syntax-rules ()
    ((test-equal-if2 if-rel? rel)
     (begin
       ;; check result
       (test-equal (if-rel? -1 'yes 'no) (if (rel -1 0) 'yes 'no))
       (test-equal (if-rel?  0 'yes 'no) (if (rel  0 0) 'yes 'no))
       (test-equal (if-rel?  1 'yes 'no) (if (rel  1 0) 'yes 'no))
       ;; check result of 'laterally challenged if'
       (test-equal (let ((x #f)) (if-rel? -1 (set! x #t)) x) (rel -1 0))
       (test-equal (let ((x #f)) (if-rel?  0 (set! x #t)) x) (rel  0 0))
       (test-equal (let ((x #f)) (if-rel?  1 (set! x #t)) x) (rel  1 0))
       ;; check that <c> is evaluated exactly once
       (test-equal (let ((n 0)) (if-rel? (begin (set! n (+ n 1)) -1) #t #f) n) 1)
       (test-equal (let ((n 0)) (if-rel? (begin (set! n (+ n 1))  0) #t #f) n) 1)
       (test-equal (let ((n 0)) (if-rel? (begin (set! n (+ n 1))  1) #t #f) n) 1)
       (test-equal (let ((n 0)) (if-rel? (begin (set! n (+ n 1)) -1) #t) n) 1)
       (test-equal (let ((n 0)) (if-rel? (begin (set! n (+ n 1))  0) #t) n) 1)
       (test-equal (let ((n 0)) (if-rel? (begin (set! n (+ n 1))  1) #t) n) 1)))))

(define-syntax test-equal-chain2
  (syntax-rules ()
    ((test-equal-chain2 rel? rel)
     (begin
       ;; all chains of length 2
       (test-equal (rel? ci 0 0) (rel 0 0))
       (test-equal (rel? ci 0 1) (rel 0 1))
       (test-equal (rel? ci 1 0) (rel 1 0))
       ;; using default-compare
       (test-equal (rel? 0 0) (rel 0 0))
       (test-equal (rel? 0 1) (rel 0 1))
       (test-equal (rel? 1 0) (rel 1 0))
       ;; as a combinator
       (test-equal ((rel? ci) 0 0) (rel 0 0))
       (test-equal ((rel? ci) 0 1) (rel 0 1))
       (test-equal ((rel? ci) 1 0) (rel 1 0))
       ;; using default-compare as a combinator
       (test-equal ((rel?) 0 0) (rel 0 0))
       (test-equal ((rel?) 0 1) (rel 0 1))
       (test-equal ((rel?) 1 0) (rel 1 0))))))

(define (list->set xs)                  ; xs a list of integers
  (if (null? xs)
      '()
      (let ((max-xs
             (let max-without-apply ((m 1) (xs xs))
               (if (null? xs)
                   m
                   (max-without-apply (max m (car xs)) (cdr xs))))))
        (let ((in-xs? (make-vector (+ max-xs 1) #f)))
          (do-ec (:list x xs) (vector-set! in-xs? x #t))
          (list-ec (:vector in? (index x) in-xs?)
                   (if in?)
                   x)))))

(define-syntax arguments-used ; set of arguments (integer, >=0) used in compare
  (syntax-rules ()
    ((arguments-used (rel1/rel2 compare arg ...))
     (let ((used '()))
       (rel1/rel2 (lambda (x y)
                    (set! used (cons x (cons y used)))
                    (compare x y))
                  arg ...)
       (list->set used)))))

(define-syntax test-equal-chain3
  (syntax-rules ()
    ((test-equal-chain3 rel1/rel2? rel1 rel2)
     (begin     
       ;; all chains of length 3
       (test-equal (rel1/rel2? ci 0 0 0) (and (rel1 0 0) (rel2 0 0)))
       (test-equal (rel1/rel2? ci 0 0 1) (and (rel1 0 0) (rel2 0 1)))
       (test-equal (rel1/rel2? ci 0 1 0) (and (rel1 0 1) (rel2 1 0)))
       (test-equal (rel1/rel2? ci 1 0 0) (and (rel1 1 0) (rel2 0 0)))
       (test-equal (rel1/rel2? ci 1 1 0) (and (rel1 1 1) (rel2 1 0)))
       (test-equal (rel1/rel2? ci 1 0 1) (and (rel1 1 0) (rel2 0 1)))
       (test-equal (rel1/rel2? ci 0 1 1) (and (rel1 0 1) (rel2 1 1)))
       (test-equal (rel1/rel2? ci 0 1 2) (and (rel1 0 1) (rel2 1 2)))
       (test-equal (rel1/rel2? ci 0 2 1) (and (rel1 0 2) (rel2 2 1)))
       (test-equal (rel1/rel2? ci 1 2 0) (and (rel1 1 2) (rel2 2 0)))
       (test-equal (rel1/rel2? ci 1 0 2) (and (rel1 1 0) (rel2 0 2)))
       (test-equal (rel1/rel2? ci 2 0 1) (and (rel1 2 0) (rel2 0 1)))
       (test-equal (rel1/rel2? ci 2 1 0) (and (rel1 2 1) (rel2 1 0)))
       ;; using default-compare
       (test-equal (rel1/rel2? 0 0 0) (and (rel1 0 0) (rel2 0 0)))
       (test-equal (rel1/rel2? 0 0 1) (and (rel1 0 0) (rel2 0 1)))
       (test-equal (rel1/rel2? 0 1 0) (and (rel1 0 1) (rel2 1 0)))
       (test-equal (rel1/rel2? 1 0 0) (and (rel1 1 0) (rel2 0 0)))
       (test-equal (rel1/rel2? 1 1 0) (and (rel1 1 1) (rel2 1 0)))
       (test-equal (rel1/rel2? 1 0 1) (and (rel1 1 0) (rel2 0 1)))
       (test-equal (rel1/rel2? 0 1 1) (and (rel1 0 1) (rel2 1 1)))
       (test-equal (rel1/rel2? 0 1 2) (and (rel1 0 1) (rel2 1 2)))
       (test-equal (rel1/rel2? 0 2 1) (and (rel1 0 2) (rel2 2 1)))
       (test-equal (rel1/rel2? 1 2 0) (and (rel1 1 2) (rel2 2 0)))
       (test-equal (rel1/rel2? 1 0 2) (and (rel1 1 0) (rel2 0 2)))
       (test-equal (rel1/rel2? 2 0 1) (and (rel1 2 0) (rel2 0 1)))
       (test-equal (rel1/rel2? 2 1 0) (and (rel1 2 1) (rel2 1 0)))
       ;; as a combinator
       (test-equal ((rel1/rel2? ci) 0 0 0) (and (rel1 0 0) (rel2 0 0)))
       (test-equal ((rel1/rel2? ci) 0 0 1) (and (rel1 0 0) (rel2 0 1)))
       (test-equal ((rel1/rel2? ci) 0 1 0) (and (rel1 0 1) (rel2 1 0)))
       (test-equal ((rel1/rel2? ci) 1 0 0) (and (rel1 1 0) (rel2 0 0)))
       (test-equal ((rel1/rel2? ci) 1 1 0) (and (rel1 1 1) (rel2 1 0)))
       (test-equal ((rel1/rel2? ci) 1 0 1) (and (rel1 1 0) (rel2 0 1)))
       (test-equal ((rel1/rel2? ci) 0 1 1) (and (rel1 0 1) (rel2 1 1)))
       (test-equal ((rel1/rel2? ci) 0 1 2) (and (rel1 0 1) (rel2 1 2)))
       (test-equal ((rel1/rel2? ci) 0 2 1) (and (rel1 0 2) (rel2 2 1)))
       (test-equal ((rel1/rel2? ci) 1 2 0) (and (rel1 1 2) (rel2 2 0)))
       (test-equal ((rel1/rel2? ci) 1 0 2) (and (rel1 1 0) (rel2 0 2)))
       (test-equal ((rel1/rel2? ci) 2 0 1) (and (rel1 2 0) (rel2 0 1)))
       (test-equal ((rel1/rel2? ci) 2 1 0) (and (rel1 2 1) (rel2 1 0)))
       ;; as a combinator using default-compare
       (test-equal ((rel1/rel2?) 0 0 0) (and (rel1 0 0) (rel2 0 0)))
       (test-equal ((rel1/rel2?) 0 0 1) (and (rel1 0 0) (rel2 0 1)))
       (test-equal ((rel1/rel2?) 0 1 0) (and (rel1 0 1) (rel2 1 0)))
       (test-equal ((rel1/rel2?) 1 0 0) (and (rel1 1 0) (rel2 0 0)))
       (test-equal ((rel1/rel2?) 1 1 0) (and (rel1 1 1) (rel2 1 0)))
       (test-equal ((rel1/rel2?) 1 0 1) (and (rel1 1 0) (rel2 0 1)))
       (test-equal ((rel1/rel2?) 0 1 1) (and (rel1 0 1) (rel2 1 1)))
       (test-equal ((rel1/rel2?) 0 1 2) (and (rel1 0 1) (rel2 1 2)))
       (test-equal ((rel1/rel2?) 0 2 1) (and (rel1 0 2) (rel2 2 1)))
       (test-equal ((rel1/rel2?) 1 2 0) (and (rel1 1 2) (rel2 2 0)))
       (test-equal ((rel1/rel2?) 1 0 2) (and (rel1 1 0) (rel2 0 2)))
       (test-equal ((rel1/rel2?) 2 0 1) (and (rel1 2 0) (rel2 0 1)))
       (test-equal ((rel1/rel2?) 2 1 0) (and (rel1 2 1) (rel2 1 0)))
       ;; test if all arguments are type checked
       (test-equal (arguments-used (rel1/rel2? ci 0 1 2)) '(0 1 2))
       (test-equal (arguments-used (rel1/rel2? ci 0 2 1)) '(0 1 2))
       (test-equal (arguments-used (rel1/rel2? ci 1 2 0)) '(0 1 2))
       (test-equal (arguments-used (rel1/rel2? ci 1 0 2)) '(0 1 2))
       (test-equal (arguments-used (rel1/rel2? ci 2 0 1)) '(0 1 2))
       (test-equal (arguments-used (rel1/rel2? ci 2 1 0)) '(0 1 2))))))

(define-syntax test-equal-chain
  (syntax-rules ()
    ((test-equal-chain chain-rel? rel)
     (begin
       ;; the chain of length 0
       (test-equal (chain-rel? ci) #t)
       ;; a chain of length 1
       (test-equal (chain-rel? ci 0) #t)
       ;; all chains of length 2
       (test-equal (chain-rel? ci 0 0) (rel 0 0))
       (test-equal (chain-rel? ci 0 1) (rel 0 1))
       (test-equal (chain-rel? ci 1 0) (rel 1 0))
       ;; all chains of length 3
       (test-equal (chain-rel? ci 0 0 0) (rel 0 0 0))
       (test-equal (chain-rel? ci 0 0 1) (rel 0 0 1))
       (test-equal (chain-rel? ci 0 1 0) (rel 0 1 0))
       (test-equal (chain-rel? ci 1 0 0) (rel 1 0 0))
       (test-equal (chain-rel? ci 1 1 0) (rel 1 1 0))
       (test-equal (chain-rel? ci 1 0 1) (rel 1 0 1))
       (test-equal (chain-rel? ci 0 1 1) (rel 0 1 1))
       (test-equal (chain-rel? ci 0 1 2) (rel 0 1 2))
       (test-equal (chain-rel? ci 0 2 1) (rel 0 2 1))
       (test-equal (chain-rel? ci 1 2 0) (rel 1 2 0))
       (test-equal (chain-rel? ci 1 0 2) (rel 1 0 2))
       (test-equal (chain-rel? ci 2 0 1) (rel 2 0 1))
       (test-equal (chain-rel? ci 2 1 0) (rel 2 1 0))
       ;; check if all arguments are used
       (test-equal (arguments-used (chain-rel? ci 0)) '(0))
       (test-equal (arguments-used (chain-rel? ci 0 1)) '(0 1))
       (test-equal (arguments-used (chain-rel? ci 1 0)) '(0 1))
       (test-equal (arguments-used (chain-rel? ci 0 1 2)) '(0 1 2))
       (test-equal (arguments-used (chain-rel? ci 0 2 1)) '(0 1 2))
       (test-equal (arguments-used (chain-rel? ci 1 2 0)) '(0 1 2))
       (test-equal (arguments-used (chain-rel? ci 1 0 2)) '(0 1 2))
       (test-equal (arguments-used (chain-rel? ci 2 0 1)) '(0 1 2))
       (test-equal (arguments-used (chain-rel? ci 2 1 0)) '(0 1 2))))))

(define pairwise-not=?:long-sequences
  (let ()
    (define (extremal-pivot-sequence r)
      ;; The extremal pivot sequence of order r is a 
      ;; permutation of {0..2^(r+1)-2} such that the
      ;; middle element is minimal, and this property
      ;; holds recursively for each binary subdivision.
      ;;   This sequence exposes a naive implementation of
      ;; pairwise-not=? chosing the middle element as pivot.
      (if (zero? r)
          '(0)
          (let* ((s (extremal-pivot-sequence (- r 1)))
                 (ns (length s)))
            (append (list-ec (:list x s) (+ x 1))
                    '(0)
                    (list-ec (:list x s) (+ x ns 1))))))
    (list (list-ec (: i 4096) i)
          (list-ec (: i 4097 0 -1) i)
          (list-ec (: i 4099) (modulo (* 1003 i) 4099))
          (extremal-pivot-sequence 11))))

(define pairwise-not=?:short-sequences
  (let ()
    (define (combinations/repeats n l)
      ;; return list of all sublists of l of size n,
      ;; the order of the elements occur in the sublists 
      ;; of the output is the same as in the input
      (let ((len (length l)))
        (cond
         ((= n 0)   '())
         ((= n 1)   (map list l))
         ((= len 1) (do ((r '() (cons (car l) r))
                         (i n (- i 1)))
                        ((= i 0) (list r))))
         (else      (append (combinations/repeats n (cdr l))
                            (map (lambda (c) (cons (car l) c))
                                 (combinations/repeats (- n 1) l)))))))
    (define (permutations l)
      ;; return a list of all permutations of l
      (let ((len (length l)))
        (cond
         ((= len 0) '(()))
         ((= len 1) (list l))
         (else      (apply append
                           (map (lambda (p) (insert-every-where (car l) p))
                                (permutations (cdr l))))))))      
    (define (insert-every-where x xs)
      (let loop ((result '()) (before '()) (after  xs))
        (let ((new (append before (cons x after))))
          (cond
           ((null? after) (cons new result))
           (else          (loop (cons new result)
                                (append before (list (car after)))
                                (cdr after))))))) 
    (define (sequences n max)
      (apply append
             (map permutations
                  (combinations/repeats n (list-ec (: i max) i)))))
    (append-ec (: n 5) (sequences n 5))))

(define (colliding-compare x y)
  (ci (modulo x 3) (modulo y 3)))

(define (naive-pairwise-not=? compare . xs)
  (let ((xs (list->vector xs)))
    (every?-ec (:range i (- (vector-length xs) 1))
               (:let xs-i (vector-ref xs i))
               (:range j (+ i 1) (vector-length xs))
               (:let xs-j (vector-ref xs j))
               (not=? compare xs-i xs-j))))

;; min/max

(define min/max:sequences
  (append pairwise-not=?:short-sequences
          pairwise-not=?:long-sequences))


; kth-largest

(define kth-largest:sequences
  pairwise-not=?:short-sequences)

(define (naive-kth-largest compare k . xs)
  (let ((vec (list->vector xs)))
    ;; bubble sort: simple, stable, O(|xs|^2)
    (do-ec (:range n (- (vector-length vec) 1))
           (:range i 0 (- (- (vector-length vec) 1) n))
           (if>? (compare (vector-ref vec i)
                          (vector-ref vec (+ i 1)))
                 (let ((vec-i (vector-ref vec i)))
                   (vector-set! vec i (vector-ref vec (+ i 1)))
                   (vector-set! vec (+ i 1) vec-i))))
    (vector-ref vec (modulo k (vector-length vec)))))

(define (sort-by-less xs pred)          ; trivial quicksort
  (if (or (null? xs) (null? (cdr xs)))
      xs
      (append 
       (sort-by-less (list-ec (:list x (cdr xs))
			      (if (pred x (car xs))) 
			      x) 
		     pred)
       (list (car xs))
       (sort-by-less (list-ec (:list x (cdr xs))
			      (if (not (pred x (car xs))))
			      x) 
		     pred))))


;; Actual tests
;; =============================================================================

;; basic functionality

(test-equal (if3 -1 'n 'z 'p) 'n)
(test-equal (if3  0 'n 'z 'p) 'z)
(test-equal (if3  1 'n 'z 'p) 'p)

;; check arguments are evaluated only once

(test-equal 
 (let ((x -1))
   (if3 (let ((x0 x)) (set! x (+ x 1)) x0) 'n 'z 'p))
 'n)

(test-equal 
 (let ((x -1) (y 0)) 
   (if3 (let ((x0 x)) (set! x (+ x 1)) x0)
        (begin (set! y (+ y 1))   y)
        (begin (set! y (+ y 10))  y)
        (begin (set! y (+ y 100)) y)))
 1)

(test-equal 
 (let ((x 0) (y 0)) 
   (if3 (let ((x0 x)) (set! x (+ x 1)) x0)
        (begin (set! y (+ y 1))   y)
        (begin (set! y (+ y 10))  y)
        (begin (set! y (+ y 100)) y)))
 10)

(test-equal 
 (let ((x 1) (y 0)) 
   (if3 (let ((x0 x)) (set! x (+ x 1)) x0)
        (begin (set! y (+ y 1))   y)
        (begin (set! y (+ y 10))  y)
        (begin (set! y (+ y 100)) y)))
 100)

;; (define (check:ifs)
  
(test-equal-if2 if=?     =)
(test-equal-if2 if<?     <)
(test-equal-if2 if>?     >)
(test-equal-if2 if<=?    <=)
(test-equal-if2 if>=?    >=)
(test-equal-if2 if-not=? (lambda (x y) (not (= x y))))

; <? etc. macros

(test-equal-chain2 =?    =)
(test-equal-chain2 <?    <)
(test-equal-chain2 >?    >)
(test-equal-chain2 <=?   <=)
(test-equal-chain2 >=?   >=)
(test-equal-chain2 not=? (lambda (x y) (not (= x y))))

(test-equal-chain3 </<?   <  <)
(test-equal-chain3 </<=?  <  <=)
(test-equal-chain3 <=/<?  <= <)
(test-equal-chain3 <=/<=? <= <=)

(test-equal-chain3 >/>?   >  >)
(test-equal-chain3 >/>=?  >  >=)
(test-equal-chain3 >=/>?  >= >)
(test-equal-chain3 >=/>=? >= >=)

(test-equal-chain chain=?  =)
(test-equal-chain chain<?  <)
(test-equal-chain chain>?  >)
(test-equal-chain chain<=? <=)
(test-equal-chain chain>=? >=)

;; 0-ary, 1-ary
(test-equal (pairwise-not=? ci)   #t)
(test-equal (pairwise-not=? ci 0) #t)

;; 2-ary
(test-equal (pairwise-not=? ci 0 0) #f)
(test-equal (pairwise-not=? ci 0 1) #t)
(test-equal (pairwise-not=? ci 1 0) #t)

;; 3-ary
(test-equal (pairwise-not=? ci 0 0 0) #f)
(test-equal (pairwise-not=? ci 0 0 1) #f)
(test-equal (pairwise-not=? ci 0 1 0) #f)
(test-equal (pairwise-not=? ci 1 0 0) #f)
(test-equal (pairwise-not=? ci 1 1 0) #f)
(test-equal (pairwise-not=? ci 1 0 1) #f)
(test-equal (pairwise-not=? ci 0 1 1) #f)
(test-equal (pairwise-not=? ci 0 1 2) #t)
(test-equal (pairwise-not=? ci 0 2 1) #t)
(test-equal (pairwise-not=? ci 1 2 0) #t)
(test-equal (pairwise-not=? ci 1 0 2) #t)
(test-equal (pairwise-not=? ci 2 0 1) #t)
(test-equal (pairwise-not=? ci 2 1 0) #t)

;; n-ary, n large: [0..n-1], [n,n-1..1], 5^[0..96] mod 97
(test-equal (apply pairwise-not=? ci (list-ec (: i 10) i)) #t)
(test-equal (apply pairwise-not=? ci (list-ec (: i 100) i)) #t)
(test-equal (apply pairwise-not=? ci (list-ec (: i 1000) i)) #t)

(test-equal (apply pairwise-not=? ci (list-ec (: i 10 0 -1) i)) #t)
(test-equal (apply pairwise-not=? ci (list-ec (: i 100 0 -1) i)) #t)
(test-equal (apply pairwise-not=? ci (list-ec (: i 1000 0 -1) i)) #t)

(test-equal (apply pairwise-not=? ci 
                   (list-ec (: i 97) (modulo (* 5 i) 97)))
            #t)

;; bury another copy of 72 = 5^50 mod 97 in 5^[0..96] mod 97
(test-equal (apply pairwise-not=? ci 
                   (append (list-ec (: i 0 23) (modulo (* 5 i) 97))
                           '(72)
                           (list-ec (: i 23 97) (modulo (* 5 i) 97))))
            #f)
(test-equal (apply pairwise-not=? ci 
                   (append (list-ec (: i 0 75) (modulo (* 5 i) 97))
                           '(72)
                           (list-ec (: i 75 97) (modulo (* 5 i) 97))))
            #f)

;; check if all arguments are used
(test-equal (arguments-used (pairwise-not=? ci 0)) '(0))
(test-equal (arguments-used (pairwise-not=? ci 0 1)) '(0 1))
(test-equal (arguments-used (pairwise-not=? ci 1 0)) '(0 1))
(test-equal (arguments-used (pairwise-not=? ci 0 2 1)) '(0 1 2))
(test-equal (arguments-used (pairwise-not=? ci 1 2 0)) '(0 1 2))
(test-equal (arguments-used (pairwise-not=? ci 1 0 2)) '(0 1 2))
(test-equal (arguments-used (pairwise-not=? ci 2 0 1)) '(0 1 2))
(test-equal (arguments-used (pairwise-not=? ci 2 1 0)) '(0 1 2))
(test-equal (arguments-used (pairwise-not=? ci 0 0 0 1 0 0 0 2 0 0 0 3))
            '(0 1 2 3))

;; all lists of length 1,2,3
(test-equal (min-compare ci 0) 0)
(test-equal (min-compare ci 0 0) 0)
(test-equal (min-compare ci 0 1) 0)
(test-equal (min-compare ci 1 0) 0)
(test-equal (min-compare ci 0 0 0) 0)
(test-equal (min-compare ci 0 0 1) 0)
(test-equal (min-compare ci 0 1 0) 0)
(test-equal (min-compare ci 1 0 0) 0)
(test-equal (min-compare ci 1 1 0) 0)
(test-equal (min-compare ci 1 0 1) 0)
(test-equal (min-compare ci 0 1 1) 0)
(test-equal (min-compare ci 0 1 2) 0)
(test-equal (min-compare ci 0 2 1) 0)
(test-equal (min-compare ci 1 2 0) 0)
(test-equal (min-compare ci 1 0 2) 0)
(test-equal (min-compare ci 2 0 1) 0)
(test-equal (min-compare ci 2 1 0) 0)

(test-equal (max-compare ci 0) 0)
(test-equal (max-compare ci 0 0) 0)
(test-equal (max-compare ci 0 1) 1)
(test-equal (max-compare ci 1 0) 1)
(test-equal (max-compare ci 0 0 0) 0)
(test-equal (max-compare ci 0 0 1) 1)
(test-equal (max-compare ci 0 1 0) 1)
(test-equal (max-compare ci 1 0 0) 1)
(test-equal (max-compare ci 1 1 0) 1)
(test-equal (max-compare ci 1 0 1) 1)
(test-equal (max-compare ci 0 1 1) 1)
(test-equal (max-compare ci 0 1 2) 2)
(test-equal (max-compare ci 0 2 1) 2)
(test-equal (max-compare ci 1 2 0) 2)
(test-equal (max-compare ci 1 0 2) 2)
(test-equal (max-compare ci 2 0 1) 2)
(test-equal (max-compare ci 2 1 0) 2)

;; check that the first minimal value is returned
(test-equal (min-compare (pair-compare-car ci)
                         '(0 1) '(0 2) '(0 3))
            '(0 1))
(test-equal (max-compare (pair-compare-car ci)
                         '(0 1) '(0 2) '(0 3))
            '(0 1))
  
;; test if arguments are only evaluated once
  
(test-equal
 (let ((nx 0) (ny 0) (nt 0))
   (select-compare (begin (set! nx (+ nx 1)) 1)
                   (begin (set! ny (+ ny 1)) 2)
                   ((lambda (z) (set! nt (+ nt   1)) #f) 0)
                   ((lambda (z) (set! nt (+ nt  10)) #f) 0)
                   ((lambda (z) (set! nt (+ nt 100)) #f) 0)
                   (else 0))
   (list nx ny nt))
 '(1 1 222))

;; examples from SRFI text for <? etc.

(test-equal (>? "laugh" "LOUD") #t)
(test-equal (<? string-compare-ci "laugh" "LOUD") #t)
(test-equal (sort-by-less '(1 a "b") (<?)) '("b" a 1))
(test-equal (sort-by-less '(1 a "b") (>?)) '(1 a "b"))

(test-end)
