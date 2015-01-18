;; Based on reference implementation by sebastian.egner@philips.com

(##spheres-load energy: testing)
(##spheres-load algorithm/comprehension)

(test-begin "SRFI-42: Eager comprehensions" 157)

; ==========================================================================
; do-ec 
; ==========================================================================

(test-equal 
  (let ((x 0)) (do-ec (set! x (+ x 1))) x)
  1)

(test-equal 
  (let ((x 0)) (do-ec (:range i 10) (set! x (+ x 1))) x)
  10)

(test-equal 
  (let ((x 0)) (do-ec (:range n 10) (:range k n) (set! x (+ x 1))) x)
  45)

; ==========================================================================
; list-ec and basic qualifiers 
; ==========================================================================

(test-equal (list-ec 1) '(1))

(test-equal (list-ec (:range i 4) i) '(0 1 2 3))

(test-equal (list-ec (:range n 3) (:range k (+ n 1)) (list n k)) 
  '((0 0) (1 0) (1 1) (2 0) (2 1) (2 2)) )

(test-equal 
  (list-ec (:range n 5) (if (even? n)) (:range k (+ n 1)) (list n k)) 
  '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4)) )

(test-equal 
  (list-ec (:range n 5) (not (even? n)) (:range k (+ n 1)) (list n k)) 
  '((1 0) (1 1) (3 0) (3 1) (3 2) (3 3)) )

(test-equal
  (list-ec (:range n 5) 
           (and (even? n) (> n 2)) 
           (:range k (+ n 1)) 
           (list n k) )
  '((4 0) (4 1) (4 2) (4 3) (4 4)) )

(test-equal
  (list-ec (:range n 5) 
           (or (even? n) (> n 3)) 
           (:range k (+ n 1)) 
           (list n k) )
  '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4)) )

(test-equal
 (let ((x 0)) (list-ec (:range n 10) (begin (set! x (+ x 1))) n) x)
 10 )

(test-equal
 (list-ec (nested (:range n 3) (:range k n)) k)
 '(0 0 1) )


; ==========================================================================
; Other comprehensions
; ==========================================================================

(test-equal (append-ec '(a b)) '(a b))
(test-equal (append-ec (:range i 0) '(a b)) '())
(test-equal (append-ec (:range i 1) '(a b)) '(a b))
(test-equal (append-ec (:range i 2) '(a b)) '(a b a b))

(test-equal (string-ec #\a) (string #\a))
(test-equal (string-ec (:range i 0) #\a) "")
(test-equal (string-ec (:range i 1) #\a) "a")
(test-equal (string-ec (:range i 2) #\a) "aa")

(test-equal (string-append-ec "ab") "ab")
(test-equal (string-append-ec (:range i 0) "ab") "")
(test-equal (string-append-ec (:range i 1) "ab") "ab")
(test-equal (string-append-ec (:range i 2) "ab") "abab")

(test-equal (vector-ec 1) (vector 1))
(test-equal (vector-ec (:range i 0) i) (vector))
(test-equal (vector-ec (:range i 1) i) (vector 0))
(test-equal (vector-ec (:range i 2) i) (vector 0 1))

(test-equal (vector-of-length-ec 1 1) (vector 1))
(test-equal (vector-of-length-ec 0 (:range i 0) i) (vector))
(test-equal (vector-of-length-ec 1 (:range i 1) i) (vector 0))
(test-equal (vector-of-length-ec 2 (:range i 2) i) (vector 0 1))

(test-equal (sum-ec 1) 1)
(test-equal (sum-ec (:range i 0) i) 0)
(test-equal (sum-ec (:range i 1) i) 0)
(test-equal (sum-ec (:range i 2) i) 1)
(test-equal (sum-ec (:range i 3) i) 3)

(test-equal (product-ec 1) 1)
(test-equal (product-ec (:range i 1 0) i) 1)
(test-equal (product-ec (:range i 1 1) i) 1)
(test-equal (product-ec (:range i 1 2) i) 1)
(test-equal (product-ec (:range i 1 3) i) 2)
(test-equal (product-ec (:range i 1 4) i) 6)

(test-equal (min-ec 1) 1)
(test-equal (min-ec (:range i 1) i) 0)
(test-equal (min-ec (:range i 2) i) 0)

(test-equal (max-ec 1) 1)
(test-equal (max-ec (:range i 1) i) 0)
(test-equal (max-ec (:range i 2) i) 1)

(test-equal (first-ec #f 1) 1)
(test-equal (first-ec #f (:range i 0) i) #f)
(test-equal (first-ec #f (:range i 1) i) 0)
(test-equal (first-ec #f (:range i 2) i) 0)

(test-equal 
  (let ((last-i -1))
    (first-ec #f (:range i 10) (begin (set! last-i i)) i)
    last-i )
  0)

(test-equal (last-ec #f 1) 1)
(test-equal (last-ec #f (:range i 0) i) #f)
(test-equal (last-ec #f (:range i 1) i) 0)
(test-equal (last-ec #f (:range i 2) i) 1)

(test-equal (any?-ec #f) #f)
(test-equal (any?-ec #t) #t)
(test-equal (any?-ec (:range i 2 2) (even? i)) #f)
(test-equal (any?-ec (:range i 2 3) (even? i)) #t)

(test-equal (every?-ec #f) #f)
(test-equal (every?-ec #t) #t)
(test-equal (every?-ec (:range i 2 2) (even? i)) #t)
(test-equal (every?-ec (:range i 2 3) (even? i)) #t)
(test-equal (every?-ec (:range i 2 4) (even? i)) #f)

(test-equal 
 (let ((sum-sqr (lambda (x result) (+ result (* x x)))))
   (fold-ec 0 (:range i 10) i sum-sqr) )
 285 )

(test-equal 
 (let ((minus-1 (lambda (x) (- x 1)))
       (sum-sqr (lambda (x result) (+ result (* x x)))))
   (fold3-ec (error "wrong") (:range i 10) i minus-1 sum-sqr) )
 284 )

(test-equal 
 (fold3-ec 'infinity (:range i 0) i min min)
 'infinity )


; ==========================================================================
; Typed generators
; ==========================================================================

(test-equal (list-ec (:list x '()) x) '())
(test-equal (list-ec (:list x '(1)) x) '(1))
(test-equal (list-ec (:list x '(1 2 3)) x) '(1 2 3))
(test-equal (list-ec (:list x '(1) '(2)) x) '(1 2))
(test-equal (list-ec (:list x '(1) '(2) '(3)) x) '(1 2 3))

(test-equal (list-ec (:string c "") c) '())
(test-equal (list-ec (:string c "1") c) '(#\1))
(test-equal (list-ec (:string c "123") c) '(#\1 #\2 #\3))
(test-equal (list-ec (:string c "1" "2") c) '(#\1 #\2))
(test-equal (list-ec (:string c "1" "2" "3") c) '(#\1 #\2 #\3))

(test-equal (list-ec (:vector x (vector)) x) '())
(test-equal (list-ec (:vector x (vector 1)) x) '(1))
(test-equal (list-ec (:vector x (vector 1 2 3)) x) '(1 2 3))
(test-equal (list-ec (:vector x (vector 1) (vector 2)) x) '(1 2))
(test-equal 
 (list-ec (:vector x (vector 1) (vector 2) (vector 3)) x)
 '(1 2 3))

(test-equal (list-ec (:range x -2) x) '())
(test-equal (list-ec (:range x -1) x) '())
(test-equal (list-ec (:range x  0) x) '())
(test-equal (list-ec (:range x  1) x) '(0))
(test-equal (list-ec (:range x  2) x) '(0 1))

(test-equal (list-ec (:range x  0  3) x) '(0 1 2))
(test-equal (list-ec (:range x  1  3) x) '(1 2))
(test-equal (list-ec (:range x -2 -1) x) '(-2))
(test-equal (list-ec (:range x -2 -2) x) '())

(test-equal (list-ec (:range x 1 5  2) x) '(1 3))
(test-equal (list-ec (:range x 1 6  2) x) '(1 3 5))
(test-equal (list-ec (:range x 5 1 -2) x) '(5 3))
(test-equal (list-ec (:range x 6 1 -2) x) '(6 4 2))

(test-equal (list-ec (:real-range x 0.0 3.0)     x) '(0. 1. 2.))
(test-equal (list-ec (:real-range x 0   3.0)     x) '(0. 1. 2.))
(test-equal (list-ec (:real-range x 0   3   1.0) x) '(0. 1. 2.))

(test-equal 
 (string-ec (:char-range c #\a #\z) c) 
 "abcdefghijklmnopqrstuvwxyz" )

;; (test-equal
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-ec (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (my-call-with-input-file "tmp1"
;;     (lambda (port) (list-ec (:port x port read) x)) ))
;;  (list-ec (:range n 10) n) )

;; (test-equal 
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-ec (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (my-call-with-input-file "tmp1"                 
;;      (lambda (port) (list-ec (:port x port) x)) ))
;;  (list-ec (:range n 10) n) )


; ==========================================================================
; The special generators :do :let :parallel :while :until
; ==========================================================================

(test-equal (list-ec (:do ((i 0)) (< i 4) ((+ i 1))) i) '(0 1 2 3))

(test-equal 
 (list-ec 
  (:do (let ((x 'x)))
       ((i 0)) 
       (< i 4) 
       (let ((j (- 10 i))))
       #t
       ((+ i 1)) )
  j )
 '(10 9 8 7) )

(test-equal (list-ec (:let x 1) x) '(1))
(test-equal (list-ec (:let x 1) (:let y (+ x 1)) y) '(2))
(test-equal (list-ec (:let x 1) (:let x (+ x 1)) x) '(2))

(test-equal 
 (list-ec (:parallel (:range i 1 10) (:list x '(a b c))) (list i x))
 '((1 a) (2 b) (3 c)) )

(test-equal 
 (list-ec (:while (:range i 1 10) (< i 5)) i)
 '(1 2 3 4) )

(test-equal 
 (list-ec (:until (:range i 1 10) (>= i 5)) i)
 '(1 2 3 4 5) )

; with generator that might use inner bindings

(test-equal
 (list-ec (:while (:list i '(1 2 3 4 5 6 7 8 9)) (< i 5)) i)
 '(1 2 3 4) )
; Was broken in original reference implementation as pointed
; out by sunnan@handgranat.org on 24-Apr-2005 comp.lang.scheme.
; Refer to http://groups-beta.google.com/group/comp.lang.scheme/
; browse_thread/thread/f5333220eaeeed66/75926634cf31c038#75926634cf31c038

(test-equal 
 (list-ec (:until (:list i '(1 2 3 4 5 6 7 8 9)) (>= i 5)) i)
 '(1 2 3 4 5) )

(test-equal
 (list-ec (:while (:vector x (index i) '#(1 2 3 4 5))
		  (< x 10))
	  x)
 '(1 2 3 4 5))
; Was broken in reference implementation, even after fix for the
; bug reported by Sunnan, as reported by Jens-Axel Soegaard on
; 4-Jun-2007.

; combine :while/:until and :parallel

(test-equal
 (list-ec (:while (:parallel (:range i 1 10)
                             (:list j '(1 2 3 4 5 6 7 8 9)))
                  (< i 5))
          (list i j))
 '((1 1) (2 2) (3 3) (4 4)))

(test-equal
 (list-ec (:until (:parallel (:range i 1 10)
                             (:list j '(1 2 3 4 5 6 7 8 9)))
                  (>= i 5))
          (list i j))
 '((1 1) (2 2) (3 3) (4 4) (5 5)))

; check that :while/:until really stop the generator

(test-equal
 (let ((n 0))
   (do-ec (:while (:range i 1 10) (begin (set! n (+ n 1)) (< i 5)))
          (if #f #f))
   n)
 5)

(test-equal
 (let ((n 0))
   (do-ec (:until (:range i 1 10) (begin (set! n (+ n 1)) (>= i 5)))
          (if #f #f))
   n)
 5)

(test-equal
 (let ((n 0))
   (do-ec (:while (:parallel (:range i 1 10)
                             (:do () (begin (set! n (+ n 1)) #t) ()))
                  (< i 5))
          (if #f #f))
   n)
 5)

(test-equal
 (let ((n 0))
   (do-ec (:until (:parallel (:range i 1 10)
                             (:do () (begin (set! n (+ n 1)) #t) ()))
                  (>= i 5))
          (if #f #f))
   n)
 5)

; ==========================================================================
; The dispatching generator
; ==========================================================================

(test-equal (list-ec (: c '(a b)) c) '(a b))
(test-equal (list-ec (: c '(a b) '(c d)) c) '(a b c d))

(test-equal (list-ec (: c "ab") c) '(#\a #\b))
(test-equal (list-ec (: c "ab" "cd") c) '(#\a #\b #\c #\d))

(test-equal (list-ec (: c (vector 'a 'b)) c) '(a b))
(test-equal (list-ec (: c (vector 'a 'b) (vector 'c)) c) '(a b c))

(test-equal (list-ec (: i 0) i) '())
(test-equal (list-ec (: i 1) i) '(0))
(test-equal (list-ec (: i 10) i) '(0 1 2 3 4 5 6 7 8 9))
(test-equal (list-ec (: i 1 2) i) '(1))
(test-equal (list-ec (: i 1 2 3) i) '(1))
(test-equal (list-ec (: i 1 9 3) i) '(1 4 7))

;; (test-equal (list-ec (: i 0.0 1.0 0.2) i)
;;             '(0. 0.2 0.4 0.6 0.8))
(test-approximate (list-ref (list-ec (: i 0.0 1.0 0.2) i) 4)
                  0.8
                  0.001)

(test-equal (list-ec (: c #\a #\c) c) '(#\a #\b #\c))

;; (test-equal 
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-ec (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (my-call-with-input-file "tmp1"                 
;;      (lambda (port) (list-ec (: x port read) x)) ))
;;  (list-ec (:range n 10) n) )
    
;; (test-equal 
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-ec (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (my-call-with-input-file "tmp1"                 
;;      (lambda (port) (list-ec (: x port) x)) ))
;;  (list-ec (:range n 10) n) )


; ==========================================================================
; With index variable
; ==========================================================================

(test-equal (list-ec (:list c (index i) '(a b)) (list c i)) '((a 0) (b 1)))
(test-equal (list-ec (:string c (index i) "a") (list c i)) '((#\a 0)))
(test-equal (list-ec (:vector c (index i) (vector 'a)) (list c i)) '((a 0)))

(test-equal 
 (list-ec (:range i (index j) 0 -3 -1) (list i j)) 
 '((0 0) (-1 1) (-2 2)) )

;; (test-equal
;;  (list-ec (:real-range i (index j) 0 1 0.2) (list i j)) 
;;  '((0. 0) (0.2 1) (0.4 2) (0.6 3) (0.8 4)))
(test-approximate
 (car (list-ref (list-ec (:real-range i (index j) 0 1 0.2) (list i j))
                4)) 
 0.8
 0.001)

(test-equal 
 (list-ec (:char-range c (index i) #\a #\c) (list c i)) 
 '((#\a 0) (#\b 1) (#\c 2)) )

(test-equal 
 (list-ec (: x (index i) '(a b c d)) (list x i))
 '((a 0) (b 1) (c 2) (d 3)) )

;; (test-equal 
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-ec (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (my-call-with-input-file "tmp1"
;;      (lambda (port) (list-ec (: x (index i) port) (list x i))) ))
;;  '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9)) )


; ==========================================================================
; The examples from the SRFI document
; ==========================================================================

; from Abstract

(test-equal (list-ec (: i 5) (* i i)) '(0 1 4 9 16))

(test-equal 
  (list-ec (: n 1 4) (: i n) (list n i)) 
  '((1 0) (2 0) (2 1) (3 0) (3 1) (3 2)) )

; from Generators

(test-equal 
  (list-ec (: x (index i) "abc") (list x i)) 
  '((#\a 0) (#\b 1) (#\c 2)) )

(test-equal
  (list-ec (:string c (index i) "a" "b") (cons c i))
  '((#\a . 0) (#\b . 1)) )


; ==========================================================================
; Little Shop of Horrors
; ==========================================================================

(test-equal (list-ec (:range x 5) (:range x x) x) '(0 0 1 0 1 2 0 1 2 3))

(test-equal (list-ec (:list x '(2 "23" (4))) (: y x) y) '(0 1 #\2 #\3 4))

(test-equal 
 (list-ec (:parallel (:integers x) 
                     (:do ((i 10)) (< x i) ((- i 1))))
          (list x i))
 '((0 10) (1 9) (2 8) (3 7) (4 6)) )


; ==========================================================================
; Less artificial examples
; ==========================================================================

(define (factorial n) ; n * (n-1) * .. * 1 for n >= 0
  (product-ec (:range k 2 (+ n 1)) k) )

(test-equal (factorial  0) 1)
(test-equal (factorial  1) 1)
(test-equal (factorial  3) 6)
(test-equal (factorial  5) 120)


(define (eratosthenes n) ; primes in {2..n-1} for n >= 1
  (let ((p? (make-string n #\1)))
    (do-ec (:range k 2 n)
           (if (char=? (string-ref p? k) #\1))
           (:range i (* 2 k) n k)
           (string-set! p? i #\0) )
    (list-ec (:range k 2 n) (if (char=? (string-ref p? k) #\1)) k) ))

(test-equal 
 (eratosthenes 50)
 '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47) )

(test-equal
 (length (eratosthenes 100000))
 9592 ) ; we expect 10^5/ln(10^5)


(define (pythagoras n) ; a, b, c s.t. 1 <= a <= b <= c <= n, a^2 + b^2 = c^2
  (list-ec 
   (:let sqr-n (* n n))
   (:range a 1 (+ n 1))
   ;; (begin (display a) (display " "))
   (:let sqr-a (* a a))
   (:range b a (+ n 1)) 
   (:let sqr-c (+ sqr-a (* b b)))
   (if (<= sqr-c sqr-n))
   (:range c b (+ n 1))
   (if (= (* c c) sqr-c))
   (list a b c) ))
           
(test-equal
 (pythagoras 15)
 '((3 4 5) (5 12 13) (6 8 10) (9 12 15)) )

(test-equal
 (length (pythagoras 200))
 127 )


(define (qsort xs) ; stable
  (if (null? xs)
      '()
      (let ((pivot (car xs)) (xrest (cdr xs)))
        (append
         (qsort (list-ec (:list x xrest) (if (<  x pivot)) x))
         (list pivot)
         (qsort (list-ec (:list x xrest) (if (>= x pivot)) x)) ))))

(test-equal 
 (qsort '(1 5 4 2 4 5 3 2 1 3))
 '(1 1 2 2 3 3 4 4 5 5) )


(define (pi-BBP m) ; approx. of pi within 16^-m (Bailey-Borwein-Plouffe)
  (sum-ec 
    (:range n 0 (+ m 1))
    (:let n8 (* 8 n))
    (* (- (/ 4 (+ n8 1))
          (+ (/ 2 (+ n8 4))
             (/ 1 (+ n8 5))
             (/ 1 (+ n8 6))))
       (/ 1 (expt 16 n)) )))

(test-equal
 (pi-BBP 5)
 (/ 40413742330349316707 12864093722915635200) )


(define (read-line port) ; next line (incl. #\newline) of port
  (let ((line
         (string-ec 
          (:until (:port c port read-char)
                  (char=? c #\newline) )
          c )))
    (if (string=? line "")
        (read-char port) ; eof-object
        line)))

(define (read-lines filename) ; list of all lines
  (my-call-with-input-file 
   filename
   (lambda (port)
     (list-ec (:port line port read-line) line))))

;; (test-equal
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-ec (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (read-lines "tmp1") )
;;  (list-ec (:char-range c #\0 #\9) (string c #\newline)))

(test-end)
