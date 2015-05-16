;;!!! A straightforward implementation of "Ropes, An Alternative to Strings"
;;   H. Boehm, R. Atkinson, M. Plass
;;   Software Practice and Experience 25, Dec 1995, pp. 1315-1330
;;
;; .author Evan Hanson, 2013
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 2013, Evan Hanson <evhan@foldling.org>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;   * The name of the author may not be used to endorse or promote products
;;     derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED
;; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Source: https://bitbucket.org/evhan/rope.git
;; commit 5cfc01404866cc6c9e1695b7fef87a6149a82c07
;; Author: Evan Hanson <evhan@foldling.org>
;; Date:   Fri Nov 22 14:34:06 2013 +1300

(define string-copy!
  (case-lambda
   ((s1 s2)
    (string-copy! s1 0 s2 0 (string-length s2)))
   ((s1 i s2)
    (string-copy! s1 i s2 0 (string-length s2)))
   ((s1 i s2 s)
    (string-copy! s1 i s2 s (string-length s2)))
   ((s1 i s2 s e)
    (do ((i i (+ i 1))
         (s s (+ s 1)))
        ((= s e) s1)
      (string-set! s1 i (string-ref s2 s))))))

(define string-reverse
  (case-lambda
   ((s1)
    (string-reverse s1 0 (string-length s1)))
   ((s1 s)
    (string-reverse s1 s (string-length s1)))
   ((s1 s e)
    (let* ((len (- e s))
           (str (make-string len)))
      (do ((s s (+ s 1))
           (e (- len 1) (- e 1)))
          ((< e 0) str)
        (string-set! str e (string-ref s1 s)))))))

(define (fold f a l)
  (if (null? l)
      a
      (fold f (f (car l) a) (cdr l))))

(define (split-at l n)
  (let lp ((a '()) (l l) (n n))
    (if (zero? n)
        (values (reverse a) l)
        (lp (cons (car l) a)
            (cdr l)
            (- n 1)))))

(define (take l n)
  (call-with-values
      (lambda () (split-at l n))
    (lambda (l r) l)))

(define (find p l)
  (and (not (null? l))
       (let ((v (car l)))
         (if (p v)
             v
             (find p (cdr l))))))

(define-record-type rope-node
  (%make-node left right length depth)
  node?
  (left node-left)
  (right node-right)
  (length node-length)
  (depth node-depth))

(define-record-type rope-leaf
  (%make-leaf string length)
  leaf?
  (string leaf-string)
  (length leaf-length))

(define eof
  (read (open-input-string "")))

(define (⌊n/2⌋ n)
  (inexact->exact (floor (/ n 2))))

(define current-maximum-leaf-length
  (make-parameter 512))

(define empty-rope
  (%make-leaf "" 0))

(define (rope? o)
  (or (leaf? o)
      (node? o)))

(define (rope-length r)
  (if (leaf? r)
      (leaf-length r)
      (node-length r)))

(define (rope-depth r)
  (if (leaf? r)
      0
      (node-depth r)))

(define (rope-null? r)
  (= (rope-length r) 0))

(define make-leaf
  (case-lambda
   ((s)
    (%make-leaf s (string-length s)))
   ((s len)
    (%make-leaf s len))))

(define make-node
  (case-lambda
   ((l r)
    (make-node l r (+ (rope-length l) (rope-length r))))
   ((l r len)
    (%make-node l r len (+ (max (rope-depth l) (rope-depth r)) 1)))
   ((l r len dep)
    (%make-node l r len dep))))

(define (rope->tree r)
  (if (leaf? r)
      (leaf-string r)
      (list
       (rope->tree (node-left r))
       (rope->tree (node-right r)))))

(define (tree->rope t)
  (if (string? t)
      (make-leaf t)
      (make-node
       (tree->rope (car t))
       (tree->rope (cadr t)))))

(define (string->rope s)
  (let lp ((s s))
    (let ((len (string-length s)))
      (if (<= len (current-maximum-leaf-length))
          (make-leaf s len)
          (let ((mid (⌊n/2⌋ len)))
            (make-node
             (lp (substring s 0 mid))
             (lp (substring s mid len))
             len))))))

(define (rope->string r)
  (if (leaf? r)
      (leaf-string r)
      (let* ((len (rope-length r))
             (str (make-string len)))
        (rope-fold-leaves
         (lambda (leaf i)
           (string-copy! str i (leaf-string leaf))
           (+ i (leaf-length leaf)))
         0
         r)
        str)))

(define rope
  (case-lambda
   (() empty-rope)
   ((s) (string->rope s))
   (args (rope-concatenate (map string->rope args)))))

(define (rope-ref r i)
  (if (or (negative? 0)
          (>= i (rope-length r)))
      (error "out of range" r i)
      (let lp ((r r)
               (i i))
        (if (leaf? r)
            (string-ref (leaf-string r) i)
            (let* ((l (node-left r))
                   (m (rope-length l)))
              (if (< i m)
                  (lp l i)
                  (lp (node-right r)
                      (- i m))))))))

(define subrope
  (case-lambda
   ((r s)
    (subrope r s (rope-length r)))
   ((r s e)
    (if (or (negative? s)
            (> s e)
            (> s (rope-length r)))
        (error "out of range" r s)
        (let lp ((r r)
                 (s s)
                 (e e))
          (if (leaf? r)
              (if (and (= s 0) (= e (leaf-length r)))
                  r
                  (make-leaf
                   (substring (leaf-string r) s e)))
              (let* ((l (node-left r))
                     (r (node-right r))
                     (m (rope-length l)))
                (cond
                 ((<= e m)
                  (lp l s e))
                 ((>= s m)
                  (lp r (- s m) (- e m)))
                 ((and (= s 0) (= e (rope-length r)))
                  r)
                 (else
                  (make-node
                   (lp l s m)
                   (lp r 0 (- e m))))))))))))

;; Characterwise iterator over rope r.
(define (make-rope-iterator r)
  (letrec ((k (if (leaf? r)
                  (let ((len (leaf-length r))
                        (str (leaf-string r)))
                    (lambda ()
                      (let lp ((i 0))
                        (cond ((= i len) eof)
                              (else
                               (set! k (lambda () (lp (+ i 1))))
                               (string-ref str i))))))
                  (let ((i (make-rope-iterator (node-left r))))
                    (lambda ()
                      (let ((c (i)))
                        (cond ((char? c) c)
                              (else
                               (set! k (make-rope-iterator (node-right r)))
                               (k)))))))))
    (lambda () (k))))

;; Characterwise left fold.
(define rope-fold
  (let ()
    ;; Single-rope version.
    (define (rope-fold-1 f a r)
      (if (leaf? r)
          (fold f a (string->list (leaf-string r)))
          (rope-fold-1 f (rope-fold f a (node-left r)) (node-right r))))
    ;; For n-ary kons.
    (lambda (f a r1 . rn)
      (if (null? rn)
          (rope-fold-1 f a r1)
          (let ((is (map make-rope-iterator (cons r1 rn))))
            (let lp ((a a))
              (let ((cs (map (lambda (f) (f)) is)))
                (if (find eof-object? cs)
                    a
                    (lp (apply f (append cs (list a))))))))))))

;; Leafwise left fold.
(define (rope-fold-leaves f a r)
  (if (leaf? r)
      (f r a)
      (rope-fold-leaves f (rope-fold-leaves f a (node-left r)) (node-right r))))

(define (rope-for-each f r1 . rn)
  (if (null? rn)
      (rope-fold (lambda (c a) (f c)) #f r1)
      (let ((n (+ (length rn) 1)))
        (apply rope-fold (lambda args (apply f (take args n))) #f r1 rn)))
  (void))

(define rope=?
  (let ((immediate-rope=?
         (lambda (r1 r2)
           (or (eq? r1 r2)
               (and (leaf? r1)
                    (leaf? r2)
                    (= (rope-length r1)
                       (rope-length r2))
                    (string=? (leaf-string r1)
                              (leaf-string r2)))))))
    (case-lambda
     ((r1 r2)
      (or (immediate-rope=? r1 r2)
          (call-with-current-continuation
           (lambda (k)
             (rope-for-each
              (lambda (c1 c2)
                (or (char=? c1 c2)
                    (k #f)))
              r1
              r2)
             #t))))
     ((r1 . rn)
      (or (not (find (lambda (r2) (not (immediate-rope=? r1 r2))) rn))
          (call-with-current-continuation
           (lambda (k)
             (apply
              rope-for-each
              (lambda chars
                (or (apply char=? chars)
                    (k #f)))
              r1
              rn)
             #t)))))))

(define-values (rope-balanced? rope-balance)
  (let ()
    (define √5 (sqrt 5))
    (define φ (/ (+ √5 1) 2))
    (define (fib n)
      (inexact->exact (floor (+ (/ (expt φ n) √5) .5))))
    (define (unfib n)                   ; for n > 2
      (inexact->exact (floor (/ (log (+ (* n √5) .5)) (log φ)))))
    (define first-50-fibs
      '#(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765
           10946 17711 28657 46368 75025 121393 196418 317811 514229 832040
           1346269 2178309 3524578 5702887 9227465 14930352 24157817 39088169
           63245986 102334155 165580141 267914296 433494437 701408733 1134903170
           1836311903 2971215073 4807526976 7778742049))
    (define (fib* n)
      (if (< n 50)
          (vector-ref first-50-fibs n)
          (fib n)))
    (define (fib-length r)
      (fib* (unfib (max (rope-length r) 1))))
    (define (reorder r)
      (rope-fold-leaves
       (lambda (leaf subropes)
         (let ((len (fib-length leaf)))
           (if (or (null? subropes)
                   (< len (fib-length (car subropes))))
               (cons leaf subropes)
               (let ll ((l '())
                        (r subropes))
                 (if (< len (fib-length (car r)))
                     (ll (cons (car r) l)
                         (cdr r))
                     (let lr ((l (fold make-node leaf l))
                              (r r))
                       (if (or (null? r)
                               (< (fib-length l)
                                  (fib-length (car r))))
                           (cons l r)
                           (lr (make-node (car r) l)
                               (cdr r)))))))))
       '()
       r))
    (values
     (lambda (r)
       (let ((l (rope-length r)))
         (or (zero? l)
             (>= l (fib* (+ (rope-depth r) 2))))))
     (lambda (r)
       (let ((n (reorder r)))
         (fold make-node (car n) (cdr n)))))))

(define rope-append
  (let ()
    (define (leaf-append r1 r2)
      (let ((len (+ (leaf-length r1) (leaf-length r2))))
        (make-leaf (string-append (leaf-string r1) (leaf-string r2)) len)))
    ;; Flatten r2 onto r1 if both are short leaves.
    (define (rope-append r1 r2)
      (let* ((len1 (rope-length r1))
             (len2 (rope-length r2))
             (len  (+ len1 len2)))
        (cond
         ((zero? len2) r1)
         ((zero? len1) r2)
         ((leaf? r2)
          (let ((mll (current-maximum-leaf-length)))
            (if (leaf? r1)
                (if (<= len mll)
                    (leaf-append r1 r2)
                    (make-node r1 r2 len))
                (let ((r1r (node-right r1)))
                  (if (and (leaf? r1r)
                           (<= (+ (rope-length r1r) len2) mll))
                      (make-node (node-left r1) (leaf-append r1r r2))
                      (make-node r1 r2 len))))))
         (else
          (make-node r1 r2 len)))))
    (define (ensure-balanced r)
      (if (rope-balanced? r)
          r
          (rope-balance r)))
    (case-lambda
     (() empty-rope)
     ((r1) r1)
     ((r1 r2)
      (ensure-balanced
       (rope-append r1 r2)))
     ((r1 r2 r3)
      (ensure-balanced
       (rope-append (rope-append r1 r2) r3)))
     ((r1 r2 r3 r4)
      (ensure-balanced
       (rope-append
        (rope-append r1 r2)
        (rope-append r3 r4))))
     (ropes
      (rope-concatenate ropes)))))

(define (rope-concatenate ropes)
  (let lp ((ropes ropes)
           (middle (⌊n/2⌋ (length ropes))))
    (if (< middle 2)
        (apply rope-append ropes)
        (call-with-values
            (lambda ()
              (split-at ropes middle))
          (lambda (l r)
            (let ((m (⌊n/2⌋ middle)))
              (make-node (lp l m) (lp r m))))))))

;; Ssssllllloooowwwwwww.
(define (rope-reverse r)
  (rope-concatenate
   (rope-fold-leaves
    (lambda (r a)
      (cons (make-leaf (string-reverse (leaf-string r))) a))
    '()
    r)))

(define open-output-rope open-output-string)

(define get-output-rope
  (case-lambda
   (()
    (get-output-rope (current-output-port)))
   ((port)
    (string->rope (get-output-string port)))))

(define read-rope
  (case-lambda
   (()
    (read-rope (current-input-port) +inf.0))
   ((port)
    (read-rope port +inf.0))
   ((port len)
    (let ((mll (current-maximum-leaf-length)))
      (let lp ((n 0)
               (a '()))
        (if (>= n len)
            (rope-concatenate (reverse a))
            (let ((len (min len mll)))
              (let ((s (open-output-string)))
                (let ll ((i 0))
                  (if (>= i len)
                      (lp (+ n i)
                          (cons (make-leaf (get-output-string s)) a))
                      (let ((c (read-char port)))
                        (if (eof-object? c)
                            (ll +inf.0)
                            (begin
                              (display c s)
                              (ll (+ i 1)))))))))))))))
