;;!!! Hamming distance
;; .author Álvaro Castro-Castilla, 2012-2015. See LICENSE file.


(declare (mostly-fixnum))

;;! Calculates the hamming distance (number of different positions) of two lists
;; of equal length
(define (hamming-distance la lb)
  ((letrec ((H (lambda (a b distance)
                 (cond
                  ((and (null? a) (null? b))
                   distance)
                  ((or (null? a) (null? b))
                   (error "lists have different length"))
                  ((equal? (car a) (car b))
                   (H (cdr a) (cdr b) distance))
                  (else
                   (H (cdr a) (cdr b) (+ distance 1))))))) H) la lb 0))

;;! Calculates the minimum hamming distance between a list and the permutations
;; of the second
(define (hamming-distance-all-permutations la lb)
  (define (find-remove pred lis)
    (call/cc
     (lambda (failed)
       ((letrec ((R (lambda (l)
                      (if (null? l)
                          (failed #f)
                          (receive (h t) (car+cdr l)
                                   (if (pred h)
                                       t
                                       (cons h (R t)))))))) R) lis))))
  ((letrec ((H (lambda (a b d)
                 ;; a always decreases
                 (if (null? a)          
                     ;; the length must be equal to the times it wasn't cdr'd
                     (if (= d (length b))
                         d
                         (error "lists have different length"))
                     (let ((newb (find-remove (lambda (x) (eq? x (car a))) b)))
                       (if newb
                           (H (cdr a) newb d)
                           (H (cdr a) b (+ d 1)))))))) H) la lb 0))
