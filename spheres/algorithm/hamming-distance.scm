;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List encodings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Distances
;-------------------------------------------------------------------------------

;;; Calculates the hamming distance (number of different positions) of two lists
;;; of equal length

(define (hamming-distance la lb)
  ((letrec ((H (lambda (a b d)
                 (cond
                  ((and (null? a) (null? b))
                   d)
                  ((xor (null? a) (null? b))
                   (error "lists have different length"))
                  ((equal? (car a) (car b))
                   (H (cdr a) (cdr b) d))
                  (else
                   (H (cdr a) (cdr b) (+ d 1))))))) H) la lb 0))

;;; Calculates the minimum hamming distance between a list and the permutations
;;; of the second

(define (min-hamming-distance la lb)
  ((letrec ((H (lambda (a b d)
                 (if (null? a)          ; a always decreases
                     (if (= d (length b)) ; the length must be equal to the times it wasn't cdr'd
                         d
                         (error "lists have different length"))
                     (let ((newb (find-rember (curry eq? (car a)) b))) ; Removes the first instance if found
                       (if newb
                           (H (cdr a) newb d)
                           (H (cdr a) b (+ d 1)))))))) H) la lb 0))

;-------------------------------------------------------------------------------
; Encodings
;-------------------------------------------------------------------------------

(define (run-length-encode l)
  (error "Not implemented"))

(define (run-length-decode l)
  (error "Not implemented"))

(define (lzw-encode l)
  (error "Not implemented"))

(define (lzw-decode l)
  (error "Not implemented"))
