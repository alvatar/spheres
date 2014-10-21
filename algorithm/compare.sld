;;!!! SRFI-67: Compare procedures
;; .author Sebastian Egner, Jens Axel Sogaard, 2004-2005
;; .author Alvaro Castro-Castilla, 2014

(define-library (spheres/algorithm compare)
  (export if3
          compare:if-rel?
          if=?
          if<?
          if>?
          if<=?
          if>=?
          if-not=?
          refine-compare
          select-compare
          cond-compare
          boolean-compare
          char-compare
          char-compare-ci
          string-compare
          string-compare-ci
          symbol-compare
          integer-compare
          rational-compare
          real-compare
          complex-compare
          number-compare
          vector-compare
          vector-compare-as-list
          list-compare
          list-compare-as-vector
          pair-compare-car
          pair-compare-cdr
          pair-compare
          default-compare
          =?
          <?
          >?
          <=?
          >=?
          not=?
          </<?
          </<=?
          <=/<?
          <=/<=?
          >/>?
          >/>=?
          >=/>?
          >=/>=?
          chain=?
          chain<?
          chain>?
          chain<=?
          chain>=?
          pairwise-not=?
          min-compare
          max-compare
          kth-largest
          compare-by<
          compare-by>
          compare-by<=
          compare-by>=
          compare-by=/<
          compare-by=/>
          debug-compare)
  (import (gambit)
          (spheres/core base))

  ;;! 3-sided conditional
  (define-syntax if3
    (syntax-rules ()
      ((if3 c less equal greater)
       (case c
         ((-1) less)
         (( 0) equal)
         (( 1) greater)
         (else (error "comparison value not in {-1,0,1}"))))))


  ;;! 2-sided conditionals for comparisons
  (define-syntax compare:if-rel?
    (syntax-rules ()
      ((compare:if-rel? c-cases a-cases c consequence)
       (compare:if-rel? c-cases a-cases c consequence (if #f #f)))
      ((compare:if-rel? c-cases a-cases c consequence alternate)
       (case c
         (c-cases consequence)
         (a-cases alternate)
         (else    (error "comparison value not in {-1,0,1}"))))))

  (define-syntax if=?
    (syntax-rules ()
      ((if=? arg ...)
       (compare:if-rel? (0) (-1 1) arg ...))))

  (define-syntax if<?
    (syntax-rules ()
      ((if<? arg ...)
       (compare:if-rel? (-1) (0 1) arg ...))))

  (define-syntax if>?
    (syntax-rules ()
      ((if>? arg ...)
       (compare:if-rel? (1) (-1 0) arg ...))))

  (define-syntax if<=?
    (syntax-rules ()
      ((if<=? arg ...)
       (compare:if-rel? (-1 0) (1) arg ...))))

  (define-syntax if>=?
    (syntax-rules ()
      ((if>=? arg ...)
       (compare:if-rel? (0 1) (-1) arg ...))))

  (define-syntax if-not=?
    (syntax-rules ()
      ((if-not=? arg ...)
       (compare:if-rel? (-1 1) (0) arg ...))))

  ;;! refine and extend construction
  (define-syntax refine-compare
    (syntax-rules ()
      ((refine-compare)
       0)
      ((refine-compare c1)
       c1)
      ((refine-compare c1 c2 cs ...)
       (if3 c1 -1 (refine-compare c2 cs ...) 1))))

  (define-syntax select-compare
    (syntax-rules (else)
      ((select-compare x y clause ...)
       (let ((x-val x) (y-val y))
         (select-compare (x-val y-val clause ...))))
                                        ; used internally: (select-compare (x y clause ...))
      ((select-compare (x y))
       0)
      ((select-compare (x y (else c ...)))
       (refine-compare c ...))
      ((select-compare (x y (t? c ...) clause ...))
       (let ((t?-val t?))
         (let ((tx (t?-val x)) (ty (t?-val y)))
           (if tx
               (if ty (refine-compare c ...) -1)
               (if ty 1 (select-compare (x y clause ...)))))))))

  (define-syntax cond-compare
    (syntax-rules (else)
      ((cond-compare)
       0)
      ((cond-compare (else cs ...))
       (refine-compare cs ...))
      ((cond-compare ((tx ty) cs ...) clause ...)
       (let ((tx-val tx) (ty-val ty))
         (if tx-val
             (if ty-val (refine-compare cs ...) -1)
             (if ty-val 1 (cond-compare clause ...)))))))

  (include "compare.scm"))
