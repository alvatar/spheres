
(include "../../sxml-tools/tests/xtest-harness.sch")

;; $Id: xtest-harness.scm,v 1.2 2005/01/28 09:16:57 lizorkin Exp $
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin


;==============================================================================
; Auxiliary

; A counterpart to list= from SRFI optimized for two lists
(define (xtest-list= = list-a list-b)
  (let rpt ((a list-a) (b list-b))
    (if (null? a)
      (null? b)
      (and (not (null? b))
	   (= (car a) (car b))
	   (rpt (cdr a) (cdr b))))))

; For use in expected results: will be considered as equal to any lambda
(define x-lambda-placeholder (lambda() #t))

; Like equal? but yields #t for comparison of any two procedures.
(define (xtest-equal? x y)
  (cond
    ((and (procedure? x)
	  (procedure? y)))
    ((not (and
	    (list? x)
	    (list? y)))
     (equal? x y))
    (else 
      (xtest-list=
	(lambda (x y) 
	  (xtest-equal? x y))
	x y)
      )))


;==============================================================================
; Test assertions verificators

(define xtest-sep-line "  ==============  ")


;==========================================================================
; Diff

; Returns '() if objects are equal
; Differences found:
; (listof '(diff (sxpath ...) (comment ...) (first ...) (second ...))
(define (xtest:diff obj1 obj2)
  (letrec
      ((form-diff
        ; Forms a difference element
        (lambda (node1 node2 lpath comment)
          `(diff
            (sxpath ,(reverse lpath))
            (comment ,comment)
            (result ,node1)
            (expected ,node2))))
       (tree-walk
        ; lpath - reverse location path to current nodes
        ; Returns diffs found or '()
        (lambda (node1 node2 lpath)
          (cond
            ((or (not (pair? node1))
                 (not (pair? node2)))  ; either node is atomic
             (if (equal? node1 node2)
                 '()
                 (list
                  (form-diff node1 node2 lpath "different atoms"))))            
            ((not (equal? (car node1) (car node2)))  ; different element names
             (list
              (form-diff node1 node2 lpath "different operations")))
            ((not (= (length node1)
                     (length node2)))  ; different number of arguments
             (list
              (form-diff node1 node2 lpath "different number of arguments")))
            (else  ; recursive
             (apply
              append
              (map
               (lambda (kid1 kid2)
                 (tree-walk kid1 kid2 (cons (car node1) lpath)))
               (cdr node1) (cdr node2))))))))
    (tree-walk obj1 obj2 '())))
