; <PLAINTEXT>
; Eager Comprehensions in [outer..inner|expr]-Convention
; ======================================================
;
; sebastian.egner@philips.com, Eindhoven, The Netherlands, 26-Dec-2007
; Scheme R5RS (incl. macros), SRFI-23 (error).
; 
; Loading the implementation into Scheme48 0.57:
;   ,open srfi-23
;   ,load ec.scm
;
; Loading the implementation into PLT/DrScheme 317:
;   ; File > Open ... "ec.scm", click Execute
;
; Loading the implementation into SCM 5d7:
;   (require 'macro) (require 'record) 
;   (load "ec.scm")
;
; Implementation comments:
;   * All local (not exported) identifiers are named ec-<something>.
;   * This implementation focuses on portability, performance, 
;     readability, and simplicity roughly in this order. Design
;     decisions related to performance are taken for Scheme48.
;   * Alternative implementations, Comments and Warnings are 
;     mentioned after the definition with a heading.


; ==========================================================================
; The fundamental comprehension do-ec
; ==========================================================================
;
; All eager comprehensions are reduced into do-ec and
; all generators are reduced to :do. 
;
; We use the following short names for syntactic variables
;   q    - qualifier
;   cc   - current continuation, thing to call at the end;
;          the CPS is (m (cc ...) arg ...) -> (cc ... expr ...)
;   cmd  - an expression being evaluated for its side-effects
;   expr - an expression
;   gen  - a generator of an eager comprehension
;   ob   - outer binding
;   oc   - outer command
;   lb   - loop binding
;   ne1? - not-end1? (before the payload)
;   ib   - inner binding
;   ic   - inner command
;   ne2? - not-end2? (after the payload)
;   ls   - loop step
;   etc  - more arguments of mixed type

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))


(define (dispatch-union d1 d2)
  (lambda (args)
    (let ((g1 (d1 args)) (g2 (d2 args)))
      (if g1
          (if g2 
              (if (null? args)
                  (append (if (list? g1) g1 (list g1)) 
                          (if (list? g2) g2 (list g2)))
                  (error "dispatching conflict" args (d1 '()) (d2 '())))
              g1 )
          (if g2 g2 #f)))))


;;-------------------------------------------------------------------------------
;;! The dispatching generator

(define (make-initial-:-dispatch)
  (lambda (args)
    (case (length args)
      ((0) 'SRFI42)
      ((1) (let ((a1 (car args)))
             (cond
              ((list? a1)
               (:generator-proc (:list a1)))
              ((string? a1)
               (:generator-proc (:string a1)))
              ((vector? a1)
               (:generator-proc (:vector a1)))
              ((and (integer? a1) (exact? a1))
               (:generator-proc (:range a1)))
              ((real? a1)
               (:generator-proc (:real-range a1)))
              ((input-port? a1)
               (:generator-proc (:port a1)))
              (else
               #f ))))
      ((2) (let ((a1 (car args)) (a2 (cadr args)))
             (cond
              ((and (list? a1) (list? a2))
               (:generator-proc (:list a1 a2)))
              ((and (string? a1) (string? a1))
               (:generator-proc (:string a1 a2)))
              ((and (vector? a1) (vector? a2))
               (:generator-proc (:vector a1 a2)))
              ((and (integer? a1) (exact? a1) (integer? a2) (exact? a2))
               (:generator-proc (:range a1 a2)))
              ((and (real? a1) (real? a2))
               (:generator-proc (:real-range a1 a2)))
              ((and (char? a1) (char? a2))
               (:generator-proc (:char-range a1 a2)))
              ((and (input-port? a1) (procedure? a2))
               (:generator-proc (:port a1 a2)))
              (else
               #f ))))
      ((3) (let ((a1 (car args)) (a2 (cadr args)) (a3 (caddr args)))
             (cond
              ((and (list? a1) (list? a2) (list? a3))
               (:generator-proc (:list a1 a2 a3)))
              ((and (string? a1) (string? a1) (string? a3))
               (:generator-proc (:string a1 a2 a3)))
              ((and (vector? a1) (vector? a2) (vector? a3))
               (:generator-proc (:vector a1 a2 a3)))
              ((and (integer? a1) (exact? a1) 
                    (integer? a2) (exact? a2)
                    (integer? a3) (exact? a3))
               (:generator-proc (:range a1 a2 a3)))
              ((and (real? a1) (real? a2) (real? a3))
               (:generator-proc (:real-range a1 a2 a3)))
              (else
               #f ))))
      (else
       (letrec ((every? 
                 (lambda (pred args)
                   (if (null? args)
                       #t
                       (and (pred (car args))
                            (every? pred (cdr args)))))))
         (cond
          ((every? list? args)
           (:generator-proc (:list (apply append args))))
          ((every? string? args)
           (:generator-proc (:string (apply string-append args))))
          ((every? vector? args)
           (:generator-proc (:list (apply append (map vector->list args)))))
          (else
           #f)))))))

(define :-dispatch
  (make-initial-:-dispatch))

(define (:-dispatch-ref)
  :-dispatch)

(define (:-dispatch-set! dispatch)
  (if (not (procedure? dispatch))
      (error "not a procedure" dispatch))
  (set! :-dispatch dispatch))

