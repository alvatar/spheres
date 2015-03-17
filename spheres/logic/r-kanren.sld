;;!!! Guided Search in miniKanren
;; .author Cameron Swords, 2013
;; .author Daniel P. Friedman, 2013

(define-library (spheres/logic r-kanren)
  (export run ;; TODO: find correct exports
          run*)
  (import (spheres/algorithm sort-merge))

  ;;(define-syntax lambdaf@
  ;;  (syntax-rules ()
  ;;    ((_ () e) (lambda () e))))

  (define-syntax lambdaf@
    (syntax-rules ()
      ((_ () r e) (make-stream r (lambda () e)))))

  ;;(define-syntax thunk
  ;;  (syntax-rules () ((_ e) (lambdaf@ () e))))

  (define-syntax lambda-goal-subst
    (syntax-rules ()
      ((_ (p) e) (lambda (p) e))
      ((_ (p : r s c*) e)
       (lambda (p)
         (let ((r (subst-rank p))
               (s (subst-alist p))
               (c* (subst-diseq p))
               )
           e)))))

  (define-syntax var                    ;: a -> var
    (syntax-rules ()
      ((_ x) (vector x))))

  (define-syntax var?                   ;: a -> bool
    (syntax-rules ()
      ((_ x) (vector? x))))

  (define-syntax thunk
    (syntax-rules () ((_ r e) (make-stream r (lambda () e)))))

  (define-syntax size-s                 ;: subst -> int
    (syntax-rules ()
      ((_ x) (length (subst-alist  x)))))

  (define-syntax mzero
    (syntax-rules () ((_) #f)))

  (define-syntax case-inf
    (syntax-rules ()
      ((_ e (() no-ans)
          ((stream^) more-stream)
          ((ans^) one-ans)
          ((ans stream) ans-and-stream))
       (let ([a-inf e])
         (cond
          [(not a-inf) (incr-rank no-ans)]
          [(stream? a-inf)
           (let ([stream^ a-inf]) (incr-rank more-stream))]
          [(not (and (pair? a-inf) (stream? (cdr a-inf))))
           (let ([ans^ a-inf]) (incr-rank one-ans))]
          [else
           (let ([ans (car a-inf)]
                 [stream (cdr a-inf)])
             (incr-rank ans-and-stream))])))))

  (define-syntax choice
    (syntax-rules () ((_ a f) (cons a f))))

  (define-syntax mplus*
    (syntax-rules ()
      ((_ e) e)
      ((_ e0 e ...)
       (let ([min-rank (apply-min (map get-rank `(,e ...)))]
             [e0-rank (get-rank e0)])
         (if (< e0-rank min-rank)
             (mplus e0 (lambdaf@ () min-rank (mplus* e ...)))
             (mplus (lambdaf@ () min-rank (mplus* e ...)) (lambdaf@ () e0-rank e0)))))))

  (define-syntax unit
    (syntax-rules () ((_ a) a)))

  (define-syntax bind*
    (syntax-rules ()
      ((_ e) e)
      ((_ e g0 g ...) (bind* (bind e g0) g ...))))

  (define-syntax run
    (syntax-rules ()
      ((_ n (x) g0 g ...)
       (take n
             (lambdaf@ () 0
                       ((fresh (x) g0 g ...
                               (lambda-goal-subst (a)
                                                  (choice ((reify x) a) empty-f)))
                        empty-a))))))

  (define-syntax run*
    (syntax-rules ()
      ((_ (x) g ...) (run #f (x) g ...))))

  (define-syntax fresh
    (syntax-rules ()
      ((_ (x ...) g0 g ...)
       (lambda-goal-subst (a : r s c*)
                          (thunk r
                                 (let ((x (var 'x)) ...)
                                   (bind* (g0 a) g ...)))))))

  (define-syntax conde
    (syntax-rules ()
      ((_ (g0 g ...) (g1 g^ ...) ...)
       (lambda-goal-subst (a : r s c*)
                          (thunk r
                                 (let ((a (subst-incr-rank a)))
                                   (mplus*
                                    (bind* (g0 a) g ...)
                                    (bind* (g1 a) g^ ...) ...)))))))

  (define-syntax condr
    (syntax-rules ()
      ((_ (p0 g0 g ...) (p1 g1 g^ ...) ...)
       (lambda-goal-subst (a : r s c*)
                          (thunk r
                                 (let ((a (subst-incr-rank a)))
                                   (mplus*
                                    (bind* (g0 (subst-add-rank a p0)) g ...)
                                    (bind* (g1 (subst-add-rank a p1)) g^ ...) ...)))))))


  (define-syntax conda
    (syntax-rules ()
      ((_ (g0 g ...) (g1 g^ ...) ...)
       (lambda-goal-subst (a : r s c*)
                          (thunk r
                                 (let ((s (subst-incr-rank a)))
                                   (ifa ((g0 a) g ...)
                                        ((g1 a) g^ ...) ...)))))))

  (define-syntax ifa
    (syntax-rules ()
      ((_) (mzero))
      ((_ (e g ...) b ...)
       (let loop ((a-inf e))
         (case-inf a-inf
                   (() (ifa b ...))
                   ((f) (thunk (+ 1 (stream-rank f)) (loop (invoke f))))
                   ((a) (bind* a-inf g ...))
                   ((a f) (bind* a-inf g ...)))))))

  (define-syntax condu
    (syntax-rules ()
      ((_ (g0 g ...) (g1 g^ ...) ...)
       (lambda-goal-subst (a : r s c*)
                          (thunk r
                                 (let ((a (subst-incr-rank a)))
                                   (ifu ((g0 a) g ...)
                                        ((g1 a) g^ ...) ...)))))))

  (define-syntax ifu
    (syntax-rules ()
      ((_) (mzero))
      ((_ (e g ...) b ...)
       (let loop ((a-inf e))
         (case-inf a-inf
                   (() (ifu b ...))
                   ((f) (thunk (+ 1 (stream-rank f)) (loop (invoke f))))
                   ((a) (bind* a-inf g ...))
                   ((a f) (bind* (unit a) g ...)))))))

  (define-syntax project
    (syntax-rules ()
      ((_ (x ...) g g* ...)
       (lambda-goal-subst (a)
                          (let ((x (walk* x a)) ...)
                            ((fresh () g g* ...) a))))))



  (include "r-kanren.scm"))
