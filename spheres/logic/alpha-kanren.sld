;;!!! alphaKanren: Nominal Logic Programming
;; Implementation of the paper alphaKanren: A Fresh Name in Nominal Logic Programming
;; .author William E. Byrd, 2014
;; .author Alvaro Castro-Castilla, 2015
;; .license MIT

(define-library (spheres/logic alpha-kanren)
  (export ==
          bind
          empty-sigma
          empty-nabla
          exist
          fresh
          hash
          mplus
          nom
          nom?
          pmatch
          take
          run
          reify
          stepso
          var)

  (define-syntax pmatch
    (syntax-rules (else guard)
      ((_ (op arg ...) cs ...)
       (let ((v (op arg ...)))
         (pmatch v cs ...)))
      ((_ v) (if #f #f))
      ((_ v (else e0 e ...)) (begin e0 e ...))
      ((_ v (pat (guard g ...) e0 e ...) cs ...)
       (let ((fk (lambda () (pmatch v cs ...))))
         (ppat v pat
               (if (and g ...) (begin e0 e ...) (fk))
               (fk))))
      ((_ v (pat e0 e ...) cs ...)
       (let ((fk (lambda () (pmatch v cs ...))))
         (ppat v pat (begin e0 e ...) (fk))))))

  (define-syntax ppat
    (syntax-rules (? quote unquote)
      ((_ v ? kt kf) kt)
      ((_ v () kt kf) (if (null? v) kt kf))
      ((_ v (quote lit) kt kf)
       (if (equal? v (quote lit)) kt kf))
      ((_ v (unquote var) kt kf) (let ((var v)) kt))
      ((_ v (x . y) kt kf)
       (if (pair? v)
           (let ((vx (car v)) (vy (cdr v)))
             (ppat vx x (ppat vy y kt kf) kf))
           kf))
      ((_ v lit kt kf) (if (equal? v (quote lit)) kt kf))))

  (define-syntax mv-let
    (syntax-rules ()
      ((_ ((x ...) e) b0 b ...)
       (pmatch e ((,x ...) b0 b ...)))))

  (define-syntax tie
    (syntax-rules ()
      ((_ a t)
       (begin
         (or (nom? a) (error 'tie "first argument is not a nom" a))
         `(tie-tag ,a ,t)))))

  (define-syntax choice
    (syntax-rules ()
      ((_ a f) (cons a f))))

  (define-syntax inc
    (syntax-rules ()
      ((_ e) (lambdaf@ () e))))

  (define-syntax case-inf
    (syntax-rules ()
      ((_ a-inf (() e0) ((f^) e1) ((a^) e2) ((a f) e3))
       (pmatch a-inf
               (#f e0)
               (,f^ (guard (procedure? f^)) e1)
               (,a^ (guard (not
                            (and (pair? a^)
                                 (procedure? (cdr a^)))))
                    e2)
               ((,a . ,f) e3)))))

  (define-syntax lambdag@
    (syntax-rules () ((_ (p) e) (lambda (p) e))))

  (define-syntax lambdaf@
    (syntax-rules () ((_ () e) (lambda () e))))

  (define-syntax run
    (syntax-rules ()
      ((_ n (x) g0 g ...)
       (take n (lambdaf@ ()
                         ((exist (x) g0 g ...
                                 (lambdag@ (p)
                                           (cons (reify x p) '())))
                          `(,empty-sigma ,empty-nabla)))))))

  (define-syntax run*
    (syntax-rules ()
      ((_ (x) g0 g ...)
       (run #f (x) g0 g ...))))

  (define-syntax exist
    (syntax-rules ()
      ((_ (x ...) g0 g ...)
       (lambdag@ (p)
                 (inc
                  (let ((x (var)) ...)
                    (bind* (g0 p) g ...)))))))

  (define-syntax fresh
    (syntax-rules ()
      ((_ (a ...) g0 g ...)
       (lambdag@ (p)
                 (inc
                  (let ((a (nom 'a)) ...)
                    (bind* (g0 p) g ...)))))))

  (define-syntax bind*
    (syntax-rules ()
      ((_ e) e)
      ((_ e g0 g ...)
       (let ((a-inf e))
         (and a-inf (bind* (bind a-inf g0) g ...))))))

  (define-syntax conde
    (syntax-rules ()
      ((_ (g0 g ...) (g1 g^ ...) ...)
       (lambdag@ (p)
                 (inc
                  (mplus* (bind* (g0 p) g ...)
                          (bind* (g1 p) g^ ...)
                          ...))))))

  (define-syntax mplus*
    (syntax-rules ()
      ((_ e) e)
      ((_ e0 e ...) (mplus e0 (lambdaf@ () (mplus* e ...))))))

  (define-syntax conda
    (syntax-rules ()
      ((_ (g0 g ...) (g1 g^ ...) ...)
       (lambdag@ (p)
                 (inc (ifa ((g0 p) g ...)
                           ((g1 p) g^ ...) ...))))))

  (define-syntax ifa
    (syntax-rules ()
      ((_) #f)
      ((_ (e g ...) b ...)
       (let loop ((a-inf e))
         (case-inf a-inf
                   (() (ifa b ...))
                   ((f) (inc (loop (f))))
                   ((a) (bind* a-inf g ...))
                   ((a f) (bind* a-inf g ...)))))))

  (define-syntax condu
    (syntax-rules ()
      ((_ (g0 g ...) (g1 g^ ...) ...)
       (lambdag@ (p)
                 (inc (ifu ((g0 p) g ...)
                           ((g1 p) g^ ...) ...))))))

  (define-syntax ifu
    (syntax-rules ()
      ((_) #f)
      ((_ (e g ...) b ...)
       (let loop ((a-inf e))
         (case-inf a-inf
                   (() (ifu b ...))
                   ((f) (inc (loop (f))))
                   ((a) (bind* a-inf g ...))
                   ((a f) (bind* a g ...)))))))

  (define-syntax project
    (syntax-rules ()
      ((_ (x ...) g0 g ...)
       (lambdag@ (p)
                 (mv-let ((sigma nabla) p)
                         (let ((x (apply-subst sigma x)) ...)
                           (bind* (g0 p) g ...)))))))

  (include "alpha-kanren.scm"))
