;;!!! Mini-Kanren
;; .author William E. Byrd, 2015
;; .author Alvaro Castro-Castilla, 2015
;; .version https://github.com/webyrd/miniKanren-with-symbolic-constraints/commit/a628cb76be1f1028212e884dcd850c9e6cf24df9
;; .license MIT

(define-library (spheres/logic kanren)
  (export empty-c ;; Exported for syntax expansion, shouldn't be used directly
          var
          empty-f
          bind
          take
          mplus
          c->B
          c->E
          c->S
          c->D
          c->Y
          c->N
          c->T
          choice
          succeed
          fail
          reify
          ;; Proper exports
          conde
          conda
          condu
          absento
          symbolo
          numbero
          project
          =/=
          ==
          run
          run*
          fresh
          eigen)
  (import (spheres/algorithm sort-merge))

  (define-syntax inc
    (syntax-rules ()
      ((_ e) (lambdaf@ () e))))

  (define-syntax lambdaf@
    (syntax-rules ()
      ((_ () e) (lambda () e))))

  (define-syntax lambdag@
    (syntax-rules (:)
      ((_ (c) e) (lambda (c) e))
      ((_ (c : B E S) e)
       (lambda (c)
         (let ((B (c->B c)) (E (c->E c)) (S (c->S c)))
           e)))
      ((_ (c : B E S D Y N T) e)
       (lambda (c)
         (let ((B (c->B c)) (E (c->E c)) (S (c->S c)) (D (c->D c))
               (Y (c->Y c)) (N (c->N c)) (T (c->T c)))
           e)))))

  (define-syntax run
    (syntax-rules ()
      ((_ n (q) g0 g ...)
       (take n
             (lambdaf@ ()
                       ((fresh (q) g0 g ...
                               (lambdag@ (final-c)
                                         (let ((z ((reify q) final-c)))
                                           (choice z empty-f))))
                        empty-c))))
      ((_ n (q0 q1 q ...) g0 g ...)
       (run n (x) (fresh (q0 q1 q ...) g0 g ... (== `(,q0 ,q1 ,q ...) x))))))

  (define-syntax run*
    (syntax-rules ()
      ((_ (q0 q ...) g0 g ...) (run #f (q0 q ...) g0 g ...))))

  (define-syntax bind*
    (syntax-rules ()
      ((_ e) e)
      ((_ e g0 g ...) (bind* (bind e g0) g ...))))

  (define-syntax mplus*
    (syntax-rules ()
      ((_ e) e)
      ((_ e0 e ...) (mplus e0
                           (lambdaf@ () (mplus* e ...))))))

  (define-syntax fresh
    (syntax-rules ()
      ((_ (x ...) g0 g ...)
       (lambdag@ (c : B E S D Y N T)
                 (inc
                  (let ((x (var 'x)) ...)
                    (let ((B (append `(,x ...) B)))
                      (bind* (g0 `(,B ,E ,S ,D ,Y ,N ,T)) g ...))))))))

  (define-syntax eigen
    (syntax-rules ()
      ((_ (x ...) g0 g ...)
       (lambdag@ (c : B E S)
                 (let ((x (eigen-var)) ...)
                   ((fresh () (eigen-absento `(,x ...) B) g0 g ...) c))))))

  (define-syntax project
    (syntax-rules ()
      ((_ (x ...) g g* ...)
       (lambdag@ (c : B E S)
                 (let ((x (walk* x S)) ...)
                   ((fresh () g g* ...) c))))))

  (define-syntax conde
    (syntax-rules ()
      ((_ (g0 g ...) (g1 g^ ...) ...)
       (lambdag@ (c)
                 (inc
                  (mplus*
                   (bind* (g0 c) g ...)
                   (bind* (g1 c) g^ ...) ...))))))

  (define-syntax case-inf
    (syntax-rules ()
      ((_ e (() e0) ((f^) e1) ((c^) e2) ((c f) e3))
       (let ((c-inf e))
         (cond
          ((not c-inf) e0)
          ((procedure? c-inf)  (let ((f^ c-inf)) e1))
          ((not (and (pair? c-inf)
                     (procedure? (cdr c-inf))))
           (let ((c^ c-inf)) e2))
          (else (let ((c (car c-inf)) (f (cdr c-inf)))
                  e3)))))))

  (define-syntax ifa
    (syntax-rules ()
      ((_) (mzero))
      ((_ (e g ...) b ...)
       (let loop ((c-inf e))
         (case-inf c-inf
                   (() (ifa b ...))
                   ((f) (inc (loop (f))))
                   ((a) (bind* c-inf g ...))
                   ((a f) (bind* c-inf g ...)))))))

  (define-syntax conda
    (syntax-rules ()
      ((_ (g0 g ...) (g1 g^ ...) ...)
       (lambdag@ (c)
                 (inc
                  (ifa ((g0 c) g ...)
                       ((g1 c) g^ ...) ...))))))
  (define-syntax ifu
    (syntax-rules ()
      ((_) (mzero))
      ((_ (e g ...) b ...)
       (let loop ((c-inf e))
         (case-inf c-inf
                   (() (ifu b ...))
                   ((f) (inc (loop (f))))
                   ((c) (bind* c-inf g ...))
                   ((c f) (bind* (unit c) g ...)))))))

  (define-syntax condu
    (syntax-rules ()
      ((_ (g0 g ...) (g1 g^ ...) ...)
       (lambdag@ (c)
                 (inc
                  (ifu ((g0 c) g ...)
                       ((g1 c) g^ ...) ...))))))

  (include "kanren.scm"))
