(define (remp p l)
  (if (null? l)
      '()
      (if (p (car l))
          (remp p (cdr l))
          (cons (car l) (remp p (cdr l))))))



(define-structure stream rank proc)

(define rhs (lambda (x) (cdr x)))

(define lhs (lambda (x) (car x)))


;;------------------------------------------------------------------------------
;;!! Thunk Definitions for Streams

(define stream-incr-rank
  (lambda (stream)
    (make-stream (+ 1 (stream-rank stream)) (stream-proc stream))))

(define invoke
  (lambda (stream)
    ;; (printf "Thunk: ~s~n" (stream-rank stream))
    ((stream-proc stream))))

;; If we are getting the rank of a stream, return that.
;; Otherwise, return -1 because we have failed and want to do it NOW.
(define get-rank
  (lambda (a-inf)
    (cond
     [(stream? a-inf) (stream-rank a-inf)]
     [(subst? a-inf)  (subst-rank a-inf)]
     [else -1])))

;; (define debug-var #f)

;; (define debug-printf
;;   (lambda ls
;;     (if debug-var (apply printf ls) (void))))


;;------------------------------------------------------------------------------
;;!! Substitution Definitions

(define-structure subst rank alist diseq)

(define empty-a (make-subst 0 '() '())) ;: subst

(define lookup-s ;: var -> subst -> (var, val)
  (lambda (u S)
    (assq u (subst-alist S))))

(define subst-incr-rank ;: subst -> subst
  (lambda (s)
    (make-subst (+ 1 (subst-rank s)) (subst-alist s) (subst-diseq s))))

(define subst-add-rank ;: subst -> subst
  (lambda (s r)
    (make-subst (+ (subst-rank s) r) (subst-alist s) (subst-diseq s))))

(define ext-s ;: var -> val -> subst -> subst
  (lambda (x v s)
    (make-subst (subst-rank s) (cons `(,x . ,v) (subst-alist s)) (subst-diseq s))))

(define ext-s-check ;: var -> val -> subst -> (Either subst #f)
  (lambda (x v s)
    (cond
     ((occurs-check x v s) #f)
     (else (ext-s x v s)))))

(define occurs-check ;: var -> var-> subst -> bool
  (lambda (x y s)
    (let ((y (walk y s)))
      (cond
       ((var? y) (eq? y x))
       ((pair? y)
        (or
         (occurs-check x (car y) s)
         (occurs-check x (cdr y) s)))
       (else #f)))))

(define incr-rank
  (lambda (a-inf)
    (cond
     [(stream? a-inf) (stream-incr-rank a-inf)]
     [(subst? a-inf)  (subst-incr-rank a-inf)]
     [(pair? a-inf)   (cons (incr-rank (car a-inf)) (incr-rank (cdr a-inf)))]
     [else a-inf])))

(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
              (() (invoke f))
              ((f^) (let ([f^-rank (get-rank f^)]
                          [f-rank  (get-rank f)])
                      (if (< f^-rank f-rank)
                          (thunk f^-rank (mplus (invoke f^) f))
                          (thunk f-rank  (mplus (invoke f)  f^)))))
              ((a) (choice a f))
              ((a f^) (choice a
                              (let ([f^-rank (get-rank f^)]
                                    [f-rank  (get-rank f)])
                                (if (< f^-rank f-rank)
                                    (lambdaf@ () f^-rank (mplus (invoke f^) f))
                                    (lambdaf@ () f-rank  (mplus (invoke f)  f^)))))))))

(define apply-min
  (lambda (ls)
    (apply min ls)))

(define bind
  (lambda (a-inf g)
    (case-inf a-inf
              (() (mzero))
              ((f) (thunk (get-rank f) (bind (invoke f) g)))
              ((a) (g a))
              ((a f) (mplus (g a)
                            (lambdaf@ () (get-rank f) (bind (invoke f) g)))))))


;;------------------------------------------------------------------------------
;;!! miniKanren-isms: run, run*, fresh, ==, and take (which run uses)

(define empty-f)
(set! empty-f  (lambdaf@ () 0 (mzero)))

(define =/=
  (lambda (u v)
    (lambda-goal-subst (a : r s c*)
                       (cond
                        ((unify u v a) =>
                         (lambda (a0)
                           (let ((s0 (subst-alist a0)))
                             (cond
                              ((eq? s0 s) (mzero))
                              (else
                               (let ((p* (list (prefix-s s0 a))))
                                 (let ((c* (append p* c*)))
                                   (unit (make-subst r s c*)))))))))
                        (else (unit a))))))

(define ==
  (lambda (u v)
    (lambda-goal-subst (a : r s c*)
                       (cond
                        ((unify u v a) =>
                         (lambda (a0)
                           (let ((s0 (subst-alist a0)))
                             (cond
                              ((eq? s0 s) (unit a))
                              ((verify-c* c* (make-subst r s0 c*)) =>
                               (lambda (c*)
                                 (unit (make-subst r s0 c*))))
                              (else (mzero))))))
                        (else (mzero))))))

(define verify-c*
  (lambda (c* s)
    (cond
     ((null? c*) '())
     ((verify-c* (cdr c*) s) =>
      (lambda (c0*)
        (let ((c (car c*)))
          (cond
           ((verify-c*-aux c c0* s))
           (else (mzero))))))
     (else #f))))

(define verify-c*-aux
  (lambda (c c0* a)
    (cond
     ((unify* c a) =>
      (lambda (a0)
        (let ((s0 (subst-alist a0)))
          (and (not (eq? s0 (subst-alist a)))
               (cons (prefix-s s0 a) c0*)))))
     (else c0*))))

(define prefix-s
  (lambda (s0 a)
    (cond
     ((eq? s0 (subst-alist a)) '())
     (else (cons (car s0)
                 (prefix-s (cdr s0) a))))))

(define unify*
  (lambda (c s)
    (unify (map lhs c) (map rhs c) s)))

(define take
  (lambda (n f)
    (if (and n (zero? n))
        '()
        (case-inf (invoke f)
                  (() '())
                  ((f) (take n f))
                  ((a) (cons a '()))
                  ((a f)
                   (cons a
                         (take (and n (- n 1)) f)))))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define onceo
  (lambda (g)
    (condu
     (g succeed)
     ((== #f #f) fail))))


;;------------------------------------------------------------------------------
;;!! Walking and Reification Code

(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
       ((eq? u v) s)
       ((var? u) (ext-s-check u v s))
       ((var? v) (ext-s-check v u s))
       ((and (pair? u) (pair? v))
        (let ((s (unify
                  (car u) (car v) s)))
          (and s (unify
                  (cdr u) (cdr v) s))))
       ((equal? u v) s)
       (else #f)))))

(define walk
  (lambda (u S)
    (cond
     ((and (var? u) (lookup-s u S)) =>
      (lambda (pr) (walk (rhs pr) S)))
     (else u))))

(define walk*
  (lambda (w s)
    (let ((v (walk w s)))
      (cond
       ((var? v) v)
       ((pair? v)
        (cons
         (walk* (car v) s)
         (walk* (cdr v) s)))
       (else v)))))

;; (define reify-s
;;   (lambda (v s)
;;     (let ((v (walk v s)))
;;       (cond
;;         ((var? v)
;;          (ext-s v (reify-name (gensym)) s))
;;         ((pair? v) (reify-s (cdr v)
;;                      (reify-s (car v) s)))
;;         (else s)))))
;;
;; (define reify-name
;;   (lambda (n) n))

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
       ((var? v)
        (ext-s v (reify-name (size-s s)) s))
       ((pair? v) (reify-s (cdr v)
                           (reify-s (car v) s)))
       (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
     (string-append "_" "." (number->string n)))))

(define reify
  (lambda (v)
    (lambda-goal-subst (a : r s c*)
                       (let ((v (walk* v a)))
                         (let ((r (reify-s v empty-a)))
                           (let ((c* (remp
                                      (lambda (c)
                                        (anyvar? c r))
                                      c*)))
                             (reify-aux r v (rem-subsumed c*))))))))

(define anyvar?
  (lambda (c r)
    (cond
     ((pair? c)
      (or (anyvar? (car c) r)
          (anyvar? (cdr c) r)))
     (else (and (var? c) (var? (walk c r)))))))

(define rem-subsumed
  (lambda (c*)
    (let rem-subsumed ((c* c*) (c^* '()))
      (cond
       ((null? c*) c^*)
       ((let ((car-c*-subst (make-subst 0 (car c*) '())))
          (or (subsumed? car-c*-subst (cdr c*))
              (subsumed? car-c*-subst c^*)))
        (rem-subsumed (cdr c*) c^*))
       (else (rem-subsumed (cdr c*)
                           (cons (car c*) c^*)))))))

(define subsumed?
  (lambda (c c*)
    (cond
     ((null? c*) #f)
     (else
      (let ((c^ (unify* (car c*) c)))
        (or
         (and c^ (eq? (subst-alist c^)
                      (subst-alist c)))
         (subsumed? c (cdr c*))))))))

(define reify-aux
  (lambda (r v c*)
    (let ((v (walk* v r))
          (c* (walk* c* r)))
      (let ((c* (drop-dot-D (sort-D c*))))
        (cond
         ((null? c*) v)
         (else `(,v (=/= . ,c*) . ())))))))

(define sort-D
  (lambda (D)
    (sort (map sort-d D) lex<=?)))

(define sort-d
  (lambda (d)
    (sort
     (map sort-pr d)
     (lambda (x y)
       (lex<=? (car x) (car y))))))

(define lex<=?
  (lambda (x y)
    (cond
     ((vector? x) #t)
     ((vector? y) #f)
     ((port? x) #t)
     ((port? y) #f)
     ((procedure? x) #t)
     ((procedure? y) #f)
     ((boolean? x)
      (cond
       ((boolean? y) (or (not x) (eq? x y)))
       (else #t)))
     ((boolean? y) #f)
     ((null? x) #t)
     ((null? y) #f)
     ((char? x)
      (cond
       ((char? y) (char<=? x y))
       (else #t)))
     ((char? y) #f)
     ((number? x)
      (cond
       ((number? y) (<= x y))
       (else #t)))
     ((number? y) #f)
     ((string? x)
      (cond
       ((string? y) (string<=? x y))
       (else #t)))
     ((string? y) #f)
     ((symbol? x)
      (cond
       ((symbol? y)
        (string<=? (symbol->string x)
                   (symbol->string y)))
       (else #t)))
     ((symbol? y) #f)
     ((pair? x)
      (cond
       ((pair? y)
        (cond
         ((equal? (car x) (car y))
          (lex<=? (cdr x) (cdr y)))
         (else (lex<=? (car x) (car y)))))))
     ((pair? y) #f)
     (else #t))))

(define sort-pr
  (lambda (pr)
    (let ((l (lhs pr))
          (r (rhs pr)))
      (cond
       ((lex<-reified-name? r) pr)
       ((lex<=? r l) `(,r . ,l))
       (else pr)))))

(define lex<-reified-name?
  (lambda (r)
    (char<?
     (string-ref (datum->string r) 0)
     (string-ref "_" 0))))

(define datum->string
  (lambda (x)
    (call-with-output-string
     '()
     (lambda (p) (display x p)))))

(define drop-dot-D
  (lambda (D)
    (map
     (lambda (X)
       (map (lambda (t)
              (let ((a (lhs t))
                    (d (rhs t)))
                `(,a ,d)))
            X))
     D)))

;; (trace bind)
;; (trace mplus)

;; (define test
;;   (lambda (e n)
;;     (fresh (a b)
;;            (condr
;;             ((if (< n 1) 10 1) (== e '(x)))
;;             (2 (== e `(b . ,a)) (test a (+ 1 n)))
;;             (4 (== e `(a . ,b)) (test b (+ 1 n)))))))

;; (load "==-tests.ss")
;; (load "mktests.scm")
;; (load "disequality-tests.ss")


;;-------------------------------------------------------------------------------
;;!! ex.scm

;; (define gen-expo
;;   (lambda (out)
;;     (fresh (x e1 e2 e3 body)
;;            (condr
;;             (10 (== x out))
;;             (2 (== out `(lambda (,x) ,body))
;;                (gen-expo body))
;;             (4 (== out `(if ,e1 ,e2 ,e3))
;;                (gen-expo e1)
;;                (gen-expo e2)
;;                (gen-expo e3))
;;             (1 (== out `(,e1 ,e2)))))))


;;-------------------------------------------------------------------------------
;;!! inf.scm

;; (define !-r
;;   (lambda (gamma e t n)
;;     (let ([n (add1 n)])
;;       (fresh (e1 e2 e3 t1 t2)
;;              (condr
;;               [(if (< n 5) 30 1)
;;                (== e `(intc ,e1))
;;                (== t 'int)]
;;               [(if (< n 5) 30 2)
;;                (== e `(+ ,e1 ,e2))
;;                (== t 'int)
;;                (!-r gamma e1 'int n)
;;                (!-r gamma e2 'int n)]
;;               [(if (< n 5) 30 1)
;;                (== e `(var ,e1))
;;                (lookupo gamma e1 t)]
;;               [4
;;                (== e `(lambda (,e1) ,e2))
;;                (== t `(-> ,t1 ,t2))
;;                (!-r `((,e1 . ,t1) . ,gamma) e2 t2 n)]
;;               [2
;;                (== e `(app ,e1 ,e2))
;;                (!-r gamma e1 `(-> ,t1 ,t) n)
;;                (!-r gamma e2 t1 n)])))))

;; (define !-o
;;   (lambda (gamma e t)
;;     (fresh (e1 e2 e3 t1 t2)
;;            (conde
;;             [(== e `(intc ,e1))
;;              (== t 'int) ]
;;             [(== e `(+ ,e1 ,e2))
;;              (== t 'int)
;;              (!-o gamma e1 'int)
;;              (!-o gamma e2 'int)]
;;             [(== e `(var ,e1))
;;              (lookupo gamma e1 t)]
;;             [(== e `(lambda (,e1) ,e2))
;;              (== t `(-> ,t1 ,t2))
;;              (!-o `((,e1 . ,t1) . ,gamma) e2 t2)]
;;             [(== e `(app ,e1 ,e2))
;;              (!-o gamma e1 `(-> ,t1 ,t))
;;              (!-o gamma e2 t1)]))))

;; (define lookupo
;;   (lambda (G x t)
;;     (fresh (rest type y)
;;            (conde
;;             ((== `((,x . ,t) . ,rest) G))
;;             ((== `((,y . ,type) . ,rest) G)
;;              (=/= x y)
;;              (lookupo rest x t))))))


;;-------------------------------------------------------------------------------
;;!! infero.scm

;; (define !-r
;;   (lambda (gamma e t n)
;;     (let ([n (add1 n)])
;;       (fresh (e1 e2 e3 t1 t2)
;;              (condr
;;               [(if (< n 3) 30 1)
;;                (== e `(intc ,e1))
;;                (== t 'int)]
;;               [(if (< n 3) 30 1)
;;                (== e `(boolc ,e1))
;;                (== t 'bool)]
;;               [(if (< n 3) 30 1)
;;                (== e `(zero? ,e1))
;;                (== t 'bool)
;;                (!-r gamma e1 'int n)]
;;               [(if (< n 3) 30 1)
;;                (== e `(* ,e1 ,e2))
;;                (== t 'int)
;;                (!-r gamma e1 'int n)
;;                (!-r gamma e2 'int n)]
;;               [(if (< n 3) 30 1)
;;                (== e `(+ ,e1 ,e2))
;;                (== t 'int)
;;                (!-r gamma e1 'int n)
;;                (!-r gamma e2 'int n)]
;;               [(if (< n 3) 30 1)
;;                (== e `(if ,e1 ,e2 ,e3))
;;                (!-r gamma e1 'bool n)
;;                (!-r gamma e2 t n)
;;                (!-r gamma e3 t n)]
;;               [(if (< n 4) 30 15)
;;                (== e `(var ,e1))
;;                (lookupo gamma e1 t)]
;;               [4
;;                (== e `(lambda (,e1) ,e2))
;;                (== t `(-> ,t1 ,t2))
;;                (!-r `((,e1 . ,t1) . ,gamma) e2 t2 n)]
;;               [2
;;                (== e `(app ,e1 ,e2))
;;                (!-r gamma e1 `(-> ,t1 ,t) n)
;;                (!-r gamma e2 t1 n)])))))

;; (define !-o
;;   (lambda (gamma e t)
;;     (fresh (e1 e2 e3 t1 t2)
;;            (conde
;;             [(== e `(intc ,e1))
;;              (== t 'int) ]
;;             [(== e `(boolc ,e1))
;;              (== t 'bool)]
;;             [(== e `(zero? ,e1))
;;              (== t 'bool)
;;              (!-o gamma e1 'int)]
;;             [(== e `(* ,e1 ,e2))
;;              (== t 'int)
;;              (!-o gamma e1 'int)
;;              (!-o gamma e2 'int)]
;;             [(== e `(+ ,e1 ,e2))
;;              (== t 'int)
;;              (!-o gamma e1 'int)
;;              (!-o gamma e2 'int)]
;;             [(== e `(if ,e1 ,e2 ,e3))
;;              (!-o gamma e1 'bool)
;;              (!-o gamma e2 t)
;;              (!-o gamma e3 t)]
;;             [(== e `(var ,e1))
;;              (lookupo gamma e1 t)]
;;             [(== e `(lambda (,e1) ,e2))
;;              (== t `(-> ,t1 ,t2))
;;              (!-o `((,e1 . ,t1) . ,gamma) e2 t2)]
;;             [(== e `(app ,e1 ,e2))
;;              (!-o gamma e1 `(-> ,t1 ,t))
;;              (!-o gamma e2 t1)]))))

;; (define lookupo
;;   (lambda (G x t)
;;     (fresh (rest type y)
;;            (conde
;;             ((== `((,x . ,t) . ,rest) G))
;;             ((== `((,y . ,type) . ,rest) G)
;;              (=/= x y)
;;              (lookupo rest x t))))))
