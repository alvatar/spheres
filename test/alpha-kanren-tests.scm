(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (println "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (println "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                      'tested-expression expected produced)))))))

(test "1"
 (run 1 (q) (exist (x y z) (== x z) (== 3 y)))
 '(_.0))

(test "2"
 (run 1 (q) (exist (x y) (== x q) (== 3 y)))
 '(_.0))

(test "3"
  (run 1 (y)
    (exist (x z)
      (== x z)
      (== 3 y)))
 '(3))

(test "4"
 (run 1 (q)
  (exist (x z)
    (== x z)
    (== 3 z)
    (== q x)))
 '(3))

(test "5"
  (run 1 (y)
   (exist (x y)
     (== 4 x)
     (== x y))
   (== 3 y))
  '(3))

(test "6"
  (run 1 (x) (== 4 3))
  '())

(test "7"
 (run 2 (q)
  (exist (x y z)
    (conde
      ((== `(,x ,y ,z ,x) q))
      ((== `(,z ,y ,x ,z) q)))))
 '((_.0 _.1 _.2 _.0)
   (_.0 _.1 _.2 _.0)))

(test "8"
 (run 5 (q)
  (let loop ()
    (conde
      ((== #f q))
      ((== #t q))
      ((loop)))))
 '(#f #t #f #t #f))

(define anyo
  (lambda (g)
    (conde
      (g)
      ((anyo g)))))

(test "9"
 (run 5 (q)
  (conde
    ((anyo (== #f q)))
    ((== #t q))))
 '(#t #f #f #f #f))

(test "10"
 (run 10 (q)
   (anyo
    (conde
      ((== 1 q))
      ((== 2 q))
      ((== 3 q)))))
 '(1 2 3 1 2 3 1 2 3 1))

(test "11"
 (run 3 (q)
  (let ((nevero (anyo (== #f #t))))
    (conde
      ((== 1 q))
      (nevero)
      ((conde
         ((== 2 q))
         (nevero)
         ((== 3 q)))))))
 '(1 2 3))

(test "12"
  (run* (q) (fresh (a) (== a a)))
  '(_.0))

(test "13"
 (run* (q) (fresh (a) (== a 5)))
 '())

(test "14"
  (run* (q) (fresh (a b) (== a b)))
  '())

(test "15"
  (run* (q) (fresh (a b) (== b q)))
  '(a.0))

(test "16"
 (run* (q)
  (exist (x y z)
    (fresh (a)
      (== x a)
      (fresh (a b)
        (== y a)
        (== `(,x ,y ,z ,a ,b) q)))))
  '((a.0 a.1 _.0 a.1 a.2)))

(test "17"
 (run* (q)
  (fresh (a b)
    (== (tie a `(foo ,a 3 ,b)) q)))
 '((tie-tag a.0 (foo a.0 3 a.1))))

(test "18"
  (run* (q) (fresh (a b) (== `(foo ,a ,a) `(foo ,b ,b))))
  '())

(test "19"
  (run* (q) (fresh (a b) (== (tie a a) (tie b b))))
  '(_.0))

(test "20"
 (run* (q) (fresh (a b) (== (tie a q) (tie b b))))
 '(a.0))

(test "21"
 (run* (q)
  (exist (t u)
    (fresh (a b c d)
      (== `(lam ,(tie a `(lam ,(tie b `(var ,a))))) t)
      (== `(lam ,(tie c `(lam ,(tie d `(var ,c))))) u)
      (== t u))))
 '(_.0))

(test "22"
 (run* (q)
  (exist (t u)
    (fresh (a b c d)
      (== `(lam ,(tie a `(lam ,(tie b `(var ,a))))) t)
      (== `(lam ,(tie c `(lam ,(tie d `(var ,d))))) u)
      (== t u))))
 '())

(test "23"
 (run* (q) (fresh (a) (hash a a)))
 '())

(test "24"
 (run* (q) (fresh (a) (hash a 5)))
 '(_.0))

(test "25"
 (run* (q) (fresh (a) (hash a (tie a a))))
 '(_.0))

(test "26"
 (run* (q) (fresh (a b) (hash a (tie b a))))
 '())

(test "27"
 (run* (k)
  (exist (t)
    (fresh (a)
      (hash a k)
      (== `(5 ,(tie a a) ,t) k))))
 '(((5 (tie-tag a.0 a.0) _.0) : ((a.0 . _.0)))))

(test "28"
 (run* (k)
  (exist (t)
    (fresh (a)
      (hash a k)
      (== `(5 ,(tie a a) ,t) k)
      (== `(foo ,a 7) t))))
 '())

(test "29"
 (run* (k)
  (exist (t)
    (fresh (a b)
      (== (tie a (tie b t)) k)
      (hash a t)
      (== `((,a ,b) ,b) t))))
 '())

(test "30"
 (run* (k)
  (exist (t)
    (fresh (a b)
      (== (tie a (tie b t)) k)
      (hash a t)
      (== `(,b ,(tie a `(,a (,b ,b)))) t))))
 '((tie-tag a.0 (tie-tag a.1 (a.1 (tie-tag a.0 (a.0 (a.1 a.1))))))))

(test "31"
 (run* (q)
  (exist (k1 k2 t u)
    (fresh (a b c d)
      (== (tie a (tie b t)) k1)
      (hash a t)
      (== (tie c (tie d u)) k2)
      (hash c u)
      (== k1 k2)
      (== `(,k1 ,k2) q))))
 '((((tie-tag a.0 (tie-tag a.1 (susp-tag ((a.1 a.2) (a.0 a.3)) _.0)))
    (tie-tag a.3 (tie-tag a.2 _.0)))
   :
   ((a.3 . _.0) (a.1 . _.0) (a.0 . _.0)))))

(test "32"
 (run* (q)
  (exist (k1 k2 t u)
    (fresh (a b c d)
      (== (tie a (tie b t)) k1)
      (hash a t)
      (== `(,b ,b) t)
      (== (tie c (tie d u)) k2)
      (hash c u)
      (== `(,d ,d) u)
      (== k1 k2)
      (== `(,k1 ,k2) q))))
 '(((tie-tag a.0 (tie-tag a.1 (a.1 a.1)))
    (tie-tag a.2 (tie-tag a.3 (a.3 a.3))))))

(test "33"
 (run* (q)
   (fresh (a b)
     (exist (x y)
       (== (tie a (tie a x)) (tie a (tie b y)))
       (== `(,x ,y) q))))
 '((((susp-tag ((a.0 a.1)) _.0) _.0) : ((a.0 . _.0)))))

(test "34"
 (run* (q)
  (fresh (a b)
    (exist (x y)
      (conde
        ((== (tie a (tie b `(,x ,b))) (tie b (tie a `(,a ,x)))))
        ((== (tie a (tie b `(,y ,b))) (tie b (tie a `(,a ,x)))))
        ((== (tie a (tie b `(,b ,y))) (tie b (tie a `(,a ,x)))))
        ((== (tie a (tie b `(,b ,y))) (tie a (tie a `(,a ,x))))))
      (== `(,x ,y) q))))
 '((a.0 a.1)
  (_.0 (susp-tag ((a.0 a.1)) _.0))
  ((_.0 (susp-tag ((a.0 a.1)) _.0)) : ((a.0 . _.0)))))


;; Adopted from this code in examples/lam.apl from alphaProlog release 'aprolog-0.3'
;; (see http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/)

;; (* correct substitution *)

;; pred subst (id\tm) tm tm.
;; subst (a\var a) E E.
;; subst (a\var B) E (var B) :- a # B.
;; subst (a\app E1 E2) E (app E1' E2') :- subst (a\E1) E E1', subst (a\E2) E E2'.
;; subst (a\lam (b\E1)) E (lam (b\E1')) / b # E :- subst (a\E1) E E1'.

(define substo
  (lambda (id/tm E out)
    (conde
      ((fresh (a)
         (== (tie a `(var ,a)) id/tm)
         (== E out)))
      ((fresh (a)
         (exist (B)
           (hash a B)
           (== (tie a `(var ,B)) id/tm)
           (== `(var ,B) out))))
      ((fresh (a b)
         (exist (E1 E1^)
           (hash b E)
           (== (tie a `(lam ,(tie b E1))) id/tm)
           (== `(lam ,(tie b E1^)) out)
           (substo (tie a E1) E E1^))))
      ((fresh (a)
         (exist (E1 E2 E1^ E2^)
           (== (tie a `(app ,E1 ,E2)) id/tm)
           (== `(app ,E1^ ,E2^) out)
           (substo (tie a E1) E E1^)
           (substo (tie a E2) E E2^)))))))

(test "35"
 (run* (x)
  (fresh (a b)
    (substo (tie b `(lam ,(tie a `(var ,b)))) `(var ,a) x)))
 '((lam (tie-tag a.0 (var a.1)))))

(test "36"
 (run* (q)
  (fresh (a b)
    (substo
     (tie a `(lam ,(tie a `(app (var ,a) (var ,b))))) b q)))
 '((lam (tie-tag a.0 (app (var a.0) (var a.1))))))

(test "37"
 (run 10 (q)
   (exist (id/tm E out)
     (substo id/tm E out)
     (== `(,id/tm ,E ,out) q)))
 '(((tie-tag a.0 (var a.0)) _.0 _.0)
   (((tie-tag a.0 (var _.0)) _.1 (var _.0)) : ((a.0 . _.0)))
   (((tie-tag a.0 (lam (tie-tag a.1 (var a.0)))) _.0
     (lam (tie-tag a.1 _.0)))
    : ((a.1 . _.0)))
   (((tie-tag a.0
              (lam (tie-tag a.1 (var (susp-tag ((a.2 a.0)) _.0)))))
     _.1 (lam (tie-tag a.1 (var _.0))))
    : ((a.1 . _.1) (a.2 . _.0) (a.0 . _.0)))
   ((tie-tag a.0 (app (var a.0) (var a.0))) _.0 (app _.0 _.0))
   (((tie-tag a.0
              (lam (tie-tag a.1 (lam (tie-tag a.2 (var a.0))))))
     _.0 (lam (tie-tag a.1 (lam (tie-tag a.2 _.0)))))
    : ((a.1 . _.0) (a.2 . _.0)))
   (((tie-tag a.0
              (app (var a.0) (var (susp-tag ((a.1 a.0)) _.0))))
     _.1 (app _.1 (var _.0)))
    : ((a.1 . _.0) (a.0 . _.0)))
   (((tie-tag a.0
              (app (var (susp-tag ((a.1 a.0)) _.0)) (var a.0)))
     _.1 (app (var _.0) _.1))
    : ((a.1 . _.0) (a.0 . _.0)))
   (((tie-tag a.0
              (lam
               (tie-tag a.1
                        (lam
                         (tie-tag a.2
                                  (var (susp-tag ((a.3 a.0) (a.4 a.3)) _.0)))))))
     _.1 (lam (tie-tag a.1 (lam (tie-tag a.2 (var _.0))))))
    :
    ((a.1 . _.1) (a.2 . _.1) (a.0 . _.0) (a.4 . _.0)
     (a.3 . _.0)))
   (((tie-tag a.0
              (lam (tie-tag a.1 (app (var a.0) (var a.0)))))
     _.0 (lam (tie-tag a.1 (app _.0 _.0))))
    : ((a.1 . _.0)))))

;; Adopted from this code in examples/lam.apl from alphaProlog release 'aprolog-0.3'
;; (see http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/)

;; pred beta tm tm.
;; beta (app (lam (b\E)) E') E'' :- subst(b\E) E' E''.

(define betao
  (lambda (t1 E^^)
    (fresh (b)
      (exist (E E^)
        (== `(app (lam ,(tie b E)) ,E^) t1)
        (substo (tie b E) E^ E^^)))))

(test "38"
 (run* (q)
   (fresh (a b)
     (betao `(app (lam ,(tie a `(lam ,(tie b `(var ,a))))) (var ,b)) q)))
 '((lam (tie-tag a.0 (var a.1)))))

;; ?beta (app (lam (a\ lam (b\ var a))) (var b)) (lam (c\var b)).
(test "39"
 (run* (q)
   (fresh (a b c)
     (betao `(app (lam ,(tie a `(lam ,(tie b `(var ,a))))) (var ,b)) `(lam ,(tie c `(var ,b))))))
 '(_.0))

;; ?beta (app (lam (a\ lam (b\ var a))) (var b)) (lam (b\var b)).
(test "40"
 (run* (q)
   (fresh (a b)
     (betao `(app (lam ,(tie a `(lam ,(tie b `(var ,a))))) (var ,b)) `(lam ,(tie b `(var ,b))))))
 '())

;; ?beta (app (lam (a\ lam (b\ var a))) (var b)) (lam (b\var a)).
(test "41"
 (run* (q)
   (fresh (a b)
     (betao `(app (lam ,(tie a `(lam ,(tie b `(var ,a))))) (var ,b)) `(lam ,(tie b `(var ,a))))))
 '())


(test "42"
 (run 3 (q)
   (exist (t1 t2)
     (betao t1 t2)
     (== `(,t1 ,t2) q)))
 '(((app (lam (tie-tag a.0 (var a.0))) _.0) _.0)
   (((app
      (lam (tie-tag a.0 (var (susp-tag ((a.1 a.0)) _.0))))
      _.1)
     (var _.0))
    : ((a.1 . _.0) (a.0 . _.0)))
   (((app (lam (tie-tag a.0 (lam (tie-tag a.1 (var a.0)))))
          _.0)
     (lam (tie-tag a.1 _.0)))
    : ((a.1 . _.0)))))

;; Adopted from this code in examples/lam.apl from alphaProlog release 'aprolog-0.3'
;; (see http://homepages.inf.ed.ac.uk/jcheney/programs/aprolog/)

;; (* single step reduction *)
;; pred step tm tm.
;; step M M' :- beta M M'.
;; step (app M N) (app M' N) :- step M M'.
;; step (app M N) (app M N') :- step N N'.
;; step (lam (x\M)) (lam (x\M')) :- step M M'.

;; My reducer will not go under lambda's
(define stepo
  (lambda (t1 t2)
    (conde
      ((betao t1 t2))
      ((exist (M N M^)
         (== `(app ,M ,N) t1)
         (== `(app ,M^ ,N) t2)
         (stepo M M^)))
      ((exist (M N N^)
         (== `(app ,M ,N) t1)
         (== `(app ,M ,N^) t2)
         (stepo N N^))))))

(test "43"
 (run* (q)
   (fresh (a b)
     (stepo `(app (lam ,(tie a `(lam ,(tie b `(var ,a))))) (var ,b)) q)))
 '((lam (tie-tag a.0 (var a.1)))))

;;; generate a quine (Omega)
(test "44"
 (run 1 (Q)
   (fresh (z)
     (hash z Q)
     (stepo Q Q)))
 '((app (lam (tie-tag a.0 (app (var a.0) (var a.0))))
        (lam (tie-tag a.0 (app (var a.0) (var a.0)))))))

;; reflexive transitive closure of stepo
(define stepso
  (lambda (t1 t2)
    (conde
      ((== t1 t2))
      ((exist (t)
         (stepo t1 t)
         (stepso t t2))))))

(test "45"
 (run* (q)
   (fresh (a b)
     (stepso `(app (lam ,(tie a `(lam ,(tie b `(var ,a))))) (var ,b)) q)))
 '((app (lam (tie-tag a.0 (lam (tie-tag a.1 (var a.0))))) (var a.1))
   (lam (tie-tag a.0 (var a.1)))))

(test "46"
 (run 3 (q)
   (exist (M N)
     (stepso M N)
     (== `(,M ,N) q)))
 '((_.0 _.0)
   ((app (lam (tie-tag a.0 (var a.0))) _.0) _.0)
   (((app
      (lam (tie-tag a.0 (var (susp-tag ((a.1 a.0)) _.0))))
      _.1)
     (var _.0))
    : ((a.1 . _.0) (a.0 . _.0)))))

;; combinator generation

;; I combinator

(test "47"
 (run 1 (I)
   (exist (E)
     (fresh (a b)
       (== `(lam ,(tie b E)) I)
       (hash a I)
       (stepso `(app ,I (var ,a)) `(var ,a)))))
 '((lam (tie-tag a.0 (var a.0)))))

(test "48"
 (run 3 (I)
   (exist (E)
     (fresh (a b)
       (== `(lam ,(tie b E)) I)
       (hash a I)
       (stepso `(app ,I (var ,a)) `(var ,a)))))
 '((lam (tie-tag a.0 (var a.0)))
   (lam
    (tie-tag a.0
             (app (lam (tie-tag a.1 (var a.0))) (var a.0))))
   ((lam
     (tie-tag a.0
              (app (lam (tie-tag a.1 (var a.0)))
                   (var (susp-tag ((a.2 a.0) (a.3 a.2) (a.4 a.3)) _.0)))))
    : ((a.0 . _.0) (a.2 . _.0) (a.4 . _.0) (a.3 . _.0)))))

;; W combinator

; Wxy => xyy

(test "49"
 (run 1 (W)
   (exist (E)
     (fresh (x y c)
       (== `(lam ,(tie c E)) W)
       (hash x W)
       (hash y W)
       (stepso `(app (app ,W (var ,x)) (var ,y))
               `(app (app (var ,x) (var ,y)) (var ,y))))))
 '((lam
    (tie-tag a.0
             (lam
              (tie-tag a.1
                       (app (app (var a.0) (var a.1)) (var a.1))))))))

(test "50"
 (run 1 (W)
   (exist (E)
     (fresh (x y c d)
       (== `(lam ,(tie c `(lam ,(tie d E)))) W)
       (hash x W)
       (hash y W)
       (stepso `(app (app ,W (var ,x)) (var ,y))
               `(app (app (var ,x) (var ,y)) (var ,y))))))
 '((lam
    (tie-tag a.0
             (lam
              (tie-tag a.1
                       (app (app (var a.0) (var a.1)) (var a.1))))))))

;; B' combinator

; B'xyz => y(xz)

(test "51"
 (run 1 (B^)
   (exist (E)
     (fresh (x y z c d e)
       (== `(lam ,(tie c `(lam ,(tie d `(lam ,(tie e E)))))) B^)
       (hash x B^)
       (hash y B^)
       (hash z B^)
       (stepso `(app (app (app ,B^ (var ,x)) (var ,y)) (var ,z))
               `(app (var ,y) (app (var ,x) (var ,z)))))))
 '((lam
    (tie-tag a.0
             (lam
              (tie-tag a.1
                       (lam
                        (tie-tag a.2
                                 (app (var a.1) (app (var a.0) (var a.2)))))))))))

;; Y combinator
;;
;; Yx = x(Yx)

;; Y = λf.(λx.f (x x)) (λx.f (x x))


(test "52"
 (run* (t)
   (exist (Y)
     (fresh (f x g)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash g Y)
       (betao `(app ,Y (var ,g)) t))))
 '((app
    (lam
     (tie-tag a.0 (app (var a.1) (app (var a.0) (var a.0)))))
    (lam
     (tie-tag a.2 (app (var a.1) (app (var a.2) (var a.2))))))))

(test "53"
 (run* (t)
   (exist (Y t1)
     (fresh (f x g)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash g Y)
       (betao `(app ,Y (var ,g)) t1)
       (betao t1 t))))
 '((app (var a.0)
        (app
         (lam
          (tie-tag a.1
                   (app (var a.0) (app (var a.1) (var a.1)))))
         (lam
          (tie-tag a.1
                   (app (var a.0) (app (var a.1) (var a.1)))))))))


(test "54"
 (run 1 (q)
   (exist (Y t1 t2)
     (fresh (f x g)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash g Y)
       (betao `(app ,Y (var ,g)) t1)
       (betao t1 t2)
       (== `(,t1 ,t2) q))))
 '(((app
     (lam
      (tie-tag a.0 (app (var a.1) (app (var a.0) (var a.0)))))
     (lam
      (tie-tag a.2 (app (var a.1) (app (var a.2) (var a.2))))))
    (app (var a.1)
         (app
          (lam
           (tie-tag a.2
                    (app (var a.1) (app (var a.2) (var a.2)))))
          (lam
           (tie-tag a.2
                    (app (var a.1) (app (var a.2) (var a.2))))))))))

(test "55"
 (run 1 (t)
   (exist (Y)
     (fresh (z f x)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash z Y)
       (stepso `(app (var ,z) (app ,Y (var ,z))) t)
       (stepso `(app ,Y (var ,z)) t))))
 '((app (var a.0)
        (app
         (lam
          (tie-tag a.1
                   (app (var a.0) (app (var a.1) (var a.1)))))
         (lam
          (tie-tag a.2
                   (app (var a.0) (app (var a.2) (var a.2)))))))))


(test "56"
 (run 1 (Y)
   (exist (t)
     (fresh (z f x)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash z Y)
       (stepso `(app (var ,z) (app ,Y (var ,z))) t)
       (stepso `(app ,Y (var ,z)) t))))
 '((lam
    (tie-tag a.0
             (app
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1)))))
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1))))))))))

(test "57"
 (run 1 (Y)
   (exist (t E)
     (fresh (z f x)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) ,E))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash z Y)
       (stepso `(app (var ,z) (app ,Y (var ,z))) t)
       (stepso `(app ,Y (var ,z)) t))))
 '((lam
    (tie-tag a.0
             (app
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1)))))
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1))))))))))

(test "58"
 (run 1 (Y)
   (exist (t E)
     (fresh (z f x)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) ,E)))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash z Y)
       (stepso `(app (var ,z) (app ,Y (var ,z))) t)
       (stepso `(app ,Y (var ,z)) t))))
 '((lam
    (tie-tag a.0
             (app
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1)))))
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1))))))))))

(test "59"
 (run 1 (q)
   (exist (Y t1 t2)
     (fresh (z f x)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash z Y)
       (stepso `(app (var ,z) (app ,Y (var ,z))) t1)
       (stepso `(app ,Y (var ,z)) t2)
       (== `((t1: ,t1) (t2: ,t2)) q))))
 '(((t1:
    (app (var a.0)
      (app
        (lam
          (tie-tag a.1
            (app
              (lam
                (tie-tag a.2
                  (app (var a.1) (app (var a.2) (var a.2)))))
              (lam
                (tie-tag a.2
                  (app (var a.1) (app (var a.2) (var a.2))))))))
        (var a.0))))
   (t2:
     (app
       (lam
         (tie-tag a.1
           (app
             (lam
               (tie-tag a.2
                 (app (var a.1) (app (var a.2) (var a.2)))))
             (lam
               (tie-tag a.2
                 (app (var a.1) (app (var a.2) (var a.2))))))))
       (var a.0))))))

(test "60"
 (run 3 (q)
   (exist (Y t1 t2)
     (fresh (z f x)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash z Y)
       (stepso `(app (var ,z) (app ,Y (var ,z))) q))))
 '((app (var a.0)
        (app
         (lam
          (tie-tag a.1
                   (app
                    (lam
                     (tie-tag a.2
                              (app (var a.1) (app (var a.2) (var a.2)))))
                    (lam
                     (tie-tag a.2
                              (app (var a.1) (app (var a.2) (var a.2))))))))
         (var a.0)))
   (app (var a.0)
        (app
         (lam
          (tie-tag a.1
                   (app (var a.0) (app (var a.1) (var a.1)))))
         (lam
          (tie-tag a.2
                   (app (var a.0) (app (var a.2) (var a.2)))))))
   (app (var a.0)
        (app (var a.0)
             (app
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1)))))
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1))))))))))

(test "61"
 (run 3 (q)
   (exist (Y t1 t2)
     (fresh (z f x)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash z Y)
       (stepso `(app ,Y (var ,z)) q))))
 '((app
    (lam
     (tie-tag a.0
              (app
               (lam
                (tie-tag a.1
                         (app (var a.0) (app (var a.1) (var a.1)))))
               (lam
                (tie-tag a.1
                         (app (var a.0) (app (var a.1) (var a.1))))))))
    (var a.2))
   (app
    (lam
     (tie-tag a.0 (app (var a.1) (app (var a.0) (var a.0)))))
    (lam
     (tie-tag a.2 (app (var a.1) (app (var a.2) (var a.2))))))
   (app (var a.0)
        (app
         (lam
          (tie-tag a.1
                   (app (var a.0) (app (var a.1) (var a.1)))))
         (lam
          (tie-tag a.1
                   (app (var a.0) (app (var a.1) (var a.1)))))))))

(test "62"
 (run 1 (q)
   (exist (t1 t2)
     (fresh (x y z)
       (==
        `(app (var ,x)
              (app
               (lam
                ,(tie y
                      `(app (var ,x) (app (var ,y) (var ,y)))))
               (lam
                ,(tie z
                      `(app (var ,x) (app (var ,z) (var ,z)))))))
        t1)
       (==
        `(app (var ,x)
              (app
               (lam
                ,(tie y
                      `(app (var ,x) (app (var ,y) (var ,y)))))
               (lam
                ,(tie y
                      `(app (var ,x) (app (var ,y) (var ,y)))))))
        t2)
       (== t1 t2)
       (== `((t1: ,t1) (t2: ,t2)) q))))
 '(((t1:
     (app (var a.0)
          (app
           (lam
            (tie-tag a.1
                     (app (var a.0) (app (var a.1) (var a.1)))))
           (lam
            (tie-tag a.2
                     (app (var a.0) (app (var a.2) (var a.2))))))))
    (t2:
     (app (var a.0)
          (app
           (lam
            (tie-tag a.1
                     (app (var a.0) (app (var a.1) (var a.1)))))
           (lam
            (tie-tag a.1
                     (app (var a.0) (app (var a.1) (var a.1)))))))))))


(test "63"
 (run 1 (t)
   (exist (Y)
     (fresh (z f x)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash z Y)
       (stepso `(app ,Y (var ,z)) t)
       (stepso `(app (var ,z) (app ,Y (var ,z))) t))))
 '((app (var a.0)
        (app
         (lam
          (tie-tag a.1
                   (app (var a.0) (app (var a.1) (var a.1)))))
         (lam
          (tie-tag a.1
                   (app (var a.0) (app (var a.1) (var a.1)))))))))


(test "64"
 (run 1 (Y)
   (exist (t1 t2)
     (fresh (z f x)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash z Y)
       (stepo `(app ,Y (var ,z)) t1)
       (stepo `(app (var ,z) (app ,Y (var ,z))) t2)
       (stepso t1 t2))))
 '((lam
    (tie-tag a.0
             (app
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1)))))
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1))))))))))


(define step-equalo
  (lambda (t1 t2)
    (conde
      ((== t1 t2))
      ((exist (t1^)
         (stepo t1 t1^)
         (step-equalo t1^ t2)))
      ((exist (t2^)
         (stepo t2 t2^)
         (step-equalo t1 t2^))))))

(test "65"
 (run 1 (W)
   (exist (E)
     (fresh (x y c)
       (== `(lam ,(tie c E)) W)
       (hash x W)
       (hash y W)
       (step-equalo `(app (app ,W (var ,x)) (var ,y))
                    `(app (app (var ,x) (var ,y)) (var ,y))))))
 '((lam
    (tie-tag a.0
             (lam
              (tie-tag a.1
                       (app (app (var a.0) (var a.1)) (var a.1))))))))


(test "66"
 (run 1 (Y)
   (fresh (z f x)
     (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                             (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
         Y)
     (hash z Y)
     (step-equalo `(app ,Y (var ,z)) `(app (var ,z) (app ,Y (var ,z))))))
 '((lam
    (tie-tag a.0
             (app
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1)))))
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1))))))))))

(test "67"
 (run 1 (Y)
   (fresh (z f x)
     (exist (U)
       (== `(lam ,(tie f `(app ,U ,U))) Y)
       (== `(lam ,(tie f `(app (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x)))))
                               (lam ,(tie x `(app (var ,f) (app (var ,x) (var ,x))))))))
           Y)
       (hash z Y)
       (step-equalo `(app ,Y (var ,z)) `(app (var ,z) (app ,Y (var ,z)))))))
 '((lam
    (tie-tag a.0
             (app
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1)))))
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1))))))))))


(println "this test takes a while...\n")
(test "68"
  (run 1 (Y)
    (fresh (z f x)
      (exist (U)
        (== `(lam ,(tie f `(app ,U ,U))) Y)
        (hash z Y)
        (step-equalo `(app ,Y (var ,z)) `(app (var ,z) (app ,Y (var ,z)))))))
 '((lam
    (tie-tag a.0
             (app
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1)))))
              (lam
               (tie-tag a.1
                        (app (var a.0) (app (var a.1) (var a.1))))))))))

(println "this test takes a while...\n")
(test "69"
  (run 1 (Y)
    (fresh (z f x)
      (exist (U)
        (== `(lam ,(tie f `(app ,U ,U))) Y)
        (hash z Y)
        (step-equalo `(app (var ,z) (app ,Y (var ,z))) `(app ,Y (var ,z))))))
  '((lam
     (tie-tag a.0
              (app
               (lam
                (tie-tag a.1
                         (app (var a.0) (app (var a.1) (var a.1)))))
               (lam
                (tie-tag a.1
                         (app (var a.0) (app (var a.1) (var a.1))))))))))
