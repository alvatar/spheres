(load (spheres/util test))

(load (spheres/core match))

(test-begin "Match")

(test-assert "(match ls
                       ((1 2 3) #t)
                       (else #f))"
             (let ((ls (list 1 2 3)))
               (match ls
                      ((1 2 3) #t)
                      (else #f))))

(test-equal "(let ((ls (list 1 2 3)))
              (match ls
                     ((1 2 4) #t)
                     (else #f)))"
            (let ((ls (list 1 2 3)))
              (match ls
                     ((1 2 4) #t)
                     (else #f)))
            #f)

(test-equal "(match (list 1 2 3)
                    ((a b c) b))"
            (match (list 1 2 3)
                   ((a b c) b))
            2)

(test-equal "(match (list 1 2 1)
                    ((a a b) 1)
                    ((a b a) 2))"
            (match (list 1 2 1)
                   ((a a b) 1)
                   ((a b a) 2))
            2)

(test-equal "(match (list 1 2 1)
                    ((_ _ b) 1)
                    ((a b a) 2))"
            (match (list 1 2 1)
                   ((_ _ b) 1)
                   ((a b a) 2))
            1)

(test-equal "(match 'a
                    ('b 1)
                    ('a 2))"
            (match 'a
                   ('b 1)
                   ('a 2))
            2)

(test-equal "(match (list 1 2 3)
                    (`(1 ,b ,c) (list b c)))"
            (match (list 1 2 3)
                   (`(1 ,b ,c) (list b c)))
            (list 2 3))

(test-assert "(match (list 1 2)
                     ((1 2 3 ...) #t))"
             (match (list 1 2)
                    ((1 2 3 ...) #t)))

(test-assert "(match (list 1 2 3)
                     ((1 2 3 ...) #t))"
             (match (list 1 2 3)
                    ((1 2 3 ...) #t)))

(test-assert "(match (list 1 2 3 3 3)
                     ((1 2 3 ...) #t))"
             (match (list 1 2 3 3 3)
                    ((1 2 3 ...) #t)))

(test-equal "(match (list 1 2)
                    ((a b c ...) c))"
            (match (list 1 2)
                   ((a b c ...) c))
            '())

(test-equal "(match (list 1 2 3)
                   ((a b c ...) c))"
            (match (list 1 2 3)
                   ((a b c ...) c))
            '(3))

(test-equal "(match (list 1 2 3 4 5)
                   ((a b c ...) c))"
            (match (list 1 2 3 4 5)
                   ((a b c ...) c))
            '(3 4 5))

(test-equal "(match (list 1 2 3 4)
                   ((a b c ... d e) c))"
            (match (list 1 2 3 4)
                   ((a b c ... d e) (list a b c d e)))
            '(1 2 () 3 4))

(test-equal "(match (list 1 2 3 4 5)
                   ((a b c ... d e) c))"
            (match (list 1 2 3 4 5)
                   ((a b c ... d e) c))
            '(3))

(test-equal "(match (list 1 2 3 4 5 6 7)
                   ((a b c ... d e) c))"
            (match (list 1 2 3 4 5 6 7)
                   ((a b c ... d e) c))
            '(3 4 5))

;; TODO
(test-error "(match (list 1 2)
                   ((a b c ..1) c))"
            #t
            (match (list 1 2)
                   ((a b c ..1) c)))

(test-equal "(match (list 1 2 3)
                   ((a b c ..1) c))"
            (match (list 1 2 3)
                   ((a b c ..1) c))
            '(3))

(test-assert "(match 1
                   ((and) #t))"
            (match 1
                   ((and) #t)))

(test-equal "(match 1
                    ((and x) x))"
            (match 1
                   ((and x) x))
            1)

(test-equal "(match 1
                   ((and x 1) x)
                   1)"
            (match 1
                   ((and x 1) x))
            1)

(test-equal "(match 1 ((or) #t) (else #f))"
            (match 1 ((or) #t) (else #f))
            #f)

(test-equal "(match 1 ((or x) x))"
            (match 1 ((or x) x))
            1)

(test-equal "(match 1 ((or x 2) x))"
            (match 1 ((or x 2) x))
            1)

(test-equal "(match 1 ((not 2) #t))"
            (match 1 ((not 2) #t))
            #t)

(test-equal "(match 1 ((? odd? x) x))"
            (match 1 ((? odd? x) x))
            1)

(test-equal "(match '(1 . 2) ((= car x) x))"
            (match '(1 . 2) ((= car x) x))
            1)

(test-equal "(match 4 ((= (lambda (a) (* a a)) x) x))"
            (match 4 ((= (lambda (a) (* a a)) x) x))
            16)

;; In Gambit, for matching to work requires a type-exhibitor with the
;; type name to match
(define-type
  point
  type-exhibitor: point
  x
  y)

(test-equal "(match (make-point 300 200)
                   (($ point a b) (list a b)))"
            (match (make-point 300 200)
                   (($ point a b) (list a b)))
            (list 300 200))

(test-equal "(let ((x (cons 1 2)))
              (match x ((1 . (set! s)) (s 3) x)))"
            (let ((x (cons 1 2)))
              (match x ((1 . (set! s)) (s 3) x)))
            '(1 . 3))

(test-equal "(match '(1 . 2)
                    ((1 . (get! g)) (g)))"
            (match '(1 . 2)
                   ((1 . (get! g)) (g)))
            2)

(test-equal "(match '(a (a (a b)))
                    ((x *** 'b) x))"
            (match '(a (a (a b)))
                   ((x *** 'b) x))
            '(a a a))

(test-end)
