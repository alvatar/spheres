(%load-library '(spheres/util test))

(%load-library '(spheres/object type-class))
(%load-library '(spheres/core base))
(%load-library '(spheres/core match))
(%load-library '(spheres/algorithm list))

(test-begin "Type-classes")


;;--------------------------------------------------------------------------------------------------

;; Simple Shapes OO
;;
;; class (Shape a) where ...
;;   get-x : a -> Number
;;   get-y : a -> Number
;;   set-x : a x -> void
;;   set-y : a y -> void
;;   draw  : a -> void

(define-class <Shape> get-x get-y set-x! set-y! draw)
 
(define-structure point x y)

;; draw-position : (Shape a) => a -> void
(define=> (draw-position <Shape>)
  (lambda (a)
    (display "Shape (")
    (display (get-x a))
    (display ", ")
    (display (get-y a))
    (display ")\n")))

;; instance Shape point where ...
(define point-shape
  (make-<Shape> point-x  
                point-y 
                point-x-set! 
                point-y-set!
                (lambda (a)
                  ((draw-position point-shape) a))))

(define-structure circle x y radius)

;; instance Shape circle-data 
;;    where ...
(define circle-shape 
  (make-<Shape> circle-x
                circle-y 
                circle-x-set!
                circle-y-set!
                (lambda (c)
                  (display "Circle: ")
                  ((draw-position circle-shape) c)                    
                  (display "        radius = ")
                  (display (circle-radius c))
                  (display "\n"))))

;; Tests

(define test-point (make-point 1 2))
(define test-circle (make-circle 7 7 7))

;; (with ((<Shape> point-shape))
;;            (draw test-point))
(test-equal
 "Lexical scope 1"
 (with-output-to-string
   '()
   (lambda ()
     ;; Type-class _with_
     (with ((<Shape> point-shape))
           (draw test-point))))
 "Shape (1, 2)\n")

;; (with ((<Shape> circle-shape))
;;       (draw test-circle))
(test-equal
 "Lexical scope 2"
 (with-output-to-string
   '()
   (lambda ()
     ;; Type-class _with_
     (with ((<Shape> circle-shape))
      (draw test-circle))))
 "Circle: Shape (7, 7)
        radius = 7\n")

;; draw-shapes : [exist a. ((Shape a) and a)] -> void
(define (draw-shapes lst)
  (for-each (lambda (sa.a)
              (with ((<Shape> (car sa.a)))
                    (draw (cdr sa.a))))
            lst))

(test-equal
 "Lexical scope 3"
 (with-output-to-string
   '()
   (lambda ()
     ;; Type-class _with_
     (draw-shapes (list (cons point-shape  test-point)
                        (cons circle-shape test-circle)
                        (cons point-shape  test-point)
                        (cons circle-shape test-circle)))))
 "Shape (1, 2)
Circle: Shape (7, 7)
        radius = 7
Shape (1, 2)
Circle: Shape (7, 7)
        radius = 7\n")


;; Extending the Shape class:

(define-class <Shape+> (<Shape> shape) translate)

;; translate : (Shape a) => a dx dy -> void
(define=> (translate <Shape>)
  (lambda (a dx dy)
    (set-x! a (+ (get-x a) dx))
    (set-y! a (+ (get-y a) dy))))

(define point+
  (make-<Shape+> point-shape
                 (translate point-shape)))

(define circle+
  (make-<Shape+> circle-shape
                 (translate circle-shape)))

(test-equal
 "Extending a class"
 (with-output-to-string
   '()
   (lambda ()
     (with ((<Shape+> circle+))
           (translate test-circle 7 7)
           (draw test-circle))))
 "Circle: Shape (14, 14)
        radius = 7\n")

(import-instance (<Shape+> circle+))
(test-equal
 "Import instance"
 (with-output-to-string
   '()
   (lambda ()
     (translate test-circle 10 5)
     (draw test-circle)))
 "Circle: Shape (24, 19)
        radius = 7\n")



;;--------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------


;; Equality example
;;
;; class (Eq a) where
;;   egal?     : a a -> boolean
;;   not-egal? : a a -> boolean
(define-class <Eq>
  egal?
  not-egal?)

(define (default-Eq egal?)
  (make-<Eq> egal?
             (lambda (x y) (not (egal? x y)))))

(define num-Eq (default-Eq =))
(define eqv-Eq (default-Eq eqv?))
(define chr-Eq (default-Eq char=?))


;; Collections example
;;
;; class (Collection a c) where
;;   empty  : c
;;   insert : a c -> c
;;   ...
(define-class <Collection>
  empty
  insert
  fold)

;; contains? : (Eq a) (Collection a c) => a c -> Bool
(define=> (contains? <Eq> <Collection>)
  (lambda (a c)
    (call/cc (lambda (break)
               (fold (lambda (x seed)
                       (if (egal? a x)
                           (break #t)
                           #f))
                     #f
                     c)))))

;; class (Eq a) (Collection a) => (Set a s) where
(define-class <Set> (<Eq> eq) (<Collection> coll))

;; set-member? : (Set a s) => a s -> Bool
(define=> (set-member? <Set>)
  (contains? eq coll))    

;; instance (Eq a) => Set a [a]
(define (list-Set eq)
  (let ((srfi-fold fold))
    (letrec* ((empty  '())
              (fold srfi-fold)
              (insert (lambda (x s)
                        (if ((set-member? this) x s) 
                            s
                            (cons x s))))
              (this (make-<Set> eq
                                (make-<Collection> empty
                                                   insert
                                                   fold))))
             this)))

;; instance Set Num [Num]
;; instance Set a [a]
(define num-Set (list-Set num-Eq))                             
(define eqv-Set (list-Set eqv-Eq))


;; instance Set char string where ...
(define chr-Set 
  (letrec* ((empty  "")
            (fold (lambda (f seed s)
                    (let loop ((acc seed)
                               (i   0))
                      (if (= i (string-length s))
                          acc
                          (loop (f (string-ref s i) acc)
                                (+ i 1))))))
            (insert (lambda (x s) 
                      (if ((set-member? this) x s)
                          s
                          (string-append (string x) s))))
            (this (make-<Set> chr-Eq
                              (make-<Collection> empty
                                                 insert  
                                                 fold))))
           this))

;; list->Set : (Set a s) => [a] -> s
(define=> (list->set <Set>)
  (lambda (lst)
    (fold (lambda (x s) (insert x s)) 
          empty 
          lst)))

;; heterogenous-union : (Set a sa) (Set b sb) => sa sb -> sa
(define=> (heterogenous-union (<Set> a.) (<Set> b.))
  (lambda (x y)
    (b.fold (lambda (elem accum)
              (a.insert elem accum))
            x
            y)))

;; Extending the Set class:
;;
;; class (Set a s) => Set+ a s where
;;   union : s s -> s
;;   ...
(define-class <Set+> (<Set> set)
  union
  member?
  list->set)

;; default-Set+ : (Set a s) -> (Set+ a s)
(define (default-Set+ sa)
  (make-<Set+> sa
               (heterogenous-union sa sa)
               (set-member? sa)
               (list->set sa)))

(define num-Set+ (default-Set+ num-Set))
(define chr-Set+ (default-Set+ chr-Set))


(test-equal "Complex type-class 1"
            (with ((<Set> num-Set))
                  empty)
            '())

;; ((heterogenous-union eqv-Set chr-Set)
;;              '(1 2 3 4 5)
;;              "abcde")
(test-equal "Complex type-class 2"
            ((heterogenous-union eqv-Set chr-Set)
             '(1 2 3 4 5)
             "abcde")
            '(#\e #\d #\c #\b #\a 1 2 3 4 5))

(test-assert "Complex type-class 3"
             (equal?+ (with ((<Set+> num-Set+ num.)
                             (<Set+> chr-Set+ chr.))
                            (values
                             num.empty
                             chr.empty))
                      (values '() "")))

(import-instance (<Set+> num-Set+ num.) 
                 (<Set+> chr-Set+ chr.))

(test-equal "Import qualified 1"
            (num.union '(1 2 3 4 5)
                       '(3 4 5 6 7))
            '(7 6 1 2 3 4 5))

(test-equal "Import qualified 2"
            (chr.list->set
             (list->string '(#\a #\b #\c #\d #\a)))
            "dcba")

(import-instance (<Set+> num-Set+))

(test-equal "Import extended class instance"
            (chr.list->set
             (list->string '(#\a #\b #\c #\d #\a)))
            "dcba")

(test-equal "Import extended class instance 2"
            empty
            '())

(test-equal "Import extended class instance 3"
            (union  '(1 2 3 4 5)
                    '(2 3 4 5 7))
            '(7 1 2 3 4 5))

(test-equal "Import extended class instance 4"
            (list->set '(1 1 2 3 4 3 4))
            '(4 3 2 1))


(test-end)
