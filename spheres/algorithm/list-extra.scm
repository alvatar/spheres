;;; Copyright (c) 2012-2014, Alvaro Castro-Castilla. All rights reserved.
;;; Non-standard list procedures


(declare (fixnum))

;;-------------------------------------------------------------------------------
;;!! Basic

;;! atom?
(define atom?
  (lambda (x) (and (not (pair? x)) (not (null? x)))))

;;! xor
(define (xor a b)
  (if a (not b) b))

;;! snoc (important: always prefer the use of cons)
(define snoc
  (lambda (ls x)
    (append ls (list x))))

;;! all cars and all cdrs
(define (cars+cdrs ls)
  (call/cc
   (lambda (abort)
     (let recur ((ls ls))
       (if (pair? ls)
           (receive (hl tl) (car+cdr ls)
                    (if (null? hl) (abort '() '())
                        (receive (a d) (car+cdr hl)
                                 (receive (cars cdrs) (recur tl)
                                          (values (cons a cars)
                                                  (cons d cdrs))))))
           (values '() '()))))))

;;! all cars
(define (cars ls) (map car ls))

;;! all cdrs
(define (cdrs ls) (map cdr ls))

;;! pick an element starting from the end
(define (list-ref-right k lst)
  (let ((len (length lst)))
    (list-ref lst (- len k 1))))

;;! Rotate the list by taking the head and placing it at the tail
;; '(a b c d e) 1 -> '(b c d e a)
(define (rotate-left k lst)
  (if (zero? k)
      lst
      (let recur-cut ((l lst)
                      (k1 k)
                      (len 0))
        (cond
         ((null? l) ;; In case it wraps
          (rotate-left (modulo k len) lst))
         ((zero? k1)
          (append l
                  (let recur-append ((l lst)
                                     (k2 k))
                    (if (zero? k2)
                        '()
                        (cons (car l)
                              (recur-append (cdr l)
                                            (- k2 1)))))))
         (else
          (recur-cut (cdr l)
                     (- k1 1)
                     (+ len 1)))))))

;;! Rotate the list by taking the last element and placing it at the head
;; '(a b c d e) 1 -> '(e a b c d)
(define (rotate-right k lst)
  (if (zero? k)
      lst
      (let* ((len (length lst))
             (k (modulo k len)))
        (receive (selected discarded)
                 (let recur-discard ((l lst)
                                     (k1 (- len k)))
                   (if (zero? k1)
                       (values '() l)
                       (receive (tail discarded)
                                (recur-discard (cdr l)
                                               (- k1 1))
                                (values (cons (car l) tail)
                                        discarded))))
                 (append discarded selected)))))

;;! Swap elements in a list destructively
;; (define li '(0 1 2 3 4 5))
;; (list-swap! li 2 4)
;; li -> '(0 1 4 3 2 5)
(define (list-swap! v i j)
  (let* ((x (list-tail v i))
         (y (list-tail v j))
         (a (car x))
         (b (car y)))
    (set-car! x b)
    (set-car! y a)))


;;-------------------------------------------------------------------------------
;;!! Map/fold variants

;;! Recursive map that applies function to leafs
(define (map* f l)
  (cond
   ((null? l) '())
   ((not (pair? l)) (f l))
   (else
    (cons (map* f (car l)) (map* f (cdr l))))))

;;! Recursive map that applies function to each node
(define (map** f l)
  (cond
   ((null? l) '())
   ((not (pair? l)) (f l))
   (else
    (cons (f (map** f (car l))) (f (map** f (cdr l)))))))

;;! Map that generates a value for each element
;; (map/values (lambda (x y z) (values x y z)) '(a 1) '(b 2) '(c 3))
;; => (a b c)
;;    (1 2 3)
;;
;;     A          B          C
;;     +----+     +----+     +----+lists
;;  ---+----+-----+----+-----+----+--------> (f A0 B0 C0) ----> val1
;;     |0   |     |0   |     |0   |
;;     +----+     +----+     +----+
;;  ---+----+-----+----+-----+----+--------> (f A1 B1 C1) ----> val2
;;     |1   |     |1   |     |1   |
;;     +----+     +----+     +----+
;;  ---+----+-----+----+-----+----+--------> (f A2 B2 C2) ----> val3
;;     |2   |     |2   |     |2   |
;;     +----+     +----+     +----+
(define (map/values f . ls)
  (list->values
   (apply map (lambda args (values->list (apply f args))) ls)))

;;! Fold that accumulates several values
;; (fold/values (lambda (x a b) (values (cons (+ 1 x) a) (cons x b))) '(() ()) '(1 2 3 4 5))
;; => (6 5 4 3 2)
;;    (5 4 3 2 1)
;; (fold/values (lambda (x a b) (values (cons (car x) a) (cons (cadr x) b))) '(() ()) '((a 1) (b 2) (c 3)))
;; => (c b a)
;;    (3 2 1)
;;                               +----+    +----+    +----+
;;                        +-------val1------val2------val3--
;;                        |      +----+    +----+    +----+
;;                        |        ^         ^         ^
;;  +---+  +---+lists     v        |         |         |
;; --A0-----B0---------> f* -------+---------+---------+
;;  +---+  +---+          |
;;  |A1 |  |B1 | ------> f*
;;  +---+  +---+          |      -----> val1
;;  |A2 |  |B2 |    --->       /
;;  +---+  +---+          |   /
;;  |A3 |  |B3 |          v  /
;;  +---+  +---+         f* ---- -----> val2
;;  |A4 |  |B4 |             \
;;  +---+  +---+              \
;;                             \
;;                               -----> val3
(define (fold/values kons knil . ls)
  (list->values
   (apply fold
          (lambda args
            (let ((rev (reverse args))) ; (x . y (a . b)) -> (x . y . a . b)
              (values->list (apply kons (append (cdr rev) (car rev))))))
          knil
          (reverse ls))))

;;! Demultiplex a list
;; (demux (lambda (x) (values (car x) (cadr x))) '((a 1) (b 2) (c 3)))
;; => (a b c)
;;    (1 2 3)
;;                                       +---+---+---+---+---+list
;;                                       |0A |1A |2A |3A |4A |------> val1
;;  +---+   +---+list input      A---->  +---+---+---+---+---+
;; --0'------0"--------+        /
;;  +---+   +---+      |---> f*   [ (f n' n" ...) -> produces X values ]
;;  |1' |   |1" | - - -+        \
;;  +---+   +---+                B---->  +---+---+---+---+---+list
;;  |2' |   |2" | - - -          ...     |0B |1B |2B |3B |4B |------> val2
;;  +---+   +---+                X       +---+---+---+---+---+
;;  |3' |   |3" | - -
;;  +---+   +---+
;;  |4' |   |4" | -
;;  +---+   +---+
(define (demux f lis1 . lists)
  (if (pair? lists)
      (let ((all-ls (cons lis1 lists)))
        (let recur ((ls all-ls))
          (receive (hs ts)
                   (cars+cdrs ls)
                   (if (null? hs)
                       (list->values (make-list (values-length (apply f all-ls)) '()))
                       (call-with-values
                           (lambda () (recur ts))
                         (lambda tails
                           (call-with-values
                               (lambda () (apply f hs))
                             (lambda produced-vals
                               (list->values
                                (map (lambda (p t) (cons p t)) produced-vals tails))))))))))
      (let recur ((l lis1))             ; faster
        (if (null? l)
            (list->values (make-list (values-length (f (car lis1))) '()))
            (let ((h (car l)))
              (call-with-values
                  (lambda () (recur (cdr l)))
                (lambda tails
                  (call-with-values
                      (lambda () (f h))
                    (lambda produced-vals
                      (list->values
                       (map (lambda (p t) (cons p t)) produced-vals tails)))))))))))

;;! pair-map applies map to the entire sublist, unlike map, which applies to the head
(define (pair-map f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(receive (cars cdrs) (cars+cdrs lists)
                 (if (pair? cars)
                     (let ((x (apply f lists)))
                       (cons x (recur cdrs)))
                     '())))
      (let recur ((lis lis1))
	(if (null? lis) lis
	    (cons (f lis) (recur (cdr lis)))))))

;;! map+fold combines them two, returning the map and the fold
;; (map+fold (lambda (a b) (values (+ a b) (+ b 1))) 0 '(1 2 3 4))
;; => (1 3 5 7)
;;    3
(define (map+fold kons knil lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists))
                  (fold-ans knil))
        (receive (cars cdrs) (cars+cdrs lists)
                 (if (null? cars)
                     (values '() fold-ans)
                     (receive (mapv foldv)
                              (apply kons (snoc cars fold-ans))
                              (receive (map-next fold-next)
                                       (recur cdrs foldv)
                                       (values (cons mapv map-next)
                                               fold-next))))))
      (let recur ((l lis1)
                  (fold-ans knil))
        (if (pair? l)
            (receive (lh lt) (car+cdr l)
                     (receive (mapv foldv)
                              (kons lh fold-ans)
                              (receive (map-next fold-next)
                                       (recur lt foldv)
                                       (values (cons mapv map-next)
                                               fold-next))))
            (values '() fold-ans)))))

;;! map-fold combines them two, maps values but also accumulates as fold, so that value can be
;; used inside the map-fold computation
;; (map-fold (lambda (a b) (values (+ a b) (+ b 1))) 0 '(1 2 3 4))
;; (1 3 5 7)
(define (map-fold kons knil lis1 . lists) ; OPTIMIZE: test if better than fold+map extraction
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists))
                  (fold-ans knil))
        (receive (cars cdrs) (cars+cdrs lists)
                 (if (null? cars)
                     '()
                     (receive (mapv foldv)
                              (apply kons (snoc cars fold-ans))
                              (cons mapv (recur cdrs foldv))))))
      (let recur ((l lis1)              ; Fast path for
                  (fold-ans knil))
        (if (null? l)
            '()
            (receive (lh lt) (car+cdr l)
                     (receive (mapv foldv)
                              (kons lh fold-ans)
                              (cons mapv (recur lt foldv))))))))

;;! Like pair-fold, but stops folding when the cdr is of a given length
(define (pair-fold-x x kons knil lists)
  (pair-fold
   (lambda args
     (let ((rev-args (reverse args)))
       (if (any (lambda (e) (< (length e) x)) (cdr rev-args))
           (last args)
           (receive (cars cdrs)
                    (cars+cdrs (reverse (cdr rev-args)))
                    (apply kons (append (map (lambda (a b) (cons a b)) cars cdrs)
                                        (list (car rev-args))))))))
   knil
   lists))

;;! pair-fold-x specialization for x=2
(define pair-fold-2
  (lambda (kons knil lists)
    (pair-fold-x 2 kons knil lists)))

;;! AND applied over all elements of the resulting list
(define andmap
  (lambda (f first . rest)
    (or (null? first)
        (if (null? rest)
            (let andmap ((first first))
              (let ((x (car first)) (first (cdr first)))
                (if (null? first)
                    (f x)
                    (and (f x) (andmap first)))))
            (let andmap ((first first) (rest rest))
              (let ((x (car first))
                    (xr (map car rest))
                    (first (cdr first))
                    (rest (map cdr rest)))
                (if (null? first)
                    (apply f (cons x xr))
                    (and (apply f (cons x xr)) (andmap first rest)))))))))

;;! OR applied over all elements of the resulting list
(define ormap
  (lambda (proc list1)
    (and (not (null? list1))
         (or (proc (car list1)) (ormap proc (cdr list1))))))


;;-------------------------------------------------------------------------------
;;!! Find, remove, substitute, insert

;;! Insert given an index
(define (insert-at new k lst)
  (let recur ((lst lst) (k k))
    (cond ((zero? k)
           (cons new lst))
          ((null? lst) '())
          (else
           (cons (car lst)
                 (recur (cdr lst)
                        (- k 1)))))))

;;! Insert at the left side of the first element that satisfies the predicate
(define (insert-left-first new pred lst)
  (let recur ((lst lst))
    (if (null? lst)
        '()
        (let ((head (car lst)))
          (if (pred head)
              (cons new
                    (cons head
                          (cdr lst)))
              (cons head
                    (recur (cdr lst))))))))

;;! Insert at the left side of each element that satisfies the predicate
(define (insert-left new pred lst)
  (let recur ((lst lst))
    (if (null? lst)
        '()
        (let ((head (car lst)))
          (if (pred head)
              (cons new
                    (cons head
                          (recur (cdr lst))))
              (cons head
                    (recur (cdr lst))))))))

;;! Insert at the left side of each element that satisfies the predicate
;; (recursively)
(define (insert-left* new pred lst)
  (let recur ((lst lst))
    (if (null? lst)
        '()
        (let ((head (car lst)))
          (if (pair? head)
              (cons (recur head)
                    (recur (cdr lst)))
              (if (pred head)
                  (cons new
                        (cons head
                              (recur (cdr lst))))
                  (cons head
                        (recur (cdr lst)))))))))

;;! Insert at the right side of the first element that satisfies the predicate
(define (insert-right-first new pred lst)
  (let recur ((lst lst))
    (if (null? lst)
        '()
        (let ((head (car lst)))
          (if (pred head)
              (cons head
                    (cons new
                          (cdr lst)))
              (cons head
                    (recur (cdr lst))))))))

;;! Insert at the right side of an element that satisfies the predicate
(define (insert-right new pred lst)
  (let recur ((lst lst))
    (if (null? lst)
        '()
        (let ((head (car lst)))
          (if (pred head)
              (cons head
                    (cons new
                          (recur (cdr lst))))
              (cons head
                    (recur (cdr lst))))))))

;;! Insert at the right side of each element that satisfies the predicate
;; (recursively)
(define (insert-right* new pred lst)
  (let recur ((lst lst))
    (if (null? lst)
        '()
        (let ((head (car lst)))
          (if (pair? head)
              (cons (recur head)
                    (recur (cdr lst)))
              (if (pred head)
                  (cons head
                        (cons new
                              (recur (cdr lst))))
                  (cons head
                        (recur (cdr lst)))))))))

;;! Insert between each element
(define (intersperse lst sep)
  (cond
   ((null? lst) '())
   ((null? (cdr lst)) lst)
   (else (cons (car lst)
               (cons sep
                     (intersperse (cdr lst) sep))))))

;;! Remove element given an index
(define (remove-at k lst)
  (let recur ((lst lst) (k k))
    (cond ((null? lst) '())
          ((zero? k) (cdr lst))
          (else
           (cons (car lst)
                 (recur (cdr lst)
                        (- k 1)))))))

;;! Remove first instance
(define (remove-first pred l)
  ((letrec ((R (lambda (l)
                 (cond
                  ((null? l) '())
                  ((pred (car l)) (cdr l))
                  (else (cons (car l)
                              (R (cdr l)))))))) R) l))

;;! Remove if the predicate is satisfied with any element given in a list
(define (remove-any any-pred any-lis lis)
  (remove (lambda (e)
            (any (lambda (a) (any-pred a e)) any-lis))
          lis))

;;! Remove if the predicate is satisfied with every element given in a list
(define (remove-every every-pred every-lis lis)
  (remove (lambda (x)
            (every (lambda (e) (every-pred e x)) every-lis))
          lis))

;;! Try to find an element and remove it, yields #f if not found
(define (find-remove pred lis)
  (let/cc
   failed
   ((letrec ((R (lambda (l)
                  (if (null? l)
                      (failed #f)
                      (receive (h t) (car+cdr l)
                               (if (pred h)
                                   t
                                   (cons h (R t)))))))) R) lis)))

;;! Try to find an element, yielding #f if not found. It returns both the element
;; and the list with that element removed
(define (find+remove pred lis)
  (let/cc
   failed
   ((letrec ((R (lambda (l)
                  (if (null? l)
                      (failed #f lis)
                      (receive (h t) (car+cdr l)
                               (if (pred h)
                                   (values h t)
                                   (receive (newhead newtail)
                                            (R t)
                                            (values newhead
                                                    (cons h newtail))))))))) R) lis)))

;;! Rotates the list until the first one satisfies the predicate
(define (find-rotate pred lis)
  (define (iter lis-iter n)
    (let ((x (car lis-iter))
          (l (length lis)))
      (cond
       ((= n l) #f)
       ((pred x) lis-iter)
       (else
        (iter (append (cdr lis-iter) (list x)) (+ n 1))))))
  (iter lis 0))

;;! Find the element that satisfies the predicate against all the other elements
(define (most pred lis)
  (reduce (lambda (a b) (if (pred b a) b a)) #f lis))

;;! Most, but return also the list with that element removed
;; TODO: Benchmark and pick best version
;; (define (most+remove pred lis)
;;   (let ((res (most pred lis)))
;;     (values res
;;             (remove-first res lis))))
(define (most+remove pred lis)
  (let recur ((l lis)
              (list-common '())
              (list-rembered '())
              (ans (car lis)))
    (if (null? l)
        (values ans
                (append! list-common (cdr list-rembered)))
        (receive (h t) (car+cdr l)
                 (if (pred h ans)
                     (recur t
                            (append! list-common list-rembered)
                            (list h)
                            h)
                     (recur t
                            list-common
                            (append! list-rembered (list h))
                            ans))))))

;;! MOST using a generator instead of a comparator predicate
(define (most/generator generator comparator lis)
  (let iter ((ans (car lis))
             (current-max (generator (car lis)))
             (rest (cdr lis)))
    (if (null? rest)
        ans
        (receive (h t)
                 (car+cdr rest)
                 (let ((val (generator h)))
                   (if (comparator val current-max)
                       (iter h val t)
                       (iter ans current-max t)))))))

;; MAX/MIN standard functions with a comparable number generator. Similar to MOST,
;; but compares the numbers generated

;;! max/generator
(define (max/generator generator lis)
  (most/generator generator > lis))

;;! max/generator
(define (min/generator generator lis)
  (most/generator generator < lis))

;;! Substitution in a list (only first element)
(define (x-substitute maker pred? new l)
  ((letrec ((X (lambda (l)
                 (if (null? l)
                     '()
                     (receive (lcar lcdr) (car+cdr l)
                              (if (pred? lcar)
                                  (maker new (X lcdr))
                                  (cons lcar (X lcdr)))))))) X) l))

;;! substitute-first
(define substitute-first (lambda (pred? new l) (x-substitute cons pred? new l)))

;;! Substitution in a list (all elements)
(define substitute (lambda (pred? new l) (x-substitute append pred? new l)))

;;! Recursive substitution in a list, down to atom-level
(define (x-subst* maker old new l)
  ((letrec ((X (lambda (l)
                 (if (null? l)
                     '()
                     (receive (lcar lcdr) (car+cdr l)
                              (cond
                               ((atom? lcar) ; Atoms level
                                (cond
                                 ((eq? lcar old)
                                  (maker new
                                         (X lcdr)))
                                 (else
                                  (cons lcar
                                        (X lcdr)))))
                               ((equal? lcar old) ; Sublist level
                                (maker new (X lcdr)))
                               (else
                                (cons
                                 (X lcar)
                                 (X lcdr))))))))) X) l))
(define substitute-first* (lambda (old new l) (x-subst* cons old new l)))

;;! Recursive substitution with multiple insertion, down to atom-level
(define substitute* (lambda (old new l) (x-subst* append old new l)))


;;-------------------------------------------------------------------------------
;;!! Skeleton/shape

;;! Flatten a list (optimized)
;; http://schemecookbook.org/Cookbook/ListFlatten
;; TODO: benchmark
;; (define (flatten x)
;;   (cond
;;    ((null? x) '())
;;    ((not (pair? x)) (list x))
;;    (else (append (flatten (car x))
;;                  (flatten (cdr x))))))
(define (flatten x:xs)
  (let* ((result (cons '() '())) (last-elt result))
    (define (f x:xs)
      (cond
       ((null? x:xs)
        result)
       ((pair? (car x:xs))
        (f (car x:xs)) (f (cdr x:xs)))
       (else
        (set-cdr! last-elt (cons (car x:xs) '()))
        (set! last-elt (cdr last-elt))
        (f (cdr x:xs)))))
    (f x:xs)
    (cdr result)))

;;! Fast flatten, that doesn't respect ordering
(define (flatten-unordered x:xs)
  (define (f x:xs result)
    (cond
     ((null? x:xs)
      result)
     ((pair? (car x:xs))
      (f (cdr x:xs) (f (car x:xs) result)))
     (else
      (f (cdr x:xs) (cons (car x:xs) result)))))
  (f x:xs '()))

;;! Flatten only forms with a specific head used as tag
;; Example:
;; (flatten-tag '##begin '(##begin 1 2 3 (##begin 4 5 6) (7) (##begin 8 (9))))
;; => (1 2 3 4 5 6 (7) 8 (9))
(define (flatten-tag tag lst)
  (let recur ((lst (if (and (not (null? lst)) (eq? (car lst) tag))
                       (cdr lst)
                       lst)))
    (cond
     ((null? lst)
      '())
     ((and (pair? (car lst)) (eq? tag (caar lst)))
      (append (recur (cdar lst)) (recur (cdr lst))))
     ((pair? (car lst))
      (cons (recur (car lst)) (recur (cdr lst))))
     (else
      (cons (car lst) (recur (cdr lst)))))))

;;! Make a structure analysis of a list
(define (list->skeleton l)
  ((letrec ((S (lambda (l n)
                 (cond
                  ((null? l)
                   (if (= n 0) '() (list n)))
                  ((list? (car l))
                   (if (= n 0)
                       (cons (S (car l) 0) (S (cdr l) 0))
                       (cons n (cons (S (car l) 0) (S (cdr l) 0)))))
                  (else
                   (S (cdr l) (+ 1 n)))))))
     S) l 0))

;;! Expand a skeleton into a list with stub positions
;; (define (expand-skeleton s) ; TODO Benchmark!
;;   ((letrec ((E (lambda (s)
;;                  (cond
;;                   ((null? s) '())
;;                   ((list? (car s))
;;                    (cons (E (car s)) (E (cdr s))))
;;                   (else
;;                    (append (make-list (car s) (car s))
;;                            (E (cdr s)))))))) E) s))
(define (expand-skeleton s)
  ((letrec ((E (lambda (s)
                 (cond
                  ((null? s) '())
                  ((list? (car s))
                   (cons (E (car s))
                         (E (cdr s))))
                  (else
                   (if (= (car s) 1)
                       (cons #t (E (cdr s)))
                       (cons #t (E (cons (- (car s) 1) (cdr s)))))))))) E) s))

;;! Make a flat list fit into a skeleton
;; TODO: remove ticker, do with receive/values
(define (apply-skeleton s l)
  ((letrec ((next (ticker! l))
            (E (lambda (s)
                 (cond
                  ((null? s) '())
                  ((list? (car s))
                   (cons (E (car s))
                         (E (cdr s))))
                  (else
                   (if (= (car s) 1)
                       (cons (next) (E (cdr s)))
                       (cons (next) (E (cons (- (car s) 1) (cdr s)))))))))) E) s))


;;-------------------------------------------------------------------------------
;;!! Fragmentation

;;! Return a sublist from a start to an end positions
(define (slice l start end)
  (take (drop l start)
        (- end start)))

(define (slice! l start end)
  (take! (drop l start)
         (- end start)))

;;! Return two lists of lengths differing with at most one
;; TODO: currently reverses first list
(define (split-in-halves l)
  (let loop ((front '())
             (slow  l)
             (fast  l))
    (cond
     ((null? fast)
      (values front
              slow))
     ((null? (cdr fast))
      (values (cons (car slow) front)
              (cdr slow)))
     (else
      (loop (cons (car slow) front)
            (cdr slow)
            (cddr fast))))))

;;! split-in-halves!
(define (split-in-halves! l)
  (let loop ((slow (cons 'foo l))
             (fast (cons 'bar l)))
    (cond
     ((or (null? fast)
          (null? (cdr fast)))
      (let ((back (cdr slow)))
        (set-cdr! slow '())
        (values l back)))
     (else
      (loop (cdr slow)
            (cddr fast))))))

;;! partition a list depending on predicate satisfaction, effectively extending
;; the SRFI-1 |partition| procedure
;; Returns a list of lists
;; (classify '(0 1 0 4 2 3 6) zero? odd? (lambda (x) (= x 4)))
;;   => ((0 0) (3 1) (4) (6))
(define (classify lst . predicates)
  (fold (lambda args
          (let ((e (car args)))
            (let recur ((p predicates)
                        (visited-classes '())
                        (unvisited-classes (cadr args)))
              (if (or (null? p)
                      ((car p) e))
                  (append visited-classes
                          (list (cons e (car unvisited-classes)))
                          (cdr unvisited-classes))
                  (recur (cdr p)
                         (append visited-classes (list (car unvisited-classes)))
                         (cdr unvisited-classes))))))
        (make-list (+ (length predicates) 1) '())
        lst))

(define (classify-ordered lst . predicates)
  (map reverse (apply classify lst predicates)))


;;-------------------------------------------------------------------------------
;;!! Grouping/ungrouping

;;! Construct a new list containing each element repeated a number of times
;; '(a b c) 2 -> '(a a a b b b c c c)
(define (replicate n lst)
  (let recur ((lst lst))
    (if (null? lst)
        '()
        (let ((head (car lst)))
          (cons head
                (let repeat ((k n))
                  (if (zero? k)
                      (recur (cdr lst))
                      (cons head
                            (repeat (- k 1))))))))))

;;! Makes groups of equal elements
;; '(a a a a b b b b c c d d d) -> '((a a a a) (b b b b) (c c) (d d d))
(define (pack l)
  (error "Not implemented"))

;;! Make n groups from a list. If not divisible, the last is smaller.
(define (n-groups n l)
  (error "Not implemented"))
  
;;! Makes a number of groups with size n from a list. If not divisible, the last
;; is smaller. If a distribution is given as a list of integers, groups are made
;; with those numbers of elements per group. If not an exact distribution is
;; given, the last group is bigger or smaller than the distribution indicates
(define (group n/s l)
  (error "Not implemented"))


;;-------------------------------------------------------------------------------
;;!! Miscellaneous

;;! Creates a destructive function to read a list sequantially after each call
(define (ticker! l)
  (lambda ()
    (begin0 (car l)
            (set! l (cdr l)))))
