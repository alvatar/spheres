;;;;
;;;; Ludovic Courtès <ludo@gnu.org>
;;;;
;;;; 	Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.

(use vlist test srfi-69)


(test-group "vlist"

  (test-assert "vlist?"
    (and (vlist? vlist-null)
         (vlist? (vlist-cons 'a vlist-null))))

  (test-assert "vlist-null?"
    (vlist-null? vlist-null))

  (test-assert "vlist-cons"
    (let* ((v1 (vlist-cons 1 vlist-null))
           (v2 (vlist-cons 2 v1))
           (v3 (vlist-cons 3 v2))
           (v4 (vlist-cons 4 v3)))
      (every vlist? (list v1 v2 v3 v4))))

  (test-assert "vlist-head"
    (let* ((v1 (vlist-cons 1 vlist-null))
           (v2 (vlist-cons 2 v1))
           (v3 (vlist-cons 3 v2))
           (v4 (vlist-cons 4 v3)))
      (equal? (map vlist-head (list v1 v2 v3 v4))
              '(1 2 3 4))))

  (test-assert "vlist-tail"
    (let* ((v1 (vlist-cons 1 vlist-null))
           (v2 (vlist-cons 2 v1))
           (v3 (vlist-cons 3 v2))
           (v4 (vlist-cons 4 v3)))
      (equal? (map vlist-head
                   (map vlist-tail (list v2 v3 v4)))
              '(1 2 3))))

  (test-assert "vlist->list"
    (let* ((v1 (vlist-cons 1 vlist-null))
           (v2 (vlist-cons 2 v1))
           (v3 (vlist-cons 3 v2))
           (v4 (vlist-cons 4 v3)))
      (equal? '(4 3 2 1)
              (vlist->list v4))))

  (test-assert "list->vlist"
    (equal? (vlist->list (list->vlist '(1 2 3 4 5)))
            '(1 2 3 4 5)))

  (test-assert "vlist-drop"
    (equal? (vlist->list (vlist-drop (list->vlist (iota 77)) 7))
            (drop (iota 77) 7)))

  (test-assert "vlist-cons2"
    ;; Example from Bagwell's paper, Figure 2.
    (let* ((top  (list->vlist '(8 7 6 5 4 3)))
           (part (vlist-tail (vlist-tail top)))
           (test (vlist-cons 9 part)))
      (equal? (vlist->list test)
              '(9 6 5 4 3))))

  (test-assert "vlist-cons3"
    (let ((vlst (vlist-cons 'a
                            (vlist-cons 'b
                                        (vlist-drop (list->vlist (iota 5))
                                                    3)))))
      (equal? (vlist->list vlst)
              '(a b 3 4))))

  (test-assert "vlist-map"
    (equal? (vlist->list (vlist-map (lambda (x) (+ 1 x)) (list->vlist '(1 2 3 4 5))))
            '(2 3 4 5 6)))

  (test-assert "vlist-length"
    (= (vlist-length (list->vlist (iota 77)))
       77))

  (test-assert "vlist-length complex"
    (= (vlist-length (fold vlist-cons
                           (vlist-drop (list->vlist (iota 77)) 33)
                           (iota (- 33 7))))
       70))

  (test-assert "vlist-ref"
    (let* ((indices (iota 111))
           (vlst    (list->vlist indices)))
      (equal? (map (lambda (i)
                     (vlist-ref vlst i))
                   indices)
              indices)))

  (test-assert "vlist-ref degenerate"
    ;; Degenerate case where VLST contains only 1-element blocks.
    (let* ((indices (iota 111))
           (vlst    (fold (lambda (i vl)
                            (let ((vl (vlist-cons 'x vl)))
                              (vlist-cons i (vlist-tail vl))))
                          vlist-null
                          indices)))
      (equal? (map (lambda (i)
                     (vlist-ref vlst i))
                   (reverse indices))
              indices)))

  (test-assert "vlist-filter"
    (let* ((lst  (iota 33))
           (vlst (fold-right vlist-cons vlist-null lst)))
      (equal? (vlist->list (vlist-filter even? vlst))
              (filter even? lst))))

  (test-assert "vlist-delete"
    (let* ((lst  '(a b c d e))
           (vlst (fold-right vlist-cons vlist-null lst)))
      (equal? (vlist->list (vlist-delete 'c vlst))
              (delete 'c lst))))

  (test-assert "vlist-take"
    (let* ((lst  (iota 77))
           (vlst (fold-right vlist-cons vlist-null lst)))
      (equal? (vlist->list (vlist-take vlst 44))
              (take lst 44))))

  (test-assert "vlist-unfold"
    (let ((results (map (lambda (unfold)
                          (unfold (lambda (i) (> i 100))
                                  (lambda (i) i)
                                  (lambda (i) (+ i 1))
                                  0))
                        (list unfold vlist-unfold))))
      (equal? (car results)
              (vlist->list (cadr results)))))

  (test-assert "vlist-append"
    (let* ((lists '((a) (b c) (d e f) (g)))
           (vlst  (apply vlist-append (map list->vlist lists)))
           (lst   (apply append lists)))
      (equal? lst (vlist->list vlst))))
)


(test-group "vhash"

  (test-assert "vhash?"
    (vhash? (vhash-cons "hello" "world" vlist-null)))

  (test-assert "vhash-assoc vlist-null"
    (not (vhash-assq 'a vlist-null)))

  (test-assert "vhash-assoc simple"
    (let ((vh (vhash-cons "hello" "world" vlist-null)))
      (equal? (cons "hello" "world")
              (vhash-assoc "hello" vh))))

  (test-assert "vhash-assoc regular"
    (let* ((keys   '(a b c d e f g h i))
           (values '(1 2 3 4 5 6 7 8 9))
           (vh     (fold vhash-cons vlist-null keys values)))
      (fold (lambda (k v result)
              (and result
                   (equal? (cons k v)
                           (vhash-assoc k vh eq?))))
            #t
            keys
            values)))

  (test-assert "vhash-assoc tail"
    (let* ((keys   '(a b c d e f g h i))
           (values '(1 2 3 4 5 6 7 8 9))
           (vh1    (fold vhash-consq vlist-null keys values))
           (vh2    (vhash-consq 'x 'x (vlist-tail vh1))))
      (and (fold (lambda (k v result)
                   (and result
                        (equal? (cons k v)
                                (vhash-assq k vh2))))
                 #t
                 (cons 'x (delete 'i keys eq?))
                 (cons 'x (delete 9 values eqv?)))
           (not (vhash-assq 'i  vh2)))))

  (test-assert "vhash-assoc degenerate"
    (let* ((keys   '(a b c d e f g h i))
           (values '(1 2 3 4 5 6 7 8 9))
           (vh     (fold (lambda (k v vh)
                           ;; Degenerate case where VH2 contains only
                           ;; 1-element blocks.
                           (let* ((vh1 (vhash-cons 'x 'x vh))
                                  (vh2 (vlist-tail vh1)))
                             (vhash-cons k v vh2)))
                         vlist-null keys values)))
      (and (fold (lambda (k v result)
                   (and result
                        (equal? (cons k v)
                                (vhash-assoc k vh))))
                 #t
                 keys
                 values)
           (not (vhash-assoc 'x vh)))))

  (test-assert "vhash as vlist"
    (let* ((keys   '(a b c d e f g h i))
           (values '(1 2 3 4 5 6 7 8 9))
           (vh     (fold vhash-cons vlist-null keys values))
           (alist  (fold alist-cons '() keys values)))
      (and (equal? (vlist->list vh) alist)
           (= (length alist) (vlist-length vh))
           (fold (lambda (i result)
                   (and result
                        (equal? (list-ref alist i)
                                (vlist-ref vh i))))
                 #t
                 (iota (vlist-length vh))))))

  (test-assert "vhash entry shadowed"
    (let* ((a (vhash-consq 'a 1 vlist-null))
           (b (vhash-consq 'a 2 a)))
      (and (= 1 (cdr (vhash-assq 'a a)))
           (= 2 (cdr (vhash-assq 'a b)))
           (= 1 (cdr (vhash-assq 'a (vlist-tail b)))))))

  (test-assert "vlist-filter"
    (let* ((keys   '(a b c d e f g h i))
           (values '(1 2 3 4 5 6 7 8 9))
           (vh     (fold vhash-cons vlist-null keys values))
           (alist  (fold alist-cons '() keys values))
           (pred   (lambda (k+v)
                     (case (car k+v)
                       ((c f) #f)
                       (else  #t)))))
      (let ((vh    (vlist-filter pred vh))
            (alist (filter pred alist)))
        (and (equal? (vlist->list vh) alist)
             (= (length alist) (vlist-length vh))
             (fold (lambda (i result)
                     (and result
                          (equal? (list-ref alist i)
                                  (vlist-ref vh i))))
                   #t
                   (iota (vlist-length vh)))))))

  (test-assert "vhash-delete"
    (let* ((keys   '(a b c d e f g d h i))
           (values '(1 2 3 4 5 6 7 0 8 9))
           (vh     (fold vhash-cons vlist-null keys values))
           (alist  (fold alist-cons '() keys values)))
      (let ((vh    (vhash-delete 'd vh))
            (alist (alist-delete 'd alist)))
        (and (= (length alist) (vlist-length vh))
             (fold (lambda (k result)
                     (and result
                          (equal? (assq k alist)
                                  (vhash-assoc k vh eq?))))
                   #t
                   keys)))))

  (test-assert "vhash-delete honors HASH"
    ;; In 2.0.0, `vhash-delete' would construct a new vhash without
    ;; using the supplied hash procedure, which could lead to
    ;; inconsistencies.
    (let* ((s  "hello")
           (vh (fold vhash-consq
                     (vhash-consq s "world" vlist-null)
                     (iota 300)
                     (iota 300))))
      (and (vhash-assq s vh)
           (pair? (vhash-assq s (vhash-delete 123 vh eq? eq?-hash))))))

  (test-assert "vhash-fold"
    (let* ((keys   '(a b c d e f g d h i))
           (values '(1 2 3 4 5 6 7 0 8 9))
           (vh     (fold vhash-cons vlist-null keys values))
           (alist  (fold alist-cons '() keys values)))
      (equal? alist (reverse (vhash-fold alist-cons '() vh)))))

  (test-assert "vhash-fold-right"
    (let* ((keys   '(a b c d e f g d h i))
           (values '(1 2 3 4 5 6 7 0 8 9))
           (vh     (fold vhash-cons vlist-null keys values))
           (alist  (fold alist-cons '() keys values)))
      (equal? alist (vhash-fold-right alist-cons '() vh))))

  (test-assert "alist->vhash"
    (let* ((keys   '(a b c d e f g d h i))
           (values '(1 2 3 4 5 6 7 0 8 9))
           (alist  (fold alist-cons '() keys values))
           (vh     (alist->vhash alist))
           (alist2 (vlist-fold cons '() vh)))
      (and (equal? alist (reverse alist2))
           (fold (lambda (k result)
                   (and result
                        (equal? (assq k alist)
                                (vhash-assoc k vh eq?))))
                 #t
                 keys))))

  (test-assert "vhash-fold*"
    (let* ((keys   (make-list 10 'a))
           (values (iota 10))
           (vh     (fold vhash-cons vlist-null keys values)))
      (equal? (vhash-fold* cons '() 'a vh)
              values)))

  (test-assert "vhash-fold* tail"
    (let* ((keys   (make-list 100 'a))
           (values (iota 100))
           (vh     (fold vhash-cons vlist-null keys values)))
      (equal? (vhash-fold* cons '() 'a (vlist-drop vh 42))
              (take values (- 100 42)))))

  (test-assert "vhash-fold* interleaved"
    (let* ((keys   '(a b a b a b a b a b c d e a b))
           (values '(1 0 2 0 3 0 4 0 5 0 0 0 0 6 0))
           (vh     (fold vhash-cons vlist-null keys values)))
      (equal? (vhash-fold* cons '() 'a vh)
              (filter (cut > <> 0) values))))

  (test-assert "vhash-foldq* degenerate"
    (let* ((keys   '(a b a b a a a b a b a a a z))
           (values '(1 0 2 0 3 4 5 0 6 0 7 8 9 0))
           (vh     (fold (lambda (k v vh)
                           ;; Degenerate case where VH2 contains only
                           ;; 1-element blocks.
                           (let* ((vh1 (vhash-consq 'x 'x vh))
                                  (vh2 (vlist-tail vh1)))
                             (vhash-consq k v vh2)))
                         vlist-null keys values)))
      (equal? (vhash-foldq* cons '() 'a vh)
              (filter (cut > <> 0) values))))
)

(test-exit)
