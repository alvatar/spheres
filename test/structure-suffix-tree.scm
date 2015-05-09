(use suffix-tree)

(define t (make-suffix-tree char=? string->list ))

(define t1 (suffix-tree-insert "key1" 'test1 t))
(define t2 (suffix-tree-insert "key2" 'test2 t1))

(assert (equal? 'test2 (suffix-tree-lookup "key2" t2)))
(assert (equal? 'test1 (suffix-tree-lookup "key1" t1)))

(define t3 (suffix-tree-lookup/partial  "key" t2))

(assert (equal? 'test1 (suffix-tree-lookup "1" t3)))
(assert (equal? 'test2 (suffix-tree-lookup "2" t3)))

