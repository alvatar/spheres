(define C (make-lru-cache 4 string=?
                          (lambda (k v) (println "deleting " k v))))
(lru-cache-set! C "a" 1) ; a
(lru-cache-set! C "b" 2) ; b a
(lru-cache-set! C "c" 3) ; c b a
(lru-cache-set! C "d" 4) ; d c b a
(lru-cache-walk C println)
;; d4
;; c3
;; b2
;; a1
(lru-cache-set! C "e" 5) ; e d c b
;; deleting (a 1)
(lru-cache-ref C "b")    ; 2, b e d c
(lru-cache-ref C "d")    ; 4, d b e c
(lru-cache-walk C println)
;; d4
;; b2
;; e5
;; c3
(lru-cache-delete! C "e") ; d b c
;; deleting ("e" 5)
(lru-cache-set! C "a" 6) ; a d b c
(lru-cache-flush! C)
;; deleting ("a" 6)
;; deleting ("d" 4)
;; deleting ("b" 2)
;; deleting ("c" 3)
(lru-cache-walk C println)
