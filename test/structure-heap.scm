(use heap test)

(define (figure-6.2)
  (let ((heap (make-max-heap)))
    (for-each (lambda (i) (heap-insert! heap i i))
      '(16 4 10 14 7 9 3 2 8 1))
    heap))

(let ((heap (figure-6.2)))
  (let iter ((data '()))
    (if (heap-empty? heap)
        (test
         "Heap data" 
         data
         '(1 2 3 4 7 8 9 10 14 16))
        (iter
         (cons (heap-extract-extremum! heap) data)))))

(let ((heap (figure-6.2)))
  (test "Heap-extremum"
        16
        (heap-extremum heap))
  (test "Heap-extract-extremum!"
        16
        (heap-extract-extremum! heap))
  (test "Next heap-extremum"
        14
        (heap-extremum heap)))

(define (figure-6.5)
  (let ((heap (make-max-heap)))
    (for-each (lambda (i) (heap-insert! heap i i))
      '(16 14 10 8 7 9 3 2 4 1))
    heap))

(let ((heap (figure-6.5)))
  (heap-change-key! heap 4 15)
  (let iter ((data '()))
    (if (heap-empty? heap)
        (test
         "Heap-change-key!"
         data
         '(1 2 3 7 8 9 10 14 4 16))
        (iter
         (cons (heap-extract-extremum! heap) data)))))

;; (let ((heap (figure-6.5)))
;;   (heap-delete! heap 8)
;;   (let iter ((data '()))
;;     (if (heap-empty? heap)
;;         (test
;;          "Heap-delete!"
;;          data
;;          '(1 2 3 4 7 9 10 14 16))
;;         (iter
;;          (cons (heap-extract-extremum! heap) data)))))

(let ((heap (make-max-heap)))
  (heap-insert! heap 1 'a)
  (heap-insert! heap 2 'b)
  (heap-insert! heap 3 'c)
  (test-assert "heap-member?" (heap-member? heap 'a))
  (test "heap-key -- exists" 1 (heap-key heap 'a))
  (test "heap-key -- doesn't exist" #f (heap-key heap 'd))
  ;; (heap-delete! heap 'c)
  ;; (test-assert "not-heap-member?" (not (heap-member? heap 'c)))
  )

(let ((heap (figure-6.5)))
  (test "heap->list"
        (heap->list heap)
        '(1 2 3 4 7 8 9 10 14 16)))

(let ((heap (figure-6.5)))
  (test "heap->alist"
        (heap->alist heap)
        '((1 . 1)
          (2 . 2)
          (3 . 3)
          (4 . 4)
          (7 . 7)
          (8 . 8)
          (9 . 9)
          (10 . 10)
          (14 . 14)
          (16 . 16))))

(test-exit)
