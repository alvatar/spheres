(require-library random-access-lists simple-tests dbc)

(import chicken random-access-lists simple-tests dbc)

(contract-check-level 2)

(compound-test (random-access-lists)
  (define-test (ra-list)
    (check
      "AN EMPTY RAL OF INTEGERS"
      (define ls (make-ral fixnum?))
      (ral? ls)
      (ral-null? ls)
      (fx= (ral-height ls) 1)
      "POPULATE IT AT THE RIGHT END"
      (ral-add! ls 0 1 2 3 4)
      (fx= (ral-count ls) 5)
      (equal? (ral->list ls) '(0 1 2 3 4))
      "REMOVE SOME ITEMS"
      (ral-remove! ls 2)
      (fx= (ral-count ls) 4)
      (equal? (ral->list ls) '(0 1 3 4))
      (ral-remove! ls (fx- (ral-count ls) 1))
      (fx= (ral-count ls) 3)
      (equal? (ral->list ls) '(0 1 3))
      (ral-remove! ls 0)
      (fx= (ral-count ls) 2)
      (equal? (ral->list ls) '(1 3))
      "INSERT AN ITEM"
      (ral-insert! ls 1 2)
      (fx= (ral-ref ls 1) 2)
      (fx= (ral-count ls) 3)
      (equal? (ral->list ls) '(1 2 3))
      "RESET RAL"
      (ral-clear! ls)
      (ral-null? ls)
      "POPULATE RAL AGAIN"
      (do ((k 0 (fx+ 1 k)))
        ((fx= k 100))
        (ral-add! ls k))
      (fx= (ral-count ls) 100)
      "SPLIT, JOIN AND SUBRAL"
      (ral-eql? fx=
                ls
                (receive (head tail) (ral-split ls 50)
                  (ral-join head tail)))
      (equal?
        (ral->list (ral-from-upto ls 20 70))
        (let loop ((k 69) (result '()))
          (if (fx= k 19)
            result
            (loop (fx- k 1) (cons k result)))))
      "INSPECT AND CHANGE AN ITEM IN THE MIDDLE"
      (fx= (ral-ref ls 50) 50)
      (ral-set! ls 50 500)
      (fx= (ral-ref ls 50) 500)
      "CHANGE ITEM BACK AGAIN"
      (ral-set! ls 50 50)
      (fx= (ral-ref ls 50) 50)
      "CHANGE ITEMS AT THE ENDS AND BACK AGAIN"
      (ral-set! ls 0 1000)
      (fx= (ral-ref ls 0) 1000)
      (ral-set! ls 0 0)
      (fx= (ral-ref ls 0) 0)
      (ral-set! ls 99 1000)
      (fx= (ral-ref ls 99) 1000)
      (ral-set! ls 99 99)
      (fx= (ral-ref ls 99) 99)
      "INSERT AT LEFT END"
      (ral-add-left! ls -1 -2 -3)
      (fx= (ral-ref ls 0) -3)
      (fx= (ral-ref ls 1) -2)
      (fx= (ral-ref ls 2) -1)
      "REMOVE THEM AGAIN"
      (ral-remove! ls 0)
      (ral-remove! ls 0)
      (ral-remove! ls 0)
      (fx= (ral-ref ls 0) 0)
      (fx= (ral-count ls) 100)
      "INSERT AT RIGHT END AND REMOVE IT AGAIN"
      (ral-add! ls 100 101)
      (fx= (ral-ref ls (fx- (ral-count ls) 1)) 101)
      (ral-remove! ls (fx- (ral-count ls) 1))
      (ral-remove! ls (fx- (ral-count ls) 1))
      (fx= (ral-ref ls (fx- (ral-count ls) 1)) 99)
      "INSERT IN THE MIDDLE AND REMOVE IT AGAIN"
      (ral-insert! ls 20 200)
      (fx= (ral-ref ls 20) 200)
      (fx= (ral-ref ls 21) 20)
      (fx= (ral-count ls) 101)
      (ral-remove! ls 20)
      (fx= (ral-ref ls 20) 20)
      (fx= (ral-count ls) 100)
      "RESTRUCTURE"
      (define lsr (ral-restructure ls 4 20))
      (ral-eql? fx= ls lsr)
      (fx= (ral-width lsr) 4)
      (fx= (ral-max-height lsr) 20)
      "MAP AND FILTER"
      (equal? (ral->list (ral-map ls add1))
              (let loop ((k 100) (result '()))
                (if (fx= k 0)
                  result
                  (loop (fx- k 1) (cons k result)))))
      (equal? (ral->list (ral-filter ls odd?))
              (let loop ((k 99) (result '()))
                (if (fx< k 0)
                  result
                  (loop (fx- k 2) (cons k result)))))
    ))
    (ra-list)
  )

