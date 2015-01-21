(%load-library '(spheres/util test))

(%load-library '(spheres/algorithm list-extra))

(test-begin "list-extra" 12)

(define letters '(a b c d e f g h i))

(test-assert "cars+cdrs"
             (equal?+ (cars+cdrs '((a b c) (1 2 3)))
                      (values '(a 1)
                              '((b c) (2 3)))))

(test-equal "cars"
            (cars '((a b c) (1 2 3)))
            '(a 1))

(test-equal "cdrs"
            (cdrs '((a b c) (1 2 3)))
            '((b c) (2 3)))

(test-equal "list-ref-right"
            (list-ref-right 0 letters)
            'i)

(test-equal "list-ref-right"
            (list-ref-right 1 letters)
            'h)

(test-equal "rotate-left 1"
            (rotate-left 0 letters)
            letters)

(test-equal "rotate-left 2"
            (rotate-left 2 letters)
            '(c d e f g h i a b))

(test-equal "rotate-left 3"
            (rotate-left (length letters) letters)
            letters)

(test-equal "rotate-left 4"
            (rotate-left (+ (length letters) 4) letters)
            (rotate-left 4 letters))

(test-equal "rotate-right 1"
            (rotate-right 0 letters)
            letters)

(test-equal "rotate-right 2"
            (rotate-right 2 letters)
            '(h i a b c d e f g))

(test-equal "rotate-right 3"
            (rotate-right (length letters) letters)
            letters)

(test-equal "rotate-right 4"
            (rotate-right (+ (length letters) 4) letters)
            (rotate-right 4 letters))

(let ((test-list '(a b c d e f g)))
  (list-swap! test-list 3 6)
  (test-equal test-list
              '(a b c g e f d)))

(test-equal "insert-at 1"
            (insert-at 'hello 0 '(a b c d e f))
            '(hello a b c d e f))

(test-equal "insert-at 2"
            (insert-at 'hello 1 '(a b c d e f))
            '(a hello b c d e f))

(test-equal "insert-at 3"
            (insert-at 'hello 5 '(a b c d e f))
            '(a b c d e hello f))

(test-equal "insert-at 4"
            (insert-at 'hello 6 '(a b c d e f))
            '(a b c d e f hello))

(test-equal "insert-left-first 1"
            (insert-left-first 'hello (lambda (x) (eq? 'e x)) letters)
            '(a b c d hello e f g h i))

(test-equal "insert-left-first 2"
            (insert-left-first 'hello (lambda (x) (eq? 'e x)) '(a e a e a e))
            '(a hello e a e a e))

(test-equal "insert-left 1"
            (insert-left 'hello (lambda (x) (eq? 'e x)) letters)
            '(a b c d hello e f g h i))

(test-equal "insert-left 2"
            (insert-left 'hello (lambda (x) (eq? 'e x)) '(a e a e a e))
            '(a hello e a hello e a hello e))

(test-equal "insert-left* 1"
            (insert-left* 'hello (lambda (x) (eq? 'e x)) '(a (a e) a))
            '(a (a hello e) a))

(test-equal "insert-left* 2"
            (insert-left* 'hello (lambda (x) (eq? 'e x)) '(a (a e) a a ((a)) e a (((e a)))))
            '(a (a hello e) a a ((a)) hello e a (((hello e a)))))

(test-equal "insert-right-first 1"
            (insert-right-first 'hello (lambda (x) (eq? 'e x)) letters)
            '(a b c d e hello f g h i))

(test-equal "insert-right-first 2"
            (insert-right-first 'hello (lambda (x) (eq? 'e x)) '(a e a e a e))
            '(a e hello a e a e))

(test-equal "insert-right 1"
            (insert-right 'hello (lambda (x) (eq? 'e x)) letters)
            '(a b c d e hello f g h i))

(test-equal "insert-right 2"
            (insert-right 'hello (lambda (x) (eq? 'e x)) '(a e a e a e))
            '(a e hello a e hello a e hello))

(test-equal "insert-right* 1"
            (insert-right* 'hello (lambda (x) (eq? 'e x)) '(a (a e) a))
            '(a (a e hello) a))

(test-equal "insert-right* 2"
            (insert-right* 'hello (lambda (x) (eq? 'e x)) '(a (a e) a a ((a)) e a (((e a)))))
            '(a (a e hello) a a ((a)) e hello a (((e hello a)))))

(test-equal "intersperse"
            (intersperse letters 'Z)
            '(a Z b Z c Z d Z e Z f Z g Z h Z i))

(test-equal "remove-at 1"
            (remove-at 0 letters)
            '(b c d e f g h i))

(test-equal "remove-at 2"
            (remove-at 8 letters)
            '(a b c d e f g h))

(test-end)
