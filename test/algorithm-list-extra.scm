(%load-library '(spheres/util test))

(%load-library '(spheres/algorithm list-extra))

(test-begin "list-extra" 10)


(define letters '(a b c d e f g h i))

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

(test-end)
