(%load-library '(spheres/util test))

(%load-library '(spheres/algorithm hamming-distance))

(test-begin "hamming-distance" 9)

(test-equal "hamming distance 1"
            (hamming-distance '(a b c d) '(a b a d))
            1)

(test-equal "hamming distance 2"
            (hamming-distance '(a a c d) '(a b a d))
            2)

(test-equal "hamming distance 3"
            (hamming-distance '(a b c d) '(a b c d))
            0)

(test-error "hamming distance 4"
            (hamming-distance '(a a c d) '(a)))

(test-equal "hamming distance with all permutations 1"
            (hamming-distance-all-permutations '(a b c d) '(a b c d))
            0)

(test-equal "hamming distance with all permutations 2"
            (hamming-distance-all-permutations '(a b c d) '(a d c b))
            0)

(test-equal "hamming distance with all permutations 3"
            (hamming-distance-all-permutations '(a b c d) '(d b z a))
            1)

(test-equal "hamming distance with all permutations 4"
            (hamming-distance-all-permutations '(a b c d) '(a b z a))
            2)

(test-error "hamming distance with all permutations 5"
            (hamming-distance-all-permutations '(a b c d) '(a)))
            
(test-end)
