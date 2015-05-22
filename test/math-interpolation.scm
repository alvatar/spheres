(load (spheres/util test))

(load (spheres/math interpolation))

(test-begin "math/interpolation" 10)

(test-equal "range-expand 1"
            (range-expand '(0 8 2 5))
            '(0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 3 4 5))

(test-equal "range-expand 2"
            (range-expand '(1 3 1 3 1 3 1))
            '(1 2 3 2 1 2 3 2 1 2 3 2 1))

(test-equal "range-extract 1"
            (range-extract '(0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 3 4 5))
            '(0 8 2 5))

(test-equal "range-extract 2"
            (range-extract '(1 2 3 2 1 2 3 2 1 2 3 2 1))
            '(1 3 1 3 1 3 1))

;; 1.5 is rounded to 2.0
(test-equal "interpolate/nearest with vector 1"
            (interpolate/nearest '#(1 4 6 2 4) 1.5)
            6)

;; 2.5 is rounded to 2.0
(test-equal "interpolate/nearest with list 1"
            (interpolate/nearest '(1 4 6 2 4) 2.5)
            6)

(test-equal "interpolate/linear with vector 1"
            (interpolate/linear '#(1 4 6 2 4) 2.5)
            4.0)

(test-equal "interpolate/linear with vector 2"
            (interpolate/linear '#(1 4 6 2 4) 1.5)
            5.0)

(test-equal "interpolate/linear with list 1"
            (interpolate/linear '(1 4 6 2 4) 2.5)
            4.0)

(test-equal "interpolate/linear with list 2"
            (interpolate/linear '(1 4 6 2 4) 1.5)
            5.0)

(test-end)
