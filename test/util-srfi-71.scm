(##spheres-load core: testing)
(##spheres-include let-multiple-macros)

;; Based on code by Sebastian.Egner@philips.com, 1-Aug-2005, R5RS + SRFI-71.

(check-set-mode! 'summary)

;; --- test cases for unnamed r5rs-let ---

;; form2 with one value (i.e. ordinary srfi-let)
(check (srfi-let () 1) => 1)
(check (srfi-let ((x1 1)) x1) => 1)
(check (srfi-let ((x1 1) (y1 2)) (list x1 y1)) => '(1 2))

;; form2 with two
(check (srfi-let ((x1 x2 (values 1 2))) (list x1 x2)) => '(1 2))
(check (srfi-let ((x1 x2 x3 (values 1 2 3))) (list x1 x2 x3)) => '(1 2 3))

;; form1 without rest arg
(check (srfi-let (((values) (values))) 1) => 1)
(check (srfi-let (((values x1) (values 1))) x1) => 1)
(check (srfi-let (((values x1 x2) (values 1 2))) (list x1 x2)) => '(1 2))
(check (srfi-let (((values x1 x2 x3) (values 1 2 3))) (list x1 x2 x3)) => '(1 2 3))

;; form1 with rest arg only
(check (srfi-let (((values . x0+) (values))) x0+) => '())
(check (srfi-let (((values . x0+) (values 1))) x0+) => '(1))
(check (srfi-let (((values . x0+) (values 1 2))) x0+) => '(1 2))
(check (srfi-let (((values . x0+) (values 1 2 3))) x0+) => '(1 2 3))

;; form1 with one and rest arg
(check (srfi-let (((values x1 . x1+) (values 1))) (cons x1 x1+)) => '(1))
(check (srfi-let (((values x1 . x1+) (values 1 2))) (cons x1 x1+)) => '(1 2))
(check (srfi-let (((values x1 . x1+) (values 1 2 3))) (cons x1 x1+)) => '(1 2 3))

;; form1 with two and rest arg
(check (srfi-let (((values x1 x2 . x2+) (values 1 2)))
            (cons x1 (cons x2 x2+))) 
          => '(1 2))
(check (srfi-let (((values x1 x2 . x2+) (values 1 2 3)))
            (cons x1 (cons x2 x2+)))
          => '(1 2 3))

;; --- test cases for named srfi-let ---

;; ordinary form
(check (srfi-let loop ((x 1)) (if (zero? x) x (loop (- x 1)))) => 0)

;; using (values x)
(check (srfi-let loop (((values x) 1)) (if (zero? x) x (loop (- x 1)))) => 0)

;; --- test cases for srfi-let* ---

;; We assume that srfi-let* is defined in terms of srfi-let but check
;; if the scopes are correct.

;; simple srfi-let
(check (srfi-let* () 1) => 1)
(check (srfi-let* ((x1 1)) x1) => 1)
(check (srfi-let* (((values x1 x2 . x2+) (values 1 2 3))) 
            (cons x1 (cons x2 x2+)))
          => '(1 2 3))

;; nested srfi-let
(check (srfi-let* ((x1 1) (y1 x1)) (list x1 y1)) => '(1 1))
(check (srfi-let* (((values x1 x2 . x2+) (values 1 2 3))
                 ((values y1 y2 . y2+) (apply values x1 x2 x2+)))
            (list 
             (cons x1 (cons x2 x2+))
             (cons y1 (cons y2 y2+))))
          => '((1 2 3) (1 2 3)))

;; --- test cases for srfi-letrec ---

;; original srfi-letrec
(check (srfi-letrec () 1) => 1)
(check (srfi-letrec ((x 1)) x) => 1)
(check (srfi-letrec ((x 1) (y 2)) (list x y)) => '(1 2))
(check (srfi-letrec ((x (lambda () (y))) (y (lambda () 1))) (x)) => 1)

;; too few test cases...
(check (srfi-letrec ((x y (values (lambda () (y))
                                (lambda () 1))))
            (x))
          => 1)

;; --- nasty things ---

(check (srfi-let ((values 1)) values) => 1)

(check (srfi-let (((values values) 1)) values) => 1)

(check (srfi-let (((values bad values) (values 1 2)))
	    (list bad values))
	  => '(1 2))

;; --- values->list etc. ---

(check (values->list (values)) => '())
(check (values->list (values 1)) => '(1))
(check (values->list (values 1 2)) => '(1 2))
(check (values->list (values 1 2 3)) => '(1 2 3))

(check (values->vector (values)) => '#())
(check (values->vector (values 1)) => '#(1))
(check (values->vector (values 1 2)) => '#(1 2))
(check (values->vector (values 1 2 3)) => '#(1 2 3))

(check-report)
