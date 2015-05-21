(load-relative "../lazy-seq")
(import lazy-seq)
(use test)

(test-group "custom lazy-seq"
  (define calls 0)

  (define natural-numbers
    (case-lambda
     (()  (natural-numbers 0))
     ((n) (lazy-seq
            (set! calls (+ calls 1))
            (cons n (natural-numbers (+ n 1)))))))

  (define numbers (natural-numbers))

  (test '(0 1 2) (lazy-seq->list (lazy-take 3 numbers)))
  (test '(0 1 2) (lazy-seq->list (lazy-take 3 numbers)))
  (test calls 3))

(test-group "lazy-seq-realized?"
  (define even-numbers (lazy-numbers step: 2))
  (test-assert (not (lazy-seq-realized? even-numbers)))
  (test 0 (lazy-head even-numbers))
  (test-assert (lazy-seq-realized? even-numbers))
  (test-assert (not (lazy-seq-realized? (lazy-tail even-numbers))))
  (test 2 (lazy-head (lazy-tail even-numbers))))

(test-group "list->lazy-seq"
  (define seq
    (list->lazy-seq '("foo" "bar")))

  (test-assert (lazy-seq? seq))
  (test-assert (lazy-seq-realized? seq))
  (test '("foo" "bar") (lazy-seq->list seq))
  (test-assert (lazy-null? (lazy-tail (lazy-tail seq))))
  (test "ok" (lazy-head (lazy-seq (list->lazy-seq '("ok"))))))


(test-group "lazy-length"
  (test 10 (lazy-length (lazy-numbers count: 10))))

(test-group "lazy-map"
  (test '(1 2 3 4 5)
        (lazy-seq->list
         (lazy-take 5 (lazy-map add1 (lazy-numbers)))))
  (test '(10 12 14)
        (lazy-seq->list
         (lazy-take 3 (lazy-map +
                                (lazy-numbers start: 7)
                                (lazy-numbers start: 3))))))

(test-group "lazy-append-map"
  (test '(1 2 3 4 5 6)
        (lazy-seq->list
         (lazy-append-map
          (compose list->lazy-seq list)
          (list->lazy-seq '(1 3 5))
          (list->lazy-seq '(2 4 6 7))))))


(test-group "lazy-filter"
  (test '(2 8 14 20 26)
        (lazy-seq->list
         (lazy-take 5 (lazy-filter
                       (lambda (x) (zero? (modulo x 2)))
                       (lazy-numbers start: 2 step: 3)))))
  (test '(2) (lazy-seq->list
              (lazy-filter (lambda (x) (= x 2))
                           (lazy-numbers count: 10)))))


(test-group "lazy-ref"
  (test 3 (lazy-ref 3 (lazy-numbers))))

(test-group "input-port->lazy-seq"
  (test '(b c d)
        (lazy-seq->list
         (lazy-take
          3 (lazy-tail
             (input-port->lazy-seq
              (open-input-string "a b c d e")
              read)))))

  (test-assert
   (lazy-null?
    (input-port->lazy-seq
     (open-input-string "")
     read))))

(test-group "lazy-each"
  (define sum 0)
  (lazy-each (lambda (i)
               (set! sum (+ i sum)))
             (lazy-numbers count: 5))
  (test sum (fold + 0 (iota 5))))

(test-group "lazy-repeat"
  (test '(x x x x) (lazy-seq->list (lazy-take 4 (lazy-repeat 'x)))))

(test-group "lazy-repeatedly"
  (test 10 (lazy-ref 9 (lazy-repeatedly
                        (let ((n 0))
                          (lambda () (set! n (+ n 1)) n))))))

(test-group "lazy-append"
  (test 6 (lazy-ref 5 (lazy-append (lazy-numbers count: 2)
                                   (lazy-numbers count: 2)
                                   (lazy-numbers start: 5)))))

(test-group "lazy-iterate"
  (test '(2 4 8 16)
        (lazy-seq->list
         (lazy-take 4 (lazy-iterate (lambda (x) (* x 2)) 2)))))

(test-group "lazy-take-while"
  (test '(2 2 2 4)
        (lazy-seq->list
         (lazy-take-while even? (list->lazy-seq '(2 2 2 4 5 6))))))

(test-group "lazy-drop-while"
  (test '(end)
        (lazy-seq->list
         (lazy-drop-while number? (list->lazy-seq '(1 2 3 4 end))))))

(test-group "lazy-reverse"
  (test '() (lazy-seq->list (lazy-reverse (lazy-seq '()))))
  (test '(3 2 1)
        (lazy-seq->list
         (lazy-reverse
          (lazy-numbers count: 3 start: 1)))))

(test-group "lazy-cycle"
  (test '(1 2 1 2 1 2)
        (lazy-seq->list
         (lazy-take 6 (lazy-cycle
                       (list->lazy-seq '(1 2))))))
  (test '() (lazy-seq->list (lazy-cycle (list->lazy-seq '())))))

(test-group "lazy-fold"
  (test 390 (lazy-fold + 0 (lazy-numbers start: 10 count: 20))))

(test-group "lazy-concatenate"
  (test '(1 2 3 4)
        (lazy-seq->list
          (lazy-concatenate
            (list->lazy-seq (list (list->lazy-seq '(1))
                                  (list->lazy-seq '(2 3))
                                  (list->lazy-seq '(4))))))))

(test-group "lazy-flatten"
  (test "nothing to flatten"
        '(1 2 3 4)
        (lazy-seq->list (lazy-flatten (list->lazy-seq '(1 2 3 4)))))
  (test "flatten one level deep"
        '(1 2 3 4)
        (lazy-seq->list
          (lazy-flatten (list->lazy-seq
                          (map (lambda (x) (list->lazy-seq (list x)))
                               '(1 2 3 4))))))
  (test "flatten multiple levels"
        '(1 2 3 4 5 6)
        (lazy-seq->list
          (lazy-flatten
            (list->lazy-seq
              (list (list->lazy-seq '(1))
                    (list->lazy-seq
                      (list
                        (list->lazy-seq '(2 3))
                        (list->lazy-seq '(4))
                        5))
                    6)))))

  (define depth1-calls 0)
  (define depth2-calls 0)

  (define seq
    (lazy-map
      (lambda (x)
        (set! depth1-calls (+ depth1-calls 1))
        (lazy-map
          (lambda (y)
            (set! depth2-calls (+ depth2-calls 1))
            (* 2 y)) x))
      (list->lazy-seq
        (list (list->lazy-seq '(1))
              (list->lazy-seq '(2))
              (list->lazy-seq '(3))
              (list->lazy-seq '(4))))))

  (lazy-seq->list (lazy-take 2 (lazy-flatten seq)))
  (test "lists are resolved lazily"
        '(2 2) (list depth1-calls depth2-calls)))


(test-group "nested lazy-seq realizations are retained"
  (define (realized-prefix seq)
    (if (lazy-seq-realized? seq)
        (cons (lazy-head seq)
              (realized-prefix (lazy-tail seq)))
        '()))

  (let* ((q (lazy-numbers))
         (r (lazy-seq q))
         (s (lazy-seq r)))
    (test '(0 1 2 3 4) (lazy-seq->list (lazy-take 5 s)))
    (test '(0 1 2 3 4) (realized-prefix r))
    (test '(0 1 2 3 4) (realized-prefix q))))

(test-exit)
