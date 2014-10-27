;;!!! Shuffle algorithms
;; .author Taylor Campbell

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))


(define (flip-coin)
  (zero? (random-integer 2)))

;;! Binary Shuffle
;; Go through the list, collecting a left list and a right list by
;; randomly choosing which list to put successive elements on.
;; Recursively the left and right lists, and then concatenate them.
(define (binary-shuffle-list list)
  (define (bifurcate list left right)
    (if (null-list? list)
        (values left right)
        (let ((item (car list))
              (list (cdr list)))
          (if (flip-coin)
              (bifurcate list (cons item left) right)
              (bifurcate list left (cons item right))))))
  (let shuffle ((list list) (tail '()))
    (cond ((null-list? list)
           tail)
          ((null-list? (cdr list))
           (cons (car list) tail))
          ((null-list? (cddr list))
           (if (flip-coin)
               (cons (car list) (cons (cadr list) tail))
               (cons (cadr list) (cons (car list) tail))))
          (else
           (receive (left right) (bifurcate list '() '())
                    (shuffle left (shuffle right tail)))))))

(define (binary-shuffle-list! list)
  (define (bifurcate! list left right)
    (if (null-list? list)
        (values left right)
        (let ((item (car list))
              (next (cdr list)))
          (if (flip-coin)
              (begin (set-cdr! list left)
                     (bifurcate! next list right))
              (begin (set-cdr! list right)
                     (bifurcate! next left list))))))
  (let shuffle! ((list list) (tail '()))
    (cond ((null-list? list)
           tail)
          ((null-list? (cdr list))
           (set-cdr! list tail)
           list)
          ((null-list? (cddr list))
           ;; LIST is (A B), so...
           (if (flip-coin)
               (let ((next (cdr list)))
                 ;; ...set it to (B A . tail).
                 (set-cdr! list tail)
                 (set-cdr! next list)
                 next)
               (begin
                 ;; ...set it to (A B . tail).
                 (set-cdr! (cdr list) tail)
                 list)))
          (else
           (receive (left right) (bifurcate! list '() '())
                    (shuffle! left (shuffle! right tail)))))))

;;! Merge Shuffle
;; Partition the list into two equal halves; shuffle the two halves,
;; and then merge them by randomly choosing which half to select the
;; next element from.
(define (merge-shuffle-list list)
  (define (merge a b)
    (cond ((not (pair? a)) b)
          ((not (pair? b)) a)
          (else
           (if (flip-coin)
               (cons (car a) (merge (cdr a) b))
               (cons (car b) (merge a (cdr b)))))))

  (define (partition list a b)
    (let ((next (cdr list))
          (a b)
          (b (cons (car list) a)))
      (if (null-list? next)
          (values a b)
          (partition next a b))))

  (if (null-list? list)
      '()
      (let shuffle ((list list))
        (if (null-list? (cdr list))
            list
            (receive (a b) (partition list '() '())
                     (merge (shuffle a) (shuffle b)))))))

(define (merge-shuffle-list! list)
  (define (merge! a b)
    (cond ((null-list? a)       b)
          ((null-list? b)       a)
          ((flip-coin)          (%merge! a b) a)
          (else                 (%merge! b a) b)))
  (define (%merge! a b)
    (cond ((null-list? (cdr a))
           (set-cdr! a b))
          ((flip-coin)
           (%merge! (cdr a) b))
          (else
           (%merge! b (let ((next (cdr a)))
                        (set-cdr! a b)
                        next)))))
  (define (partition! list a b)
    (let ((next (cdr list)))
      (set-cdr! list a)
      (if (null-list? next)
          (values list b)
          (partition! next b list))))
  (if (null-list? list)
      '()
      (let shuffle! ((list list))
        (if (null-list? (cdr list))
            list
            (receive (a b) (partition! list '() '())
                     (merge! (shuffle! a) (shuffle! b)))))))

;;! Insertion Shuffle
(define (insertion-shuffle-list list)
  (define (insert list position item)
    (if (zero? position)
        (cons item list)
        (cons (car list)
              (insert (cdr list) (- position 1) item))))
  (if (null-list? list)
      '()
      (let loop ((in (cdr list)) (count 1) (out (cons (car list) '())))
        (let ((count (+ count 1))
              (item (car in))
              (next (cdr in)))
          (let ((out (insert out (random-integer count) item)))
            (if (null-list? next)
                out
                (loop next count out)))))))

(define (insertion-shuffle-list! list)
  (define (insert! list lag position cell)
    (let ((position (- position 1)))
      (if (zero? position)
          (begin (set-cdr! lag cell)
                 (set-cdr! cell list))
          (insert! (cdr list) list position cell))))
  (if (null-list? list)
      '()
      (let ((in (cdr list)))
        (set-cdr! list '())
        (let loop ((in in) (count 1) (out list))
          (if (null-list? in)
              out
              (let ((next (cdr in))
                    (count (+ count 1)))
                (loop next
                      count
                      (let ((position (random-integer count)))
                        (if (zero? position)
                            (begin (set-cdr! in out)
                                   in)
                            (begin (insert! (cdr out) out position in)
                                   out))))))))))

;;! Selection Shuffle
(define (selection-shuffle-list list)
  (define (select list position)
    (if (zero? position)
        (values (car list) (cdr list))
        (receive (item tail)
                 (select (cdr list) (- position 1))
                 (values item (cons (car list) tail)))))
  (if (null-list? list)
      '()
      (let loop ((in list) (out '()) (len (length list)))
        (receive (item list) (select in (random-integer len))
                 (let ((out (cons item out)))
                   (if (null-list? list)
                       out
                       (loop list
                             (cons item out)
                             (- len 1))))))))

(define (selection-shuffle-list! list)
  (define (select! list lag position)
    (if (zero? position)
        (begin (set-cdr! lag (cdr list))
               list)
        (select! (cdr list) list (- position 1))))
  (if (null-list? list)
      '()
      (let loop ((in list) (out '()) (len (length list)))
        (let ((position (random-integer len)))
          (receive (cell next)
                   (if (zero? position)
                       (values in (cdr in))
                       (values (select! (cdr in) in (- position 1))
                               in))
                   (set-cdr! cell out)
                   (if (null-list? next)
                       cell
                       (loop next cell (- len 1))))))))

;;! Fisher-Yates O(n) Random-Access Shuffle
(define (Fisher-Yates-shuffler sequence-exchange!)
  (lambda (sequence start end)
    (do ((i start (+ i 1)))
        ((>= i end))
      (let ((j (+ start (random-integer (+ 1 (- i start))))))
        (if (not (= i j))
            (sequence-exchange! sequence i j))))))

(define (sequence-exchanger sequence-ref sequence-set!)
  (lambda (sequence i j)
    (let ((elt-i (sequence-ref sequence i))
          (elt-j (sequence-ref sequence j)))
      (sequence-set! sequence j elt-i)
      (sequence-set! sequence i elt-j))))

(define shuffle-vector!
  (Fisher-Yates-shuffler (sequence-exchanger vector-ref vector-set!)))

(define shuffle-string!
  (Fisher-Yates-shuffler (sequence-exchanger string-ref string-set!)))
