;; SRFI-41
;; Do not remove previous line (used for tests)
;; Based on code by Kon Lovett, 2009

(%load-library '(spheres/util test))
(%load-library '(spheres/streams primitive))
(%load-library '(spheres/streams derived))
(%load-library '(spheres/streams extra))

(test-begin "SRFI-41: Streams" 239)

(define strm123 (stream 1 2 3))

;; stream-null
(test-assert "stream-null 1" (stream? stream-null))
(test-assert "stream-null 2" (stream-null? stream-null))
(test-equal "stream-null 3" (stream-pair? stream-null) #f)

;; stream-cons
(test-assert "stream-cons 1" (stream? (stream-cons 1 stream-null)))
(test-equal "stream-cons 2" (stream-null? (stream-cons 1 stream-null)) #f)
(test-assert "stream-cons 3" (stream-pair? (stream-cons 1 stream-null)))

;; stream?
(test-assert "stream? 1" (stream? stream-null))
(test-assert "stream? 2" (stream? (stream-cons 1 stream-null)))
(test-equal "stream? 3" (stream? "four") #f)

;; stream-null?
(test-assert "stream-null? 1" (stream-null? stream-null))
(test-equal "stream-null? 2" (stream-null? (stream-cons 1 stream-null)) #f)
(test-equal "stream-null? 3" (stream-null? "four") #f)

;; stream-pair?
(test-equal "stream-pair? 1" (stream-pair? stream-null) #f)
(test-assert "stream-pair? 2" (stream-pair? (stream-cons 1 stream-null)))
(test-equal "stream-pair? 3" (stream-pair? "four") #f)

;; stream-car
(test-error "stream-car 1" #t (stream-car "four"))
(test-error "stream-car 2" #t(stream-car stream-null))
(test-equal "stream-car 3" (stream-car strm123) 1)

;; stream-cdr
(test-error "stream-cdr 1" #t (stream-cdr "four"))
(test-error "stream-cdr 2" #t (stream-cdr stream-null))
(test-equal "stream-cdr 3" (stream-car (stream-cdr strm123)) 2)

;; stream-lambda
(test-equal "stream-lambda"
 (stream->list
  (letrec ((double
            (stream-lambda (strm)
                           (if (stream-null? strm)
                               stream-null
                               (stream-cons
                                (* 2 (stream-car strm))
                                (double (stream-cdr strm)))))))
    (double strm123)))
 '(2 4 6))

;; define-stream
(test-equal "define-stream"
 (stream->list
  (let ()
    (define-stream (double strm)
      (if (stream-null? strm)
          stream-null
          (stream-cons
           (* 2 (stream-car strm))
           (double (stream-cdr strm)))))
    (double strm123)))
 '(2 4 6))

;; list->stream
(test-error "list->stream 1" #t (list->stream "four"))
(test-equal (stream->list (list->stream '())) '())
(test-equal (stream->list (list->stream '(1 2 3))) '(1 2 3))

;; port->stream
(let* ((p (open-input-file ".library/test/streams/streams.scm"))
       (s (port->stream p)))
  (test-error "port->stream 1" (port->stream "four"))
  (test-assert "port->stream 2" (string=? (list->string (stream->list 10 s)) ";; SRFI-41"))
  (close-input-port p))

;; stream
(test-equal "stream 1" (stream->list (stream))
            '())
(test-equal "stream 2" (stream->list (stream 1))
            '(1))
(test-equal "stream 3" (stream->list (stream 1 2 3))
            '(1 2 3))

;; stream->list
(test-error "stream->list 1" #t (stream->list '()))
(test-error "stream->list 2" #t (stream->list "four" strm123))
(test-error "stream->list 3" #t (stream->list -1 strm123))
(test-equal "stream->list 4" (stream->list (stream)) '())
(test-equal "stream->list 5" (stream->list strm123) '(1 2 3))
(test-equal "stream->list 6" (stream->list 5 strm123) '(1 2 3))
(test-equal "stream->list 7" (stream->list 3 (stream-from 1)) '(1 2 3))

;; stream-append
(test-error "stream-append 1" (stream-append "four"))
(test-equal "stream-append 2" (stream->list (stream-append strm123)) '(1 2 3))
(test-equal "stream-append 3" (stream->list (stream-append strm123 strm123)) '(1 2 3 1 2 3))
(test-equal "stream-append 4" (stream->list (stream-append strm123 strm123 strm123)) '(1 2 3 1 2 3 1 2 3))
(test-equal "stream-append 5" (stream->list (stream-append strm123 stream-null)) '(1 2 3))
(test-equal "stream-append 6" (stream->list (stream-append stream-null strm123)) '(1 2 3))

;; stream-concat
(test-error "stream-concat 1" #t (stream-concat "four"))
(test-equal "stream-concat 2" (stream->list (stream-concat (stream strm123))) '(1 2 3))
(test-equal "stream-concat 3" (stream->list (stream-concat (stream strm123 strm123))) '(1 2 3 1 2 3))

;; stream-constant
(test-equal "stream-constant 1" (stream-ref (stream-constant 1) 100) 1)
(test-equal "stream-constant 2" (stream-ref (stream-constant 1 2) 100) 1)
(test-equal "stream-constant 3" (stream-ref (stream-constant 1 2 3) 3) 1)

;; stream-drop
(test-error "stream-drop 1" #t (stream-drop "four" strm123))
(test-error "stream-drop 2" #t (stream-drop -1 strm123))
(test-error "stream-drop 3" #t (stream-drop 2 "four"))
(test-equal "stream-drop 4" (stream->list (stream-drop 0 stream-null)) '())
(test-equal "stream-drop 5" (stream->list (stream-drop 0 strm123)) '(1 2 3))
(test-equal "stream-drop 6" (stream->list (stream-drop 1 strm123)) '(2 3))
(test-equal "stream-drop 7" (stream->list (stream-drop 5 strm123)) '())

;; stream-drop-while
(test-error "stream-drop-while 1" #t (stream-drop-while "four" strm123))
(test-error "stream-drop-while 2" #t (stream-drop-while odd? "four"))
(test-equal "stream-drop-while 3" (stream->list (stream-drop-while odd? stream-null)) '())
(test-equal "stream-drop-while 4" (stream->list (stream-drop-while odd? strm123)) '(2 3))
(test-equal "stream-drop-while 5" (stream->list (stream-drop-while even? strm123)) '(1 2 3))
(test-equal "stream-drop-while 6" (stream->list (stream-drop-while positive? strm123)) '())
(test-equal "stream-drop-while 7" (stream->list (stream-drop-while negative? strm123)) '(1 2 3))

;; stream-filter
(test-error "stream-filter 1" #t (stream-filter "four" strm123))
(test-error "stream-filter 2" #t (stream-filter odd? '()))
(test-equal "stream-filter 3" (stream-null? (stream-filter odd? (stream))) #t)
(test-equal "stream-filter 4" (stream->list (stream-filter odd? strm123)) '(1 3))
(test-equal "stream-filter 5" (stream->list (stream-filter even? strm123)) '(2))
(test-equal "stream-filter 6" (stream->list (stream-filter positive? strm123)) '(1 2 3))
(test-equal "stream-filter 7" (stream->list (stream-filter negative? strm123)) '())
(let loop ((n 10))
  (test-equal "stream-filter 8" (odd? (stream-ref (stream-filter odd? (stream-from 0)) n)) #t)
  (if (positive? n) (loop (- n 1))))
(let loop ((n 10))
  (test-equal "stream-filter 9" (even? (stream-ref (stream-filter odd? (stream-from 0)) n)) #f)
  (if (positive? n) (loop (- n 1))))

;; stream-fold
(test-error "stream-fold 1" #t (stream-fold "four" 0 strm123))
(test-error "stream-fold 2" #t (stream-fold + 0 '()))
(test-equal "stream-fold 3" (stream-fold + 0 strm123) 6)

;; stream-for-each
(test-error "stream-for-each 1" #t (stream-for-each "four" strm123))
(test-error "stream-for-each 2" #t (stream-for-each display))
(test-error "stream-for-each 3" #t (stream-for-each display "four"))
(test-equal (let ((sum 0)) (stream-for-each (lambda (x) (set! sum (+ sum x))) strm123) sum) 6)

;; stream-from
(test-error "stream-from 1" #t (stream-from "four"))
(test-error "stream-from 2" #t (stream-from 1 "four"))
(test-equal "stream-from 3" (stream-ref (stream-from 0) 100) 100)
(test-equal "stream-from 4" (stream-ref (stream-from 1 2) 100) 201)
(test-equal "stream-from 5" (stream-ref (stream-from 0 -1) 100) -100)

;; stream-iterate
(test-error "stream-iterate 1" #t (stream-iterate "four" 0))


(let ((left-section
       (lambda (fn . args)
         (lambda xs (apply fn (append args xs))))))
  (test-equal "stream-iterate 2"
              (stream->list 3
                            (stream-iterate
                             (left-section + 1) 1)) '(1 2 3)))

;; stream-length
(test-error "stream-length 1" #t (stream-length "four"))
(test-equal "stream-length 2" (stream-length (stream)) 0)
(test-equal "stream-length 3" (stream-length strm123) 3)

;; stream-let
(test-equal "stream-let"
            (stream->list
             (stream-let loop ((strm strm123))
                         (if (stream-null? strm)
                             stream-null
                             (stream-cons
                              (* 2 (stream-car strm))
                              (loop (stream-cdr strm))))))
            '(2 4 6))

;; stream-map
(test-error "stream-map 1" #f (stream-map "four" strm123))
(test-error "stream-map 2" #f (stream-map odd?))
(test-error "stream-map 3" #f (stream-map odd? "four"))
(test-equal "stream-map 4" (stream->list (stream-map - strm123)) '(-1 -2 -3))
(test-equal "stream-map 5" (stream->list (stream-map + strm123 strm123)) '(2 4 6))
(test-equal "stream-map 6" (stream->list (stream-map + strm123 (stream-from 1))) '(2 4 6))
(test-equal "stream-map 7" (stream->list (stream-map + (stream-from 1) strm123)) '(2 4 6))
(test-equal "stream-map 8" (stream->list (stream-map + strm123 strm123 strm123)) '(3 6 9))

;; stream-match
(test-error "stream-match 1" #f (stream-match '(1 2 3) (_ 'ok)))
(test-error "stream-match 2" #f (stream-match strm123 (() 42)))
(test-equal "stream-match 3"(stream-match stream-null (() 'ok)) 'ok)
(test-equal "stream-match 4" (stream-match strm123 (() 'no) (else 'ok)) 'ok)
(test-equal "stream-match 5" (stream-match (stream 1) (() 'no) ((a) a)) 1)
(test-equal "stream-match 6" (stream-match (stream 1) (() 'no) ((_) 'ok)) 'ok)
(test-equal "stream-match 7" (stream-match strm123 ((a b c) (list a b c))) '(1 2 3))
(test-equal "stream-match 8" (stream-match strm123 ((a . _) a)) 1)
(test-equal "stream-match 9" (stream-match strm123 ((a b . _) (list a b))) '(1 2))
(test-equal "stream-match 10" (stream-match strm123 ((a b . c) (list a b (stream-car c)))) '(1 2 3))
(test-equal "stream-match 11" (stream-match strm123 (s (stream->list s))) '(1 2 3))
(test-equal "stream-match 12" (stream-match strm123 ((a . _) (= a 1) 'ok)) 'ok)
(test-equal "stream-match 13" (stream-match strm123 ((a . _) (= a 2) 'yes) (_ 'no)) 'no)
(test-equal "stream-match 14" (stream-match strm123 ((a b c) (= a b) 'yes) (_ 'no)) 'no)
(test-equal "stream-match 15" (stream-match (stream 1 1 2) ((a b c) (= a b) 'yes) (_ 'no)) 'yes)

;; stream-of
(test-equal "stream-of 1"
            (stream->list
             (stream-of (+ y 6)
                        (x in (stream-range 1 6))
                        (odd? x)
                        (y is (* x x))))
            '(7 15 31))
(test-equal "stream-of 2"
            (stream->list
             (stream-of (* x y)
                        (x in (stream-range 1 4))
                        (y in (stream-range 1 5))))
            '(1 2 3 4 2 4 6 8 3 6 9 12))
(test-equal "stream-of 3"
            (stream-car (stream-of 1))
            1)

;; stream-range
(test-error "stream-range 1" #t (stream-range "four" 0))
(test-error "stream-range 2" #t (stream-range 0 "four"))
(test-error "stream-range 3" #t (stream-range 1 2 "three"))
(test-equal "stream-range 4" (stream->list (stream-range 0 5)) '(0 1 2 3 4))
(test-equal "stream-range 5" (stream->list (stream-range 5 0)) '(5 4 3 2 1))
(test-equal "stream-range 6" (stream->list (stream-range 0 5 2)) '(0 2 4))
(test-equal "stream-range 7" (stream->list (stream-range 5 0 -2)) '(5 3 1))
(test-equal "stream-range 8" (stream->list (stream-range 0 1 -1)) '())

;; stream-ref
(test-error "stream-ref 1" #t (stream-ref '() 4))
(test-error "stream-ref 2" #t (stream-ref natural-numbers-stream 3.5))
(test-error "stream-ref 3" #t(stream-ref natural-numbers-stream -3))
(test-error "stream-ref 4" #t (stream-ref strm123 5))
(test-equal "stream-ref 5" (stream-ref strm123 0) 1)
(test-equal "stream-ref 6" (stream-ref strm123 1) 2)
(test-equal "stream-ref 7" (stream-ref strm123 2) 3)

;; stream-reverse
(test-error "stream-reverse 1" #t (stream-reverse '()))
(test-equal "stream-reverse 2" (stream->list (stream-reverse (stream))) '())
(test-equal "stream-reverse 3" (stream->list (stream-reverse strm123)) '(3 2 1))

;; stream-scan
(test-error "stream-scan 1" #t (stream-scan "four" 0 strm123))
(test-error "stream-scan 2" #t (stream-scan + 0 '()))
(test-equal "stream-scan 3" (stream->list (stream-scan + 0 strm123)) '(0 1 3 6))

; stream-take
(test-error "stream-take 1" #t (stream-take 5 "four"))
(test-error "stream-take 2" #t (stream-take "four" strm123))
(test-error "stream-take 3" #t (stream-take -4 strm123))
(test-equal "stream-take 4" (stream->list (stream-take 5 stream-null)) '())
(test-equal "stream-take 5" (stream->list (stream-take 0 stream-null)) '())
(test-equal "stream-take 6" (stream->list (stream-take 0 strm123)) '())
(test-equal "stream-take 7" (stream->list (stream-take 2 strm123)) '(1 2))
(test-equal "stream-take 8" (stream->list (stream-take 3 strm123)) '(1 2 3))
(test-equal "stream-take 9" (stream->list (stream-take 5 strm123)) '(1 2 3))

;; stream-take-while
(test-error "stream-take-while 1" #t (stream-take-while odd? "four"))
(test-error "stream-take-while 2" (stream-take-while "four" strm123))
(test-equal "stream-take-while 3" (stream->list (stream-take-while odd? strm123)) '(1))
(test-equal "stream-take-while 4" (stream->list (stream-take-while even? strm123)) '())
(test-equal "stream-take-while 5" (stream->list (stream-take-while positive? strm123)) '(1 2 3))
(test-equal "stream-take-while 6" (stream->list (stream-take-while negative? strm123)) '())

;; stream-unfold
(test-error "stream-unfold 1" #t (stream-unfold "four" odd? + 0))
(test-error "stream-unfold 2" #t (stream-unfold + "four" + 0))
(test-error "stream-unfold 3" #t (stream-unfold + odd? "four" 0))
(let ((right-section (lambda (fn . args)
                       (lambda xs (apply fn (append xs args))) )))
  (test-equal "stream-unfold 4"
              (stream->list
               (stream-unfold (right-section expt 2) (right-section < 10) (right-section + 1) 0))
              '(0 1 4 9 16 25 36 49 64 81)))

;; stream-unfolds
(test-equal "stream-unfolds"
            (stream->list
             (stream-unfolds
              (lambda (x)
                (let ((n (car x)) (s (cdr x)))
                  (if (zero? n)
                      (values 'dummy '())
                      (values (cons (- n 1) (stream-cdr s)) (list (stream-car s))))))
              (cons 5 (stream-from 0))))
            '(0 1 2 3 4))

;; stream-zip
(test-error "stream-zip 1" #t (stream-zip))
(test-error "stream-zip 2" #t (stream-zip "four"))
(test-error "stream-zip 3" #t (stream-zip strm123 "four"))
(test-equal "stream-zip 4" (stream->list (stream-zip strm123 stream-null)) '())
(test-equal "stream-zip 5" (stream->list (stream-zip strm123)) '((1) (2) (3)))
(test-equal "stream-zip 6" (stream->list (stream-zip strm123 strm123)) '((1 1) (2 2) (3 3)))
(test-equal "stream-zip 7" (stream->list (stream-zip strm123 (stream-from 1))) '((1 1) (2 2) (3 3)))
(test-equal "stream-zip 8" (stream->list (stream-zip strm123 strm123 strm123)) '((1 1 1) (2 2 2) (3 3 3)))

;; other tests
;; (test-equal
;;  "extra test 2"
;;  (stream->list (stream-quick-sort < (stream 3 1 5 2 4)))
;;  (stream->list (stream-insertion-sort < (stream 2 5 1 4 3))))

;; (test-equal
;;  "extra test 3"
;;  (stream->list (stream-merge-sort < (stream 3 1 5 2 4)))
;;  (stream->list (stream-insertion-sort < (stream 2 5 1 4 3))))

(test-end)
