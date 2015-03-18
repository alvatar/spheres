;;!!! Token table
;; .author Marc Feeley, 2005-2007
;; .autor Per Ekerdal
;; .author Alvaro Castro-Castilla, 2015
;;
;; File: "http.scm", Time-stamp: <2007-04-04 14:42:59 feeley>
;; Copyright (c) 2005-2007 by Marc Feeley, All Rights Reserved.


(declare
 (standard-bindings)
 (extended-bindings)
 (block))


(define hash-substring
  (lambda (str start end)
    (define loop
      (lambda (h i)
        (if (< i end)
            (loop (modulo (+ (* h 5063) (char->integer (string-ref str i)))
                          65536)
                  (+ i 1))
            h)))
    (loop 0 start)))

(define token-table-lookup-substring
  (lambda (table str start end)
    (let* ((n (quotient (vector-length table) 2))
           (h (* 2 (modulo (hash-substring str start end) n)))
           (x (vector-ref table h)))
      (define loop
        (lambda (i j)
          (if (< i end)
              (if (char=? (string-ref str i) (string-ref x j))
                  (loop (+ i 1) (+ j 1))
                  #f)
              h)))
      (and x
           (= (string-length x) (- end start))
           (loop start 0)))))

(define token-table-lookup-string
  (lambda (table str)
    (token-table-lookup-substring table str 0 (string-length str))))
