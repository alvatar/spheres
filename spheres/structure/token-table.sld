;;!!! Token table
;; .author Marc Feeley, 2005-2007
;; .autor Per Ekerdal
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure token-table)
  (export hash-substring
          make-token-table
          make-perfect-hash-table
          token-table-lookup-substring
          token-table-lookup-string)

  (define-macro (make-token-table . alist)
    ;; "alist" is a list of lists of the form "(string expression)"
    ;; The result is a perfect hash-table represented as a vector of
    ;; length 2*N, where N is the hash modulus.  If the string S is in
    ;; the hash-table it is at index
    ;;
    ;;   X = (* 2 (modulo (hash-substring S 0 (string-length S)) N))
    ;;
    ;; and the associated expression is at index X+1.
    (define hash-substring             ; repeated from above to be
      (lambda (str start end)               ; available for macro expansion
        (let loop ((h 0) (i start))
          (if (< i end)
              (loop (modulo (+ (* h 5063) (char->integer (string-ref str i)))
                            65536)
                    (+ i 1))
              h))))
    (define make-perfect-hash-table
      (lambda (alist)
        (let loop1 ((n (length alist)))
          (let ((v (make-vector (* 2 n) #f)))
            (let loop2 ((lst alist))
              (if (pair? lst)
                  (let* ((x (car lst))
                         (str (car x)))
                    (let ((h
                           (* 2
                              (modulo (hash-substring str 0 (string-length str))
                                      n))))
                      (if (vector-ref v h)
                          (loop1 (+ n 1))
                          (begin
                            (vector-set! v h str)
                            (vector-set! v (+ h 1) (cadr x))
                            (loop2 (cdr lst))))))
                  v))))))
    (cons 'vector (vector->list (make-perfect-hash-table alist))))

  (include "token-table.scm"))
