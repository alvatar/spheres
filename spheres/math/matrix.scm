;;!! Vector-based matrices
;; .author Alvaro Castro-Castilla, 2013-2015


;;!! Important note: All indices are 0-based

;;-------------------------------------------------------------------------------
;;!! Matrix type

;;! make-matrix creates a matrix (a vector of vectors).
(define* (make-matrix rows columns (init #f))
  (do ((m (if init (make-vector rows init) (make-vector rows)))
       (i 0 (+ i 1)))
      ((= i rows) m)
    (vector-set!
     m i
     (if init (make-vector columns init) (make-vector columns))))) 

;;! matrix? checks to see if its argument is a matrix.
;;! It isn't foolproof, but it's generally good enough.
(define (matrix? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (vector? (vector-ref x 0)))) 

;;! Check if the matrix is a square matrix
(define (matrix-square? x)
  (= (matrix-rows x) (matrix-columns x)))

;;! matrix-rows returns the number of rows in a matrix.
(define (matrix-rows x)
  (vector-length x)) 

;;! matrix-columns returns the number of columns in a matrix.
(define (matrix-columns x)
  (vector-length (vector-ref x 0))) 

;;! matrix-ref returns the jth element of the ith row.
(define (matrix-ref m i j)
  (vector-ref (vector-ref m i) j)) 

;;! matrix-set! changes the jth element of the ith row.
(define (matrix-set! m i j x)
  (vector-set! (vector-ref m i) j x)) 


;;-------------------------------------------------------------------------------
;;!! Matrix operations

;;! Matrix addition
(define (matrix:+matrix m1 m2)
  (let ((nr1 (matrix-rows m1))
        (nc1 (matrix-columns m1))
        (nr2 (matrix-rows m2))
        (nc2 (matrix-columns m2)))
    (if (not (and (= nr1 nr2) (= nc1 nc2)))
        (error matrix:-matrix "matrices are not of the same dimension"
               nr1 nc1 nr2 nc2))
    (let ((r (make-matrix nr1 nc1)))
      (do ((i 0 (+ i 1)))
          ((= i nr1) r)
        (do ((j 0 (+ j 1)))
            ((= j nc1))
          (matrix-set! r i j
                       (+ (matrix-ref m1 i j)
                          (matrix-ref m2 i j))))))))

;;! Add a scalar to each element in a matrix
(define (matrix:+scalar m x)
  (let ((nr (matrix-rows m))
        (nc (matrix-columns m)))
    (let ((r (make-matrix nr nc)))
      (do ((i 0 (+ i 1)))
          ((= i nr) r)
        (do ((j 0 (+ j 1)))
            ((= j nc))
          (matrix-set! r i j
                       (+ x (matrix-ref m i j))))))))

;;! Generic matrix/scalar addition
(define (matrix:+ x y)
  (if (matrix? x)
      (cond
       ((number? y) (matrix:+scalar x y))
       ((matrix? y) (matrix:+matrix x y))
       (else (error "matrix:+ -- second argument is not a matrix or number" y)))
      (error "matrix:+ -- first argument is not a matrix" x)))

;;! Matrix substraction
(define (matrix:-matrix m1 m2)
  (let ((nr1 (matrix-rows m1))
        (nc1 (matrix-columns m1))
        (nr2 (matrix-rows m2))
        (nc2 (matrix-columns m2)))
    (if (not (and (= nr1 nr2) (= nc1 nc2)))
        (error matrix:-matrix "matrices are not of the same dimension"
               nr1 nc1 nr2 nc2))
    (let ((r (make-matrix nr1 nc1)))
      (do ((i 0 (+ i 1)))
          ((= i nr1) r)
        (do ((j 0 (+ j 1)))
            ((= j nc1))
          (matrix-set! r i j
                       (- (matrix-ref m1 i j)
                          (matrix-ref m2 i j))))))))

;;! Substract a scalar from each element in a matrix
(define (matrix:-scalar m x)
  (let ((nr (matrix-rows m))
        (nc (matrix-columns m)))
    (let ((r (make-matrix nr nc)))
      (do ((i 0 (+ i 1)))
          ((= i nr) r)
        (do ((j 0 (+ j 1)))
            ((= j nc))
          (matrix-set! r i j
                       (- (matrix-ref m i j) x)))))))

;;! Generic matrix/scalar substraction
(define (matrix:- x y)
  (if (matrix? x)
      (cond
       ((number? y) (matrix:-scalar x y))
       ((matrix? y) (matrix:-matrix x y))
       (else (error "matrix:- -- second argument is not a matrix or number" y)))
      (error "matrix:- -- first argument is not a matrix" x)))

;;! Matrix product
(define (matrix:*matrix m1 m2)
  (let ((nr1 (matrix-rows m1))
        (nr2 (matrix-rows m2))
        (nc2 (matrix-columns m2)))
    (if (not (= (matrix-columns m1) nr2))
        (error matrix:*matrix "matrix:*matrix arguments are incompatible operands" m1 m2))
    (let ((r (make-matrix nr1 nc2)))
      (do ((i 0 (+ i 1)))
          ((= i nr1) r)
        (do ((j 0 (+ j 1)))
            ((= j nc2))
          (do ((k 0 (+ k 1))
               (a 0
                  (+ a
                     (* (matrix-ref m1 i k)
                        (matrix-ref m2 k j)))))
              ((= k nr2)
               (matrix-set! r i j a))))))))

;;! Matrix-scalar product
(define (matrix:*scalar m x)
  (let ((nr (matrix-rows m))
        (nc (matrix-columns m)))
    (let ((r (make-matrix nr nc)))
      (do ((i 0 (+ i 1)))
          ((= i nr) r)
        (do ((j 0 (+ j 1)))
            ((= j nc))
          (matrix-set! r i j
                       (* x (matrix-ref m i j))))))))

;;! Generic matrix/scalar multiplication procedure
(define (matrix:* x y)
  (cond
   ((number? x)
    (cond
     ((number? y) (* x y))
     ((matrix? y) (matrix:*scalar y x))
     (else (error "matrix:* -- second argument is not a number or matrix" y))))
   ((matrix? x)
    (cond
     ((number? y) (matrix:*scalar x y))
     ((matrix? y) (matrix:*matrix x y))
     (else (error "matrix:* -- second argument is not a number or matrix" y))))
   (else (error "matrix:* -- first argument is not a number or matrix" x))))

;;! Matrix transposition
(define (matrix:transpose m)
  (let ((rows (matrix-rows m))
        (cols (matrix-columns m)))
    (let ((m2 (make-matrix cols rows)))
      (let loop ((r 0))
        (let loop2 ((c 0))
          (matrix-set! m2 r c (matrix-ref m c r))
          (if (< c (- cols 1)) (loop2 (+ c 1))))
        (if (< r (- rows 1)) (loop (+ r 1))))
      m2)))


;;! Extract a submatrix removing a column and a row
(define (matrix:minor-submatrix m i j)
  (let ((rows (matrix-rows m))
        (cols (matrix-columns m)))
    (if (or (>= i cols) (>= j rows))
        (error "matrix:minor-submatrix -- term (i, j) out of bounds" i j))
    (let ((m2 (make-matrix (- cols 1) (- rows 1)))
          (max-i (- cols 2))
          (max-j (- rows 2)))
      (let loop ((mr (if (zero? j) 1 0))
                 (m2r 0))
        (let loop2 ((mc (if (zero? i) 1 0))
                    (m2c 0))
          (matrix-set! m2 m2r m2c (matrix-ref m mc mr))
          (if (< m2c max-i)
              (loop2 (let ((next-mc (+ mc 1)))
                       (if (= next-mc i)
                           (+ 1 next-mc)
                           next-mc))
                     (+ m2c 1))))
        (if (< m2r max-j)
            (loop (let ((next-mr (+ mr 1)))
                    (if (= next-mr j)
                        (+ 1 next-mr)
                        next-mr))
                  (+ m2r 1))))
      m2)))

;;! Calculate the determinant of a matrix using Laplace's formula
(define (matrix:determinant m)
  (let ((rows (matrix-rows m))
        (cols (matrix-columns m)))
    (if (not (= rows cols))
        (error "matrix:determinant -- argument must be a square matrix"))
    (if (= rows 1)
        (matrix-ref m 0 0)
        (do ((j (- rows 1) (- j 1))
	     (ans 0 (+ ans (* (matrix-ref m 0 j)
                              (matrix:cofactor m 0 j)))))
	    ((< j 0) ans)))))

;;! Calculate the cofactor of an element
(define (matrix:cofactor m i j)
  (* (if (odd? (+ i j)) -1 1)
     (matrix:determinant
      (matrix:minor-submatrix m i j))))

;;! Generic matrix inversion procedure
;; This method is slow, but should stable
(define (matrix:inverse m)
  (let ((det (matrix:determinant m))
        (rows (matrix-rows m))
        (cols (matrix-columns m)))
    (if (not (= rows cols))
        (error "matrix:inverse -- matrix is not a square matrix"))
    (if (zero? det)
        (error "matrix:inverse -- matrix is not invertible")
        (let ((inv (make-matrix cols rows)))
          (do ((i 0 (+ i 1)))
              ((= i rows) inv)
            (do ((j 0 (+ j 1)))
                ((= j cols))
              (matrix-set! inv i j (/ (matrix:cofactor m j i) det))))))))

;; Calculates the Cholesky decomposition matrix r 
;; for a positive-definite, symmetric nxn matrix A.
(define (matrix:cholesky m)
  (let ((n (matrix-rows m))
        (cols (matrix-columns m)))
    (if (not (= n cols))
        (error "matrix:inverse -- matrix is not a square matrix"))
    (let ((L (make-matrix n n 0)))
      (do ((k 0 (+ k 1)))
          ((> k (- n 1)) L)
        ;; Diagonal elements r_kk.
        (matrix-set!
         L k k
         (sqrt (- (matrix-ref m k k)
                  (let recur ((j 0)
                              (sum (expt (matrix-ref L k 0) 2)))
                    (if (> j (- k 1))
                        sum
                        (let ((next-j (+ j 1)))
                          (recur next-j
                                 (+ sum (expt (matrix-ref L k next-j) 2))))
                        )))))
        ;; Elements below a diagonal element, r_ik, i=k+1..n.
        (do ((i (+ k 1) (+ i 1)))
            ((> i (- n 1)) #f)
          (matrix-set!
           L i k
           (/ (- (matrix-ref m i k)
                 (let recur ((j 0)
                             (sum (* (matrix-ref L i 0) (matrix-ref L k 0))))
                   (if (> j (- k 1))
                       sum
                       (let ((next-j (+ j 1)))
                         (recur next-j
                                (+ sum (* (matrix-ref L i next-j)
                                          (matrix-ref L k next-j))))))))
              (matrix-ref L k k))))))))

;;! Solve the matrix equation LUx = b
;; L and U are lower and upper triangular matrices respectively
(define (matrix:lu-evaluate L U b)
  ;; Ax = b -> LUx = b
  ;; y is defined to be Ux
  (let ((n (vector-length b)))
    (let ((x (make-vector n))
          (y (make-vector n)))
      ;; Forward solve Ly = b
      (do ((i 0 (+ i 1)))
          ((= i n))
        (vector-set! y i (vector-ref b i))
        (do ((j 0 (+ j 1)))
            ((= j i))
          (vector-set! y i (- (vector-ref y i)
                              (* (matrix-ref L i j)
                                 (vector-ref y j)))))
        (vector-set! y i (/ (vector-ref y i)
                            (matrix-ref L i i))))
      ;; Backward solve Ux = y
      (do ((i (- n 1) (- i 1)))
          ((< i 0))
        (vector-set! x i (vector-ref y i))
        (do ((j (+ i 1) (+ j 1)))
            ((= j n))
          (vector-set! x i (- (vector-ref x i)
                              (* (matrix-ref U i j)
                                 (vector-ref x j)))))
        (vector-set! x i (/ (vector-ref x i)
                            (matrix-ref U i i))))
      x)))

;;! Calculate the inverse using Cholesky decomposition
(define (matrix:inverse/cholesky m)
  (let ((rows (matrix-rows m))
        (cols (matrix-columns m)))
    (if (not (= rows cols))
        (error "matrix:determinant -- argument must be a square matrix"))
    (let* ((cholesky (matrix:cholesky m))
           (t-cholesky (matrix:transpose (matrix:cholesky m))))
      (let ((m2 (make-matrix cols rows)))
        (do ((i 0 (+ i 1)))
            ((= i rows) m2)
          (let ((v (make-vector rows 0)))
            (vector-set! v i 1)
            (let ((lu-sol
                   (matrix:lu-evaluate cholesky t-cholesky v)))
              (do ((j 0 (+ j 1)))
                  ((= j cols))
                (matrix-set! m2 i j (vector-ref lu-sol j))))))))))

;;! Map
(define (matrix:map f m)
  (let ((nr (matrix-rows m))
        (nc (matrix-columns m)))
    (let ((r (make-matrix nr nc)))
      (do ((i 0 (+ i 1)))
          ((= i nr) r)
        (do ((j 0 (+ j 1)))
            ((= j nc))
          (matrix-set! r i j
                       (f (matrix-ref m i j))))))))
