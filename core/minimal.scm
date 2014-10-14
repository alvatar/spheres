;;!!! Minimal toolkit for avoiding dependencies but still enjoy some goodies
;; .author Alvaro Castro-Castilla, 2014

;;! The accumulator represents the rightmost value to tack onto the end of
;; the list, after you've finished recursing down it.
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

;;! The accumulator represents the completed calculation for the leftmost
;; part of the list. Tail-recursive, more efficient than foldr.
(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

;;! Reduce
(define (reduce f i l)
  (let reduce ((i i) (l l))
    (if (null? l) i
        (reduce (f i (car l)) (cdr l)))))

;;! Unfold
(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

;;! Filter
(define (filter pred lst)
  (foldr (lambda (x y) (if (pred x) (cons x y) y))
         '()
         lst))

;;! Any
(define (any pred lst)
  (let recur ((rest lst))
    (cond ((null? rest) #f)
          ((pred (car rest)) #t)
          (else (recur (cdr rest))))))

;;! Every
(define (every pred lst)
  (let recur ((rest lst))
    (cond ((null? rest) #t)
          ((pred (car rest)) (recur (cdr rest)))
          (else #f))))

;;! Drop
(define (drop lis k)
    (let iter ((lis lis) (k k))
      (if (zero? k) lis (iter (cdr lis) (- k 1)))))

;;! Run the function at the leaves of the tree
(define (map* f l)
  (cond
   ((null? l) '())
   ((not (pair? l)) (f l))
   (else
    (cons (map** f (car l)) (map** f (cdr l))))))

;;! Run the function at every node of the tree
(define (map** f l)
  (cond
   ((null? l) '())
   ((not (pair? l)) (f l))
   (else
    (cons (f (map** f (car l))) (f (map** f (cdr l)))))))

;;! Curry
(define (curry func arg1)
  (lambda (arg) (apply func (cons arg1 arg))))

;;! Compose
(define (compose f g)
  (lambda (arg) (f (apply g arg))))

;;! Complement
(define (complement f)
  (lambda args (not (apply f args))))

;;! Non-tail recursive Quick Sort
(define (quicksort l gt?)
  (if (null? l)
      '()
      (append (quicksort (filter (lambda (x) (gt? (car l) x)) (cdr l)) gt?)
              (list (car l))
              (quicksort (filter (lambda (x) (not (gt? (car l) x))) (cdr l)) gt?))))

;;! Split a string using a separator
(define (string-split sep)
  (lambda (str)
    (call-with-input-string
     str
     (lambda (p)
       (read-all p (lambda (p) (read-line p sep)))))))

;; Concatenate strings
(define (string-concatenate strings)
  (define (%string-copy! to tstart from fstart fend)
    (if (> fstart tstart)
        (do ((i fstart (+ i 1))
             (j tstart (+ j 1)))
            ((>= i fend))
          (string-set! to j (string-ref from i)))

        (do ((i (- fend 1)                    (- i 1))
             (j (+ -1 tstart (- fend fstart)) (- j 1)))
            ((< i fstart))
          (string-set! to j (string-ref from i)))))
  (let* ((total (do ((strings strings (cdr strings))
                     (i 0 (+ i (string-length (car strings)))))
                    ((not (pair? strings)) i)))
         (ans (make-string total)))
    (let lp ((i 0) (strings strings))
      (if (pair? strings)
          (let* ((s (car strings))
                 (slen (string-length s)))
            (%string-copy! ans i s 0 slen)
            (lp (+ i slen) (cdr strings)))))
    ans))

;; Join strings
(define (string-join strings #!key (delim " ") (grammar 'infix))
  (let ((buildit (lambda (lis final)
                   (let recur ((lis lis))
                     (if (pair? lis)
                         (cons delim (cons (car lis) (recur (cdr lis))))
                         final)))))
    (cond ((pair? strings)
           (string-concatenate
            (case grammar
              ((infix strict-infix)
               (cons (car strings) (buildit (cdr strings) '())))
              ((prefix) (buildit strings '()))
              ((suffix)
               (cons (car strings) (buildit (cdr strings) (list delim))))
              (else (error "Illegal join grammar"
                           grammar string-join)))))
          ((not (null? strings))
           (error "STRINGS parameter not list." strings string-join))
          ((eq? grammar 'strict-infix)
           (error "Empty list cannot be joined with STRICT-INFIX grammar."
                  string-join))
          (else ""))))

