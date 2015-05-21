;;!!! Alternative lazy sequences based on lambdas
;;
;; .author Moritz Heidkamp, 2014
;; .author Alvaro Castro-Castilla, 2015

(define-type lazy-seq
  constructor: %make-lazy-seq
  body
  value)

(define (make-lazy-seq body)
  (%make-lazy-seq body #f))

;; (define-record-printer (lazy-seq seq out)
;;   (display "#<lazy-" out)
;;   (if (lazy-null? seq)
;;       (display "null>" out)
;;       (begin
;;         (display "seq" out)
;;         (let loop ((seq seq))
;;           (if (lazy-seq-realized? seq)
;;               (if (lazy-null? seq)
;;                   (display ">" out)
;;                   (begin
;;                     (display " " out)
;;                     (write (lazy-head seq) out)
;;                     (loop (lazy-tail seq))))
;;               (display " ...>" out))))))

(define (lazy-seq-realized? seq)
  (not (lazy-seq-body seq)))

(define lazy-null
  (lazy-seq '()))

(define (lazy-null? seq)
  (null? (realized-lazy-seq seq)))

(define (realized-lazy-seq seq)
  (or (lazy-seq-value seq)
      (let ((value ((lazy-seq-body seq))))
        (lazy-seq-body-set! seq #f)
        (let ((value (if (lazy-seq? value)
                         ;; No tailcall, deeply nested lazy seqs may blow the stack!
                         (realized-lazy-seq value)
                         value)))
          (lazy-seq-value-set! seq value)
          value))))

(define (lazy-head seq)
  (car (realized-lazy-seq seq)))

(define (lazy-tail seq)
  (cdr (realized-lazy-seq seq)))

(define (lazy-seq->list seq)
  (let loop ((seq seq)
             (result '()))
    (if (lazy-null? seq)
        (reverse! result)
        (loop (lazy-tail seq)
              (cons (lazy-head seq) result)))))

(define (list->lazy-seq lst)
  (let ((head (make-lazy-seq #f '())))
    (let loop ((lst lst) (end head))
      (if (null? lst)
          head
          (let ((new-end (make-lazy-seq #f '())))
            (lazy-seq-value-set! end (cons (car lst) new-end))
            (loop (cdr lst) new-end))))))

(define (lazy-list . elements)
  (list->lazy-seq elements))

(define (string->lazy-seq str)
  (let ((len (string-length str)))
    (let loop ((pos 0))
      (lazy-seq
       (if (= len pos)
           '()
           (cons (string-ref str pos)
                 (loop (+ pos 1))))))))

(define (lazy-seq->string seq)
  (list->string (lazy-seq->list seq)))

(define (lazy-length seq)
  (let loop ((count 0) (seq seq))
    (if (lazy-null? seq)
        count
        (loop (+ count 1) (lazy-tail seq)))))

(define (lazy-take n seq)
  (lazy-seq
   (if (or (zero? n) (lazy-null? seq))
       '()
       (cons (lazy-head seq)
             (lazy-take (- n 1) (lazy-tail seq))))))

(define (lazy-drop n seq)
  (lazy-seq
   (if (or (zero? n) (lazy-null? seq))
       seq
       (lazy-drop (- n 1) (lazy-tail seq)))))

(define (lazy-take-while pred? seq)
  (let loop ((seq seq))
    (lazy-seq
     (cond ((lazy-null? seq) '())
           ((pred? (lazy-head seq))
            (cons (lazy-head seq)
                  (loop (lazy-tail seq))))
           (else '())))))

(define (lazy-drop-while pred? seq)
  (let loop ((seq seq))
    (lazy-seq
     (cond ((lazy-null? seq) '())
           ((pred? (lazy-head seq))
            (loop (lazy-tail seq)))
           (else seq)))))

(define (lazy-numbers #!key (step 1) (start 0) count)
  (let loop ((count count) (start start) (step step))
    (lazy-seq
     (if (and count (zero? count))
         '()
         (cons start
               (loop (and count (- count 1))
                     (+ start step)
                     step))))))

(define (lazy-append . seqs)
  (if (null? seqs)
      lazy-null
      (let loop ((seq (car seqs))
                 (seqs (cdr seqs)))
        (if (lazy-null? seq)
            (if (null? seqs)
                lazy-null
                (loop (car seqs) (cdr seqs)))
            (lazy-seq
             (cons (lazy-head seq)
                   (loop (lazy-tail seq) seqs)))))))

(define (any f lst)
  (let recur ((lst lst))
    (if (null? lst)
        #f
        (if (f (car lst))
            #t
            (recur (cdr lst))))))

(define (reverse! lis)
  (let lp ((lis lis)
           (ans '()))
    (if (null? lis)
        ans
        (let ((tail (cdr lis)))
          (set-cdr! lis ans)
          (lp tail lis)))))

(define (make-lazy-mapping-proc append-result)
  (case-lambda
   ((proc seq)
    (let loop ((seq seq))
      (lazy-seq
       (if (lazy-null? seq)
           '()
           (append-result
            (proc (lazy-head seq))
            (loop (lazy-tail seq)))))))
   ((proc seq . seqs)
    (let loop ((seqs (cons seq seqs)))
      (lazy-seq
       (if (any lazy-null? seqs)
           '()
           (append-result
            (apply proc (map lazy-head seqs))
            (loop (map lazy-tail seqs)))))))))

(define lazy-map
  (make-lazy-mapping-proc cons))

(define lazy-append-map
  (make-lazy-mapping-proc lazy-append))

(define (lazy-filter pred? seq)
  (let loop ((seq seq))
    (lazy-seq
     (if (lazy-null? seq)
         '()
         (let ((head (lazy-head seq))
               (tail (loop (lazy-tail seq))))
           (if (pred? head)
               (cons head tail)
               tail))))))

(define (lazy-ref n seq)
  (if (zero? n)
      (lazy-head seq)
      (lazy-ref (- n 1) (lazy-tail seq))))

(define (lazy-each proc . seqs)
  (unless (any lazy-null? seqs)
          (apply proc (map lazy-head seqs))
          (apply lazy-each proc (map lazy-tail seqs))))

(define (input-port->lazy-seq port read)
  (let loop ()
    (lazy-seq
     (let ((datum (read port)))
       (if (eof-object? datum)
           '()
           (cons datum (loop)))))))

(define (lazy-repeat x)
  (rec seq (lazy-seq (cons x seq))))

(define (lazy-repeatedly f)
  (lazy-seq (cons (f) (lazy-repeatedly f))))

(define (lazy-iterate f x)
  (lazy-seq
   (cons x (lazy-iterate f (f x)))))

(define (lazy-reverse seq)
  (let loop ((seq seq) (rev-seq (lazy-seq '())))
    (lazy-seq
     (if (lazy-null? seq)
         rev-seq
         (loop (lazy-tail seq)
               (make-lazy-seq #f (cons (lazy-head seq) rev-seq)))))))

(define (lazy-cycle seq)
  (lazy-seq
   (if (lazy-null? seq)
       '()
       (let loop ((rest seq))
         (lazy-seq
          (if (lazy-null? rest)
              (loop seq)
              (cons (lazy-head rest)
                    (loop (lazy-tail rest)))))))))

(define (lazy-fold kons init seq)
  (if (lazy-null? seq)
      init
      (lazy-fold kons
                 (kons (lazy-head seq) init)
                 (lazy-tail seq))))

(define (lazy-concatenate seq)
  (lazy-append-map values seq))

(define (lazy-flatten seq)
  (define (flatten curr stack)
    (lazy-seq
     (cond
      ((lazy-null? curr)
       (if (null? stack)
           '()
           (flatten (car stack) (cdr stack))))
      ((lazy-seq? (lazy-head curr))
       (flatten (lazy-head curr)
                (cons (lazy-tail curr) stack)))
      (else
       (cons (lazy-head curr)
             (flatten (lazy-tail curr) stack))))))
  (flatten lazy-null (list seq)))


