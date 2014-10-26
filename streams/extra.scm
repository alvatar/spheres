;;; Copyright (c) 2008, Kon Lovett. All rights reserved.
;;; Copyright (c) 2012-2014, Alvaro Castro-Castilla. All rights reserved.
;;; Extra algorithms on streams, adds functionality to SRFI-41

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

(define-stream (stream-intersperse yy x)
  (stream-match yy
                (()
                 (stream (stream x)))
                ((y . ys)
                 (stream-append
                  (stream (stream-cons x yy))
                  (stream-map (lambda (z) (stream-cons y z))
                              (stream-intersperse ys x))))))

(define-stream (stream-permutations xs)
  (define (right-section fn . args)
    (lambda xs (apply fn (append xs args))) )
  (if (stream-null? xs)
      (stream (stream))
      (stream-concat
       (stream-map (right-section stream-intersperse (stream-car xs))
                   (stream-permutations (stream-cdr xs))))))

;; (define-stream (file->stream filename #!optional (reader read-char))
;;   (let ((port (open-input-file filename)))
;;     (stream-let loop ((obj (reader port)))
;;                 (if (eof-object? obj)
;;                     (begin (close-input-port port) stream-null)
;;                     (stream-cons obj (loop (reader port)))))))

(define (stream-split n strm)
  (values (stream-take n strm) (stream-drop n strm)))

(define-stream (stream-unique eql? strm)
  (if (stream-null? strm)
      stream-null
      (stream-cons
       (stream-car strm)
       (stream-unique eql?
                      (stream-drop-while (lambda (x) (eql? (stream-car strm) x)) strm)))))

(define (stream-fold-one func strm)
  (stream-fold func (stream-car strm) (stream-cdr strm)))

(define-stream (stream-member eql? obj strm)
  (stream-let loop ((strm strm))
              (cond ((stream-null? strm) #f)
                    ((eql? obj (stream-car strm)) strm)
                    (else
                     (loop (stream-cdr strm))))))

(define-stream (stream-merge lt? . strms)
  (define-stream (merge xx yy)
    (stream-match xx
      (() yy)
      ((x . xs)
        (stream-match yy
          (() xx)
          ((y . ys)
            (if (lt? y x)
                (stream-cons y (merge xx ys))
                (stream-cons x (merge xs yy))))))))
  (stream-let loop ((strms strms))
    (cond ((null? strms)        stream-null)
          ((null? (cdr strms))  (car strms))
          (else
            (merge (car strms) (apply stream-merge lt? (cdr strms)))))))

(define (stream-partition pred? strm)
  (stream-unfolds
   (lambda (s)
     (if (stream-null? s)
         (values s '() '())
         (let ((a (stream-car s))
               (d (stream-cdr s)))
           (if (pred? a)
               (values d (list a) #f)
               (values d #f (list a))))))
   strm))

(define-stream (stream-finds eql? obj strm)
  (stream-of (car x)
             (x in (stream-zip (stream-from 0) strm))
             (eql? obj (cadr x))))

(define (stream-find eql? obj strm)
  (stream-car (stream-append (stream-finds eql? obj strm) (stream #f))))

(define-stream (stream-remove pred? strm)
  (define (complement f)
    (lambda args (not (apply f args))))
  (stream-filter (complement pred?) strm))

(define (stream-every pred? strm)
  (let loop ((strm strm))
    (cond ((stream-null? strm) #t)
          ((not (pred? (stream-car strm))) #f)
          (else
           (loop (stream-cdr strm))))))

(define (stream-any pred? strm)
  (let loop ((strm strm))
    (cond ((stream-null? strm) #f)
          ((pred? (stream-car strm)) #t)
          (else
           (loop (stream-cdr strm))))))

(define (stream-and strm)
  (let loop ((strm strm))
    (cond ((stream-null? strm) #t)
          ((not (stream-car strm)) #f)
          (else
           (loop (stream-cdr strm))))))

(define (stream-or strm)
  (let loop ((strm strm))
    (cond ((stream-null? strm) #f)
          ((stream-car strm) #t)
          (else
           (loop (stream-cdr strm))))))

(define (stream-fold-right func base strm)
  (let loop ((base base) (strm strm))
    (if (stream-null? strm)
        base
        (func (stream-car strm) (loop base (stream-cdr strm))))))

(define (stream-fold-right-one func strm)
  (stream-match strm
                ((x)
                 x)
                ((x . xs)
                 (func x (stream-fold-right-one func xs)))))

(define (stream-assoc key dict)
  (cond ((stream-null? dict) #f)
        ((equal? key (car (stream-car dict))) (stream-car dict))
        (else
         (stream-assoc key (stream-cdr dict)))))

(define (stream-equal? eql? xs ys)
  (cond ((and (stream-null? xs) (stream-null? ys)) #t)
        ((or (stream-null? xs) (stream-null? ys)) #f)
        ((not (eql? (stream-car xs) (stream-car ys))) #f)
        (else
         (stream-equal? eql? (stream-cdr xs) (stream-cdr ys)))))

(define-stream (stream-quick-sort lt? strm)
  (let loop ([strm strm])
    (if (stream-null? strm)
        stream-null
        (let ((x (stream-car strm))
              (xs (stream-cdr strm)))
          (stream-append
           (loop (stream-filter (lambda (u) (lt? u x)) xs))
           (stream x)
           (loop (stream-filter (lambda (u) (not (lt? u x))) xs)))))))

(define-stream (stream-insertion-sort lt? strm)
  (define-stream (insert strm x)
    (stream-match strm
                  (()
                   (stream x))
                  ((y . ys)
                   (if (lt? y x)
                       (stream-cons y (insert ys x))
                       (stream-cons x strm)))))
  (stream-fold insert stream-null strm))

;; (define-stream (stream-merge-sort lt? strm)
;;   (let loop ([strm strm])
;;     (let ((n (quotient (stream-length strm) 2)))
;;       (if (zero? n)
;;           strm
;;           (stream-merge lt? (loop (stream-take n strm)) (loop (stream-drop n strm)))))))

(define (stream-maximum lt? strm)
  (stream-fold-one (lambda (x y) (if (lt? x y) y x)) strm))

(define (stream-minimum lt? strm)
  (stream-fold-one (lambda (x y) (if (lt? x y) x y)) strm))
