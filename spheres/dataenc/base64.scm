;;; Copyright (c) 2005-2007 by Marc Feeley, All Rights Reserved.
;;; Modifications:
;;; Copyright (c) 2014 by Álvaro Castor-Castilla, All Rights Reserved.

;; TODO: change the interface so that a base64 encoder and decoder is
;; really a generic port.  This way the I/O procedures can be used to
;; feed the content of the message to digest (for example
;; genport-write-subu8vector will subsume subu8vector->base64-string).


;;------------------------------------------------------------------------------
;; Representation of fifos.

(define (fifo-next fifo)
  (cdr fifo))

(define (fifo-next-set! fifo x)
  (set-cdr! fifo x))

(define (fifo-tail fifo)
  (car fifo))

(define (fifo-tail-set! fifo x)
  (set-car! fifo x))

(define (fifo-elem fifo)
  (car fifo))

(define (fifo-elem-set! fifo x)
  (set-car! fifo x))

(define (make-fifo)
  (let ((fifo (cons '() '())))
    (fifo-tail-set! fifo fifo)
    fifo))

(define (fifo-insert-at-tail! fifo elem)
  (let ((x (cons elem '())))
    (let ((tail (fifo-tail fifo)))
      (fifo-next-set! tail x)
      (fifo-tail-set! fifo x))))

(define (fifo->u8vector fifo start end)
  (subu8vector
   (apply-u8vector-append (fifo-next fifo))
   start
   end))

(define (u8vector-shrink u8vect len)
  (subu8vector u8vect 0 len))

(define (fifo->string fifo start end)
  ;; An optimization of (apply string-append ...)
  ;; This function is found in strings: string, but better avoid that
  ;; dependency for such a small function
  (define (apply-string-append lst)
    (define (append-rest-at i lst)
      (if (pair? lst)
          (let* ((src (car lst))
                 (len (string-length src))
                 (dst (append-rest-at (+ i len) (cdr lst))))
            (substring-move! src 0 len dst i)
            dst)
          (make-string i)))
    (append-rest-at 0 lst))
  (substring
   (apply-string-append (fifo-next fifo))
   start
   end))

(define (string-shrink str len)
  (substring str 0 len))

;;------------------------------------------------------------------------------

(define (base64-substring->u8vector str start end)
  (define (err)
    (error "base64 decoding error"))
  (define chunk-len 64) ;; must be a power of 2
  (define state
    (vector 0 (make-fifo)))
  (define (wr-u8 x)
    (let ((ptr (vector-ref state 0)))
      (vector-set! state 0 (+ ptr 1))
      (let ((fifo (vector-ref state 1))
            (i (fxand ptr (- chunk-len 1))))
        (u8vector-set!
         (if (= i 0)
             (let ((chunk (make-u8vector chunk-len)))
               (fifo-insert-at-tail! fifo chunk)
               chunk)
             (fifo-elem (fifo-tail fifo)))
         i
         x))))
  (define (get-output-u8vector)
    (let ((ptr (vector-ref state 0))
          (fifo (vector-ref state 1)))
      (if (and (< 0 ptr) (<= ptr chunk-len))
          (let ((u8vect (fifo-elem (fifo-tail fifo))))
            (u8vector-shrink u8vect ptr))
          (fifo->u8vector fifo 0 ptr))))
  (define (decode c)
    (cond ((and (char>=? c #\A) (char<=? c #\Z))
           (- (char->integer c) (char->integer #\A)))
          ((and (char>=? c #\a) (char<=? c #\z))
           (+ 26 (- (char->integer c) (char->integer #\a))))
          ((and (char>=? c #\0) (char<=? c #\9))
           (+ 52 (- (char->integer c) (char->integer #\0))))
          ((char=? c #\+)
           62)
          ((char=? c #\/)
           63)
          (else
           #f)))
  (define (done)
    (get-output-u8vector))
  (define (add1 x0 x1)
    (add (+ (fxarithmetic-shift-left x0 2)
            (fxarithmetic-shift-right x1 4))))
  (define (add2 x0 x1 x2)
    (add1 x0 x1)
    (add (fxand
          #xff
          (+ (fxarithmetic-shift-left x1 4)
             (fxarithmetic-shift-right x2 2)))))
  (define (add3 x0 x1 x2 x3)
    (add2 x0 x1 x2)
    (add (fxand
          #xff
          (+ (fxarithmetic-shift-left x2 6)
             x3))))
  (define (add x)
    (wr-u8 x))
  (let loop0 ((i start))
    (if (>= i end)
        (done)
        (let* ((c0 (string-ref str i))
               (x0 (decode c0)))
          (if x0
              (let loop1 ((i (+ i 1)))
                (if (>= i end)
                    (err)
                    (let* ((c1 (string-ref str i))
                           (x1 (decode c1)))
                      (if x1
                          (let loop2 ((i (+ i 1)))
                            (if (>= i end)
                                (err)
                                (let* ((c2 (string-ref str i))
                                       (x2 (decode c2)))
                                  (if x2
                                      (let loop3 ((i (+ i 1)))
                                        (if (>= i end)
                                            (err)
                                            (let* ((c3 (string-ref str i))
                                                   (x3 (decode c3)))
                                              (if x3
                                                  (begin
                                                    (add3 x0 x1 x2 x3)
                                                    (loop0 (+ i 1)))
                                                  (if (char=? c3 #\=)
                                                      (begin
                                                        (add2 x0 x1 x2)
                                                        (done))
                                                      (loop3 (+ i 1)))))))
                                      (if (char=? c2 #\=)
                                          (begin
                                            (add1 x0 x1)
                                            (done))
                                          (loop2 (+ i 1)))))))
                          (if (char=? c1 #\=)
                              (err)
                              (loop1 (+ i 1)))))))
              (if (char=? c0 #\=)
                  (err)
                  (loop0 (+ i 1))))))))

(define* (base64-string->u8vector str)
  (base64-substring->u8vector str 0 (string-length str)))

(define* (subu8vector->base64-string u8vect start end (width 0))
  (let ((chunk-len 64)) ;; must be a power of 2
    (define state
      (vector 0
              (make-fifo)))
    (define (wr-char c)
      (let ((ptr (vector-ref state 0)))
        (vector-set! state 0 (+ ptr 1))
        (let ((fifo (vector-ref state 1))
              (i (fxand ptr (- chunk-len 1))))
          (string-set!
           (if (= i 0)
               (let ((chunk (make-string chunk-len)))
                 (fifo-insert-at-tail! fifo chunk)
                 chunk)
               (fifo-elem (fifo-tail fifo)))
           i
           c))))
    (define (get-output-string)
      (let ((ptr (vector-ref state 0))
            (fifo (vector-ref state 1)))
        (if (and (< 0 ptr) (<= ptr chunk-len))
            (let ((str (fifo-elem (fifo-tail fifo))))
              (string-shrink str ptr))
            (fifo->string fifo 0 ptr))))
    (define (add c)
      (wr-char c))
    (define (out x n)
      (let ((new-n
             (cond ((= -1 n)
                    n)
                   ((= 0 n)
                    (add #\newline)
                    (- width 1))
                   (else
                    (- n 1)))))
        (add (cond ((<= x 25)
                    (integer->char (+ x (char->integer #\A))))
                   ((<= x 51)
                    (integer->char (+ (- x 26) (char->integer #\a))))
                   ((<= x 61)
                    (integer->char (+ (- x 52) (char->integer #\0))))
                   ((= x 62)
                    #\+)
                   ((= x 63)
                    #\/)
                   (else
                    #\=)))
        new-n))
    (let loop ((i start)
               (n (if (> width 0) width -1)))
      (if (<= (+ i 3) end)
          (let ((b0 (u8vector-ref u8vect i))
                (b1 (u8vector-ref u8vect (+ i 1)))
                (b2 (u8vector-ref u8vect (+ i 2))))
            (let ((x0
                   (fxarithmetic-shift-right b0 2))
                  (x1
                   (fxand
                    #x3f
                    (+ (fxarithmetic-shift-left b0 4)
                       (fxarithmetic-shift-right b1 4))))
                  (x2
                   (fxand
                    #x3f
                    (+ (fxarithmetic-shift-left b1 2)
                       (fxarithmetic-shift-right b2 6))))
                  (x3
                   (fxand #x3f b2)))
              (loop (+ i 3)
                    (out x3 (out x2 (out x1 (out x0 n)))))))
          (let ((rest (- end i)))
            (cond ((= rest 2)
                   (let ((b0 (u8vector-ref u8vect i))
                         (b1 (u8vector-ref u8vect (+ i 1))))
                     (let ((x0
                            (fxarithmetic-shift-right b0 2))
                           (x1
                            (fxand
                             #x3f
                             (+ (fxarithmetic-shift-left b0 4)
                                (fxarithmetic-shift-right b1 4))))
                           (x2
                            (fxand
                             #x3f
                             (fxarithmetic-shift-left b1 2)))
                           (x3
                            64))
                       (out x3 (out x2 (out x1 (out x0 n)))))))
                  ((= rest 1)
                   (let ((b0 (u8vector-ref u8vect i)))
                     (let ((x0
                            (fxarithmetic-shift-right b0 2))
                           (x1
                            (fxand
                             #x3f
                             (fxarithmetic-shift-left b0 4)))
                           (x2
                            64)
                           (x3
                            64))
                       (out x3 (out x2 (out x1 (out x0 n))))))))
            (get-output-string))))))

(define* (u8vector->base64-string u8vect (width 0))
  (subu8vector->base64-string u8vect 0 (u8vector-length u8vect) width))
