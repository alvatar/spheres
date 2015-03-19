;;!!! Extra string utilities
;; .author Per Eckerdal
;; .author Mikael More
;; .author Marc Feeley
;; .author Alvaro Castro-Castilla
;;
;; Copyright (c) 2008 Per Eckerdal and Mikael Möre



(define (char->string char)
  (make-string 1 char))

;; private
(define-macro (string-extra#push! var val)
  `(set! ,var (cons ,val ,var)))

;; private
(define-macro (string-extra#with-symbol sym . thunk)
  (let ((gs (gensym)))
    `(let* ((,gs (symbol? ,sym))
            (,sym (if ,gs
                      (symbol->string ,sym)
                      ,sym))
            (res (begin ,@thunk)))
       (if ,gs
           (string->symbol res)
           res))))

(define (string-strip str)
  (let* ((len (string-length str))
         (start
          (let loop ((start 0))
            (if (and (< start len)
                     (char-whitespace? (string-ref str start)))
                (loop (+ 1 start))
                start)))
         (end
          (let loop ((end (- len 1)))
            (if (and (>= end 0)
                     (char-whitespace? (string-ref str end)))
                (loop (- end 1))
                (+ 1 end)))))
    (substring str start end)))

(define (string-replace-char str from to)
  (string-map (lambda (chr)
                (if (eq? chr from) to chr))
              str))

(define* (string-split-char chr str (sparse #f))
  (let* ((curr-str '())
         (result '())
         (new-str (lambda ()
                    (string-extra#push! result (reverse-list->string curr-str))
                    (set! curr-str '())))
         (add-char (lambda (chr)
                     (string-extra#push! curr-str chr))))
    (string-for-each (lambda (c)
                       (cond
                        ((eq? c chr)
                         (if (or (not sparse) (not (null? curr-str))) (new-str)))
                        (else
                         (add-char c)))) str)
    (new-str)
    (reverse result)))

;; returns a pair.
(define* (string-split-at-first at-char
                                s
                                (not-found
                                 (lambda ()
                                   (error "String parameter didn't contain char."))))
  (let ((pos (string-index s at-char)))
    (if (not pos)
        (not-found)
        (cons (substring s 0 pos) (substring s (+ pos 1) (string-length s))))))

(define (string-split-at-first-nice at-char s)
  (string-split-at-first at-char s (lambda () #f)))

(define (string-name-split str)
  (let* ((curr-str '())
         (result '())
         (new-str (lambda ()
                    (if (not (null? curr-str))
                        (begin
                          (string-extra#push! result (reverse-list->string curr-str))
                          (set! curr-str '())))))
         (add-char (lambda (chr)
                     (string-extra#push! curr-str chr))))
    (string-for-each (lambda (chr)
                       (cond
                        ((char-upper-case? chr)
                         (new-str)
                         (add-char chr))
                        ((or (eq? chr #\ ) (eq? chr #\-))
                         (new-str))
                        (else
                         (add-char chr))))
                     str)
    (new-str)
    (reverse result)))

(define (string-set-first-char! fun str)
  (if (eq? 0 (string-length str))
      str
      (begin
        (string-set! str 0 (fun (string-ref str 0)))
        str)))

(define (string-capitalize! str)
  (string-extra#with-symbol str
                            (string-set-first-char! char-upcase str)))

(define (string-decapitalize! str)
  (string-extra#with-symbol str
                            (string-set-first-char! char-downcase str)))

(define (string-constantize str)
  (string-extra#with-symbol str
                            (string-capitalize! (string-camelize str))))

(define (string-camelize str)
  (string-extra#with-symbol str
                            (let ((split (string-name-split str)))
                              (apply string-append
                                     (cons (car split) (map string-capitalize! (cdr split)))))))

;; private
(define (make-string-x-ize chr)
  (lambda (str)
    (define (join between args)
      (cond ((null? args) '())
            ((null? (cdr args)) (list (car args)))
            (else `(,(car args) ,between ,@(join between (cdr args))))))
    (string-extra#with-symbol str
                              (apply string-append
                                     (join chr
                                           (map string-decapitalize!
                                                (string-name-split str)))))))

(define string-dasherize (make-string-x-ize "-"))

(define string-spaceize (make-string-x-ize " "))

(define (string-humanize str)
  (string-capitalize! (string-spaceize str)))

;; Per Eckerdal's versions
;; (define (string-remove-suffix haystack needle)
;;   (if (string-suffix? needle haystack)
;;       (substring haystack 0 (- (string-length haystack)
;;                                (string-length needle)))
;;       haystack))
;; (define (string-remove-prefix haystack needle)
;;   (if (string-prefix? needle haystack)
;;       (substring haystack
;;                  (string-length needle)
;;                  (string-length haystack))
;;       haystack))
;;
;; Marc Feeley's versions
;; (define (string-remove-prefix str prefix)
;;   (let ((len-str (string-length str))
;;         (len-prefix (string-length prefix)))
;;     (if (and (<= len-prefix len-str)
;;              (string=? (substring str 0 len-prefix) prefix))
;;         (substring str len-prefix len-str)
;;         #f)))
;; (define (string-remove-suffix str suffix)
;;   (let ((len-str (string-length str))
;;         (len-suffix (string-length suffix)))
;;     (if (and (<= len-suffix len-str)
;;              (string=? (substring str (- len-str len-suffix) len-str) suffix))
;;         (substring str 0 (- len-str len-suffix))
;;         #f)))

;; Alvaro Castro-Castilla
(define* (string-remove-prefix str prefix (if-no-prefix str))
  (let ((len-str (string-length str))
        (len-prefix (string-length prefix)))
    (if (and (<= len-prefix len-str)
             (string=? (substring str 0 len-prefix) prefix))
        (substring str len-prefix len-str)
        if-no-prefix)))
(define* (string-remove-suffix str suffix (if-no-suffix str))
  (let ((len-str (string-length str))
        (len-suffix (string-length suffix)))
    (if (and (<= len-suffix len-str)
             (string=? (substring str (- len-str len-suffix) len-str) suffix))
        (substring str 0 (- len-str len-suffix))
        if-no-suffix)))

(define* (string-pluralize str (num 0))
  (if (= num 1)
      str
      (string-extra#with-symbol str
                                (if (string-suffix? "s" str)
                                    str
                                    (string-append str "s")))))

(define (string-depluralize str)
  (string-extra#with-symbol str
                            (if (string-suffix? "s" str)
                                (string-remove-suffix str "s")
                                str)))

(define (string-extra#u8vector-invert! v)
  (let loop ((i (u8vector-length v)))
    (if (not (zero? i))
        (let ((i (- i 1)))
          (u8vector-set! v i (##fixnum.bitwise-xor
                              (u8vector-ref v i)
                              255))
          (loop i)))))

;; Returns a string that sorts in the opposite order in a binary
(define (string-invert s)
  (let ((v (string->utf8-u8vector s)))
    (string-extra#u8vector-invert! v)
    (u8vector->hex-string v)))

(define (string-uninvert s)
  (let ((v (hex-string->u8vector s)))
    (string-extra#u8vector-invert! v)
    (u8vector->string v)))

(define (dumps . params)
  (define (sub params port)
    (if (not (null? params))
        (let ((ce (car params)))
          ((if (string? ce)
               display
               write)
           ce
           port)
          (sub (cdr params) port))))
  (let ((port (open-output-string)))
    (sub params port)
    (let ((output (get-output-string port)))
      (close-output-port port)
      output)))
