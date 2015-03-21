;;!!! A-list utilities
;; .author Per Eckerdal, 2008
;; .author Mikael More, 2008
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 2008 Per Eckerdal, Mikael Möre

(define (al-helper . args)
  (if (null? args)
      '()
      (let ((key (car args))
            (val (cadr args))
            (rest (cddr args)))
        (cons (cons key val)
              (apply al-helper rest)))))

(define* (al-get lst key (dfl #f))
  (let ((pair (assoc key lst)))
    (if pair
        (cdr pair)
        dfl)))

(define* (al-getq lst key (dfl #f))
  (let ((pair (assq key lst)))
    (if pair
        (cdr pair)
        dfl)))

;; Set key to value in alist al.
;; Replace al with the return value on return.
(define (al-set! al key value)
  (let ((v (or (assoc key al)
               (let ((v (cons key #f)))
                 (set! al (cons v al))
                 v))))
    (set-cdr! v value)
    al))

(define (al-set!-dfl al key value)
  (let* ((not-set '(not-set))
         (v (al-get al key not-set)))
    (if (eq? v not-set)
        (al-set! al key value)
        al)))

