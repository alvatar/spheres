;;!!! A simple first-in-first-out queue library. The implementation is
;; list-based and relies on set-car!/set-cdr!. All operations are constant time.
;; .author Per Eckerdal, 2008
;; .author Alvaro Castro-Castilla, 2015

(define-type q
  id: F71EB1A0-828C-48D5-80C3-1CF3628012F9
  data
  (end-cons unprintable:)
  (size unprintable:))

(define (make-end-cons)
  (cons '*queue-end* '()))

(define (make-queue)

  (let ((end-cons (make-end-cons)))
    (make-q end-cons end-cons 0)))

(define (queue-push! q elm)
  (let ((new-end-cons (make-end-cons))
        (old-end-cons (q-end-cons q)))
    (set-car! old-end-cons elm)
    (set-cdr! old-end-cons new-end-cons)
    (q-size-set! q (+ 1 (q-size q)))
    (q-end-cons-set! q new-end-cons)))

(define (queue-pop! q)
  (if (queue-empty? q)
      (error "Queue is empty")
      (let ((data (q-data q)))
        (q-data-set! q (cdr data))
        (q-size-set! q (- (q-size q) 1))
        (car data))))

(define queue-size q-size)

(define (queue-empty? q)
  (= 0 (q-size q)))

(define (queue-empty! q)
  (q-size-set! q 0)
  (let ((end-cons (make-end-cons)))
    (q-data-set! q end-cons)
    (q-end-cons-set! q end-cons)))

(define* (queue-front q (default '*queue-no-value*))
  (if (queue-empty? q)
      (if (eq? default '*queue-no-value*)
          (error "Queue is empty")
          default)
      (car (q-data q))))
