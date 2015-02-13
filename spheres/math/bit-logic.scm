;;!!! SRFI-60 Integers as Bits


;; Most of the API is implemented by Gambit under different names

;;!! Bitwise operations
(define logand bitwise-and)
(define logior bitwise-ior)
(define logxor bitwise-xor)
(define lognot bitwise-not)
(define logtest any-bits-set?)

;;! bitwise-if and bitwise-merge are defined with complementary mask
;; i.e. switch parameters
(define (bitwise-merge mask a b)
  (##bitwise-merge mask b a))
(define bitwise-if bitwise-merge)

;;!! Integer properties
(define logcount bit-count)
;; integerlength (native)
(define log2-binary-factors first-bit-set)
(define first-set-bit first-bit-set)

;;!! Bit within word
(define logbit? bit-set?)
(define (copy-bit index to bool)
  (if bool
      (logior to (arithmetic-shift 1 index))
      (logand to (lognot (arithmetic-shift 1 index)))))

;;!! Field of bits
(define (bit-field n start end)
  (logand (lognot (ash -1 (- end start)))
          (arithmetic-shift n (- start))))

;; copy-bit-field (native)
(define ash arithmetic-shift)

(define (rotate-bit-field n count start end)
  (define width (- end start))
  (set! count (modulo count width))
  (let ((mask (lognot (ash -1 width))))
    (define zn (logand mask (arithmetic-shift n (- start))))
    (logior (arithmetic-shift
             (logior (logand mask (arithmetic-shift zn count))
                     (arithmetic-shift zn (- count width)))
             start)
            (logand (lognot (ash mask start)) n))))

(define (reverse-bit-field n start end)
  (define width (- end start))
  (define (bit-reverse k n)
    (do ((m (if (negative? n) (lognot n) n) (arithmetic-shift m -1))
         (k (+ -1 k) (+ -1 k))
         (rvs 0 (logior (arithmetic-shift rvs 1) (logand 1 m))))
        ((negative? k) (if (negative? n) (lognot rvs) rvs))))
  (let ((mask (lognot (ash -1 width))))
    (define zn (logand mask (arithmetic-shift n (- start))))
    (logior (arithmetic-shift (bit-reverse width zn) start)
            (logand (lognot (ash mask start)) n))))

;;!! Bits as booleans
(define (integer->list k . len)
  (if (null? len)
      (do ((k k (arithmetic-shift k -1))
           (lst '() (cons (odd? k) lst)))
          ((<= k 0) lst))
      (do ((idx (+ -1 (car len)) (+ -1 idx))
           (k k (arithmetic-shift k -1))
           (lst '() (cons (odd? k) lst)))
          ((negative? idx) lst))))

(define (list->integer bools)
  (do ((bs bools (cdr bs))
       (acc 0 (+ acc acc (if (car bs) 1 0))))
      ((null? bs) acc)))

(define (booleans->integer . bools)
  (list->integer bools))
