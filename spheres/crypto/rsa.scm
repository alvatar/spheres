;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.
;;; license: lgpl/v2.1
;;; Modifications:
;;; Copyright (c) 2014 by Álvaro Castor-Castilla, All Rights Reserved.
;;; Provides procedures to encrypt, decrypt, sign and verify messages
;;; using the RSA public-key cryptosystem.


(cond-expand
 ((and gambit optimize)
  (declare (fixnum)))
 (else (void)))

;; (##namespace (""
;;               ISO-8859-1-string->u8vector
;;               apply-u8vector-append
;;               bignum->u8vector
;;               expt-mod
;;               modulo-inverse
;;               random-prime
;;               u8vector->bignum 
;;               ))

;;------------------------------------------------------------------------------
;;!! Generation of RSA public and private key pairs.

(define-type rsa-key
  ;; uid: rsa-key-973409ff-46e7-4643-8e26-f5a2e6314956
  size
  modulus ;; (expt 2 size) <= modulus < (expt 2 (+ size 1))
  exponent)

(define* (make-rsa-key-pair size (show-trace #f))
  (let* ((size-p (quotient size 2))
         (start-p (expt 2 size-p))
         (end-p (* start-p 2))
         (p (random-prime start-p end-p show-trace))
         (start-n (expt 2 size))
         (end-n (* start-n 2))
         (start-q (+ (quotient (- start-n 1) p) 1))
         (end-q (quotient end-n p)))
    (let loop ()
      (let ((q (random-prime start-q end-q show-trace)))
        (if (not (= (gcd p q) 1))
            (loop)
            (let* ((n (* p q))
                   (p-1 (- p 1))
                   (q-1 (- q 1))
                   (phi (quotient (* p-1 q-1)
                                  (gcd p-1 q-1)))
                   (e (let loop ((e 65537))
                        (if (= 1 (gcd e phi))
                            e
                            (loop (+ e 2)))))
                   (d (modulo-inverse e phi)))
              (cons (make-rsa-key size n e) ;; public and private keys
                    (make-rsa-key size n d))))))))

(define (public-rsa-key rsa-key-pair)
  (car rsa-key-pair))

(define (private-rsa-key rsa-key-pair)
  (cdr rsa-key-pair))

;;! Key comparison
(define (rsa-key= rsa-key1 rsa-key2)
  (and (= (rsa-key-size rsa-key1)
          (rsa-key-size rsa-key2))
       (= (rsa-key-modulus rsa-key1)
          (rsa-key-modulus rsa-key2))
       (= (rsa-key-exponent rsa-key1)
          (rsa-key-exponent rsa-key2))))

;;! Conversion of RSA keys for serialization
(define (rsa-key->list rsa-key)
  (define (bignum->base64-string n)
    (u8vector->base64-string (bignum->u8vector n)))
  (list (rsa-key-size rsa-key)
        (bignum->base64-string (rsa-key-modulus rsa-key))
        (bignum->base64-string (rsa-key-exponent rsa-key))))

;;! Conversion of RSA keys for deserialization.
(define (list->rsa-key lst)
  (define (base64-string->bignum str)
    (u8vector->bignum (base64-string->u8vector str)))
  (if (not (and (list? lst) (= 3 (length lst))))
      (error "improperly formatted RSA key")
      (let* ((size
              (car lst))
             (modulus-str
              (cadr lst))
             (exponent-str
              (caddr lst))
             (modulus
              (and (string? modulus-str)
                   (base64-string->bignum modulus-str)))
             (exponent
              (and (string? exponent-str)
                   (base64-string->bignum exponent-str))))
        (if (not (and (memv size '(512 1024 2048))
                      modulus
                      exponent))
            (error "improperly formatted RSA key")
            (make-rsa-key size modulus exponent)))))

;;;----------------------------------------------------------------------------
;;!! Message padding and unpadding.

(define (PKCS1-pad u8vect final-len)
  (define (err)
    (error "not enough space is available for proper padding"))
  (let* ((len (u8vector-length u8vect))
         (n (- final-len (+ len 3))))
    (if (< n 8)
        (err)
        (let ((pad
               (let loop ((lst '(0)) (i 0))
                 (if (< i n)
                     (loop (cons (+ 1 (random-integer 255)) lst) (+ i 1))
                     (list->u8vector (cons 0 (cons 2 lst)))))))
          (u8vector-append pad u8vect)))))

(define (PKCS1-unpad u8vect)
  (define (err)
    (error "improperly padded message"))
  (let ((len (u8vector-length u8vect)))
    (let loop1 ((i 0))
      (if (>= i len)
          (err)
          (let ((x (u8vector-ref u8vect i)))
            (cond ((= x 0)
                   (loop1 (+ i 1)))
                  ((not (= x 2))
                   (err))
                  (else
                   (let loop2 ((j (+ i 1)))
                     (if (>= j len)
                         (err)
                         (let ((x (u8vector-ref u8vect j)))
                           (cond ((not (= x 0))
                                  (loop2 (+ j 1)))
                                 ((< (- j i) 8) ;; need at least 8 byte pad
                                  (err))
                                 (else
                                  (subu8vector
                                   u8vect
                                   (+ j 1)
                                   len)))))))))))))

;;------------------------------------------------------------------------------
;;!! Message encryption and decryption.

(define (rsa-crypt message rsa-key) ;; encryption and decryption
  (expt-mod
   message
   (rsa-key-exponent rsa-key)
   (rsa-key-modulus rsa-key)))

(define (rsa-encrypt-u8vector u8vect rsa-key final-len)
  (bignum->u8vector
   (rsa-crypt
    (u8vector->bignum (PKCS1-pad u8vect final-len))
    rsa-key)))

(define (rsa-decrypt-u8vector u8vect rsa-key)
  (PKCS1-unpad
   (bignum->u8vector
    (rsa-crypt
     (u8vector->bignum u8vect)
     rsa-key))))

;;;-----------------------------------------------------------------------------
;;!! Implementation of a subset of RFC 2898 (PKCS #5: Password-Based
;; Cryptography Specification Version 2.0).

(define (get-u8vector-password password)
  (if (string? password)
      (ISO-8859-1-string->u8vector password)
      password))

(define* (make-salt (len 8)) ;; default is 64 bit salt
  (random-u8vector len))

(define (PBKDF1 password salt iter-count len)
  (if (> len 20)
      (error "derived key too long")
      (let ((algorithm (if (> len 16) 'sha-1 'md5))
            (password (get-u8vector-password password)))
        (let loop ((k 0)
                   (t (u8vector-append password salt)))
          (if (< k iter-count)
              (loop (+ k 1)
                    (digest-u8vector t algorithm 'u8vector))
              (subu8vector t 0 len))))))

(define (PBKDF2 password salt iter-count len)
  (define (u32-be i)
    (u8vector
     (fxand #xff (fxarithmetic-shift-right i 24))
     (fxand #xff (fxarithmetic-shift-right i 16))
     (fxand #xff (fxarithmetic-shift-right i  8))
     (fxand #xff i)))
  (define (PRF password salt)
    ;; TODO: should really be using HMAC-SHA-1 (see RFC 2898)
    (digest-u8vector
     (u8vector-append password salt)
     'sha-1
     'u8vector))
  (define (F password salt iter-count i)
    (let ((x (PRF password (u8vector-append salt (u32-be i)))))
      (let loop ((k 1) (u x) (t x))
        (if (< k iter-count)
            (let ((x (PRF password u)))
              (loop (+ k 1) x (u8vector-xor t x)))
            t))))
  (if (> len 65536) ;; arbitrary limit, more than enough for most purposes
      (error "derived key too long")
      (let ((password (get-u8vector-password password)))
        (let loop ((i 1)
                   (n len)
                   (lst '()))
          (if (> n 0)
              (let* ((t (F password salt iter-count i))
                     (x (u8vector-length t)))
                (loop (+ i 1)
                      (- n x)
                      (cons (if (> n x)
                                t
                                (subu8vector t 0 n))
                            lst)))
              (apply-u8vector-append (reverse lst)))))))
