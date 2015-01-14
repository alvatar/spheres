;; Hash-based Message Authentication Code module
;; Copyright (C) 2014 Mikael More
;; MIT license.
;;
;; Described in RFC 2104 and test vectors provided in RFC 4231.
;;
;; We don't provide a CRC32 HMAC for now, in lack of test vectors.
;;
;; ## Description of the HMAC algorithm
;; The algorithm for computing a HMAC for a given key and hash algorithm follows:
;;  * If the key is longer than the hash algorithm's block size, then use the hash of the key as key.
;;  * Append 0x00:s to the end of the key as to align it with the hash algorithm's block size.
;;  * Generate an inner key padding, which is each byte of the key XOR:ed with 0x5C. (as clarified in hmac.scm and hmac.js)
;;  * Generate an outer key padding, which is each byte of the key XOR:ed with 0x36. (")
;;  * The HMAC is now computed by:
;;    (hash (u8vector-append outer-key-padding (hash (u8vector-append inner-key-padding message))))
;;    Note that the inner hash should return binary whereas the outer according to user's choice.
;;
;; ## References
;;  * RFC 2104: HMAC: Keyed-Hashing for Message Authentication
;;    https://tools.ietf.org/html/rfc2104
;;
;;  * RFC 4231: Identifiers and Test Vectors for HMAC-SHA-224, HMAC-SHA-256,
;;              HMAC-SHA-384, and HMAC-SHA-512
;;    http://tools.ietf.org/search/rfc4231
;;
;;  * https://en.wikipedia.org/wiki/Hash-based_message_authentication_code#Implementation
;;
;;  * https://github.com/ThomasHintz/chicken-scheme-hmac/blob/master/hmac.scm
;;
;;  * https://code.google.com/p/crypto-js/source/browse/tags/3.1.2/src/hmac.js
;;
;;  * NaCL of http://nacl.cr.yp.to/install.html in crypto_auth/hmacsha256/ref/hmac.c
;;
;; ## TODO
;;  * For elegance: The hmac-ansi-* routines are not optimally implemented, see source for notes.

(cond-expand
 (optimize
  (declare (fixnum)))
 (else (void)))

;; To be moved to the digest module??
(define (digest-algorithm-block-size algorithm)
  (case algorithm
    ;; ((crc32) 4) ; ?
    ((md5) 64) ;; The value of 64 here is required to accord with the example value at
    ;; https://en.wikipedia.org/wiki/Hash-based_message_authentication_code#Examples_of_HMAC_.28MD5.2C_SHA1.2C_SHA256.29 .
    ;; https://en.wikipedia.org/wiki/SHA-2#Comparison_of_SHA_functions
    ((sha-1) 64)
    ((sha-224) 64)
    ((sha-256) 64)
    (else
     (error "Unknown algorithm" algorithm))))

;;(define-macro (hmac-default-result-type) ''hex)
(define-syntax hmac-default-result-type
  (syntax-rules () ((_) 'hex)))

;; Uses the variables |block-size| |key-u8vect| |algorithm|
(define-macro (hmac-key&inner&outer-key-padding . code)
  `(let* ((key-u8vect (if (> (u8vector-length key-u8vect) block-size)
                          (digest-u8vector key-u8vect algorithm 'u8vector)
                          key-u8vect))
          (key-u8vect (u8vector-pad-to-length key-u8vect block-size))
          (inner-key-padding (u8vector-xor/byte key-u8vect #x36))
          (outer-key-padding (u8vector-xor/byte key-u8vect #x5C)))
     ,@code))

(define (hmac-debug:actual-key&inner&outer-key-padding block-size key-u8vect algorithm)
  (hmac-key&inner&outer-key-padding
   (list key-u8vect inner-key-padding outer-key-padding)))

(define (perform-hmac message message-start message-end digest-update-procedure key-u8vect algorithm result-type)
  (let* ((inner-digest-digest (open-digest algorithm))
         (block-size (digest-algorithm-block-size algorithm)))
    (hmac-key&inner&outer-key-padding
     ;; Simpler but slower impl:
     ;; (inner-digest (digest-u8vector (u8vector-append inner-key-padding
     ;;                                                 (subu8vector message message-start message-end))
     ;;                                algorithm
     ;;                                'u8vector
     ;;                                ))
     (let* ((inner-digest
             (let ((d inner-digest-digest))
               ;; We know inner-key-padding has the length of the block size.
               (digest-update-subu8vector d inner-key-padding 0 block-size)
               (digest-update-subu8vector d message message-start message-end)
               ;; Return u8vector - this was clarified by the #crypto channel on FreeNode.
               (close-digest d 'u8vector)))
            ;; Simpler but slower impl:
            ;; (outer-digest (digest-u8vector (u8vector-append outer-key-padding
            ;;                                                 inner-digest)
            ;;                                algorithm
            ;;                                result-type))
            (outer-digest
             (let ((d (open-digest algorithm)))
               (digest-update-subu8vector d outer-key-padding 0 block-size) ; We know inner-key-padding has the length of the block size.
               (digest-update-subu8vector d inner-digest 0 (u8vector-length inner-digest))
               (close-digest d result-type))))
       outer-digest))))

(define* (hmac-u8vector message-u8vect
                       key-u8vect
                       algorithm
                       (result-type (hmac-default-result-type)))
  (hmac-subu8vector message-u8vect 0 (u8vector-length message-u8vect) key-u8vect algorithm result-type))

(define* (hmac-subu8vector message-u8vect
                          message-start
                          message-end
                          key-u8vect
                          algorithm
                          (result-type (hmac-default-result-type)))
  (perform-hmac message-u8vect message-start message-end
                digest-update-subu8vector ; = digest-update-procedure
                key-u8vect algorithm result-type))

;; See the TODO for some notes.
(define* (hmac-ansi-string message-ansi-string
                          key-ansi-string
                          algorithm
                          (result-type (hmac-default-result-type)))
  (hmac-ansi-substring message-ansi-string 0 (string-length message-ansi-string) key-ansi-string algorithm result-type))

(define* (hmac-ansi-substring message-ansi-string
                             message-start
                             message-end
                             key-ansi-string
                             algorithm
                             (result-type (hmac-default-result-type)))
  ;; This is a copy of |digest-ansi-substring|'s code.
  (let* ((str message-ansi-string)
        (start message-start)
        (end message-end)
        (len (fx- end start)))
    (if (or (> end (string-length str))
            (> start end))
        (error "Out of range" start end)
        (let ((u8vect (make-u8vector len)))
          ;; We're just absolutely sure this code is bugfree, so no need for type safety here.
          (let ()
            (declare (not safe))
            (let loop ((in-idx start) (out-idx 0))
              (if (fx< out-idx len)
                  (begin
                    (u8vector-set! u8vect out-idx (char->integer (string-ref str in-idx)))
                    (loop (fx+ in-idx 1) (fx+ out-idx 1))))))
          (perform-hmac u8vect 0 len ; message-ansi-string message-start message-end
                        digest-update-subu8vector ; = digest-update-procedure
                        (ISO-8859-1-string->u8vector key-ansi-string) ; = key-u8vect - makes sense?
                        algorithm result-type)))))

;; hmac-string* is implemented in terms of |hmac-u8vector|.
(define* (hmac-string message-string
                      key-string
                      algorithm
                      (result-type (hmac-default-result-type)))
  (hmac-substring message-string 0 (string-length message-string) key-string algorithm result-type))

(define* (hmac-substring message-string
                         message-start
                         message-end
                         key-string
                         algorithm
                         (result-type (hmac-default-result-type)))
  (hmac-u8vector (substring->utf8-u8vector message-string message-start message-end)
                 (string->utf8-u8vector key-string)
                 algorithm
                 result-type))

;;! Conveniency wrappers
(define (make-conveniency-hmac-wrapper algorithm)
  (lambda* (value key (start #f) (end #f))
           (if (string? value)
               (if start
                   (hmac-substring   value start end key algorithm)
                   (hmac-string      value           key algorithm))
               (if start
                   (hmac-subu8vector value start end key algorithm)
                   (hmac-u8vector    value           key algorithm)))))

(define hmac-crc32
  (make-conveniency-hmac-wrapper 'crc32))

(define hmac-md5
  (make-conveniency-hmac-wrapper 'md5))

(define hmac-sha-1
  (make-conveniency-hmac-wrapper 'sha-1))

(define hmac-sha-224
  (make-conveniency-hmac-wrapper 'sha-224))

(define hmac-sha-256
  (make-conveniency-hmac-wrapper 'sha-256))
