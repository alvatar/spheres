;;;============================================================================

;;; File: "rfc1423.scm", Time-stamp: <2007-04-05 00:52:53 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Generalized message padding/unpadding from RFC 1423 (Privacy
;; Enhancement for Internet Electronic Mail: Part III: Algorithms,
;; Modes, and Identifiers).

(define* (RFC1423-pad u8vect (multiple 8))
  (if (or (<= multiple 0) (>= multiple 256))
      (error "illegal padding multiple")
      (let* ((len (u8vector-length u8vect))
             (n (+ multiple (remainder (- len) multiple))))
        (u8vector-append u8vect (make-u8vector n n)))))

(define* (RFC1423-unpad u8vect (multiple 8))
  (if (or (<= multiple 0) (>= multiple 256))
      (error "illegal padding multiple")
      (let ((len (u8vector-length u8vect)))
        (if (or (< len multiple)
                (not (= 0 (modulo len multiple))))
            (error "improperly padded u8vector")
            (let ((n (u8vector-ref u8vect (- len 1))))
              (if (or (= n 0) (> n multiple))
                  (error "improperly padded u8vector")
                  (let loop ((i n))
                    (if (>= i 2)
                        (if (not (= n (u8vector-ref u8vect (- len i))))
                            (error "improperly padded u8vector")
                            (loop (- i 1)))
                        (subu8vector u8vect 0 (- len n))))))))))

;;;============================================================================
