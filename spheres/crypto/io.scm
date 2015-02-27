;;;============================================================================

;;; File: "cryptio.scm", Time-stamp: <2007-02-26 11:29:40 feeley>

;;; Copyright (C) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(define (ask-for-filename-password filename reason)
  (println "Please enter " reason "password for file " filename ":"))

(define* (enter-filename-password
          filename
          (reason "access control ")
          (prompt "Password: "))
  (ask-for-filename-password filename "access control ")
  (enter-password prompt))

(define* (enter-new-filename-password
          filename
          (reason "access control ")
          (prompt "Password: "))
  (let loop ()
    (ask-for-filename-password filename reason)
    (let ((password1 (enter-password prompt)))
      (println "Please enter the password again.")
      (let ((password2 (enter-password prompt)))
        (if (not (string=? password1 password2))
            (begin
              (println "Passwords are not the same.  Please start over.")
              (loop))
            password1)))))

(define-macro (salt-len) 8)
(define-macro (block-len) 16)
(define-macro (key-len) 16)
(define-macro (iter-count) 100)

(define (read-encrypted-file filename password)
  (let ((econtent (genport-read-file filename)))
    (if (< (u8vector-length econtent)
           (+ (salt-len) (block-len)))
        (error "truncated file")
        (let* ((salt
                (subu8vector
                 econtent
                 0
                 (salt-len)))
               (key
                (PBKDF2 password salt (iter-count) (key-len)))
               (password-check1
                (subu8vector
                 econtent
                 (salt-len)
                 (+ (salt-len) (block-len))))
               (password-check2
                (make-u8vector (block-len))))
          (aes-encrypt-ecb
           (u8vector->aes-context key)
           (u8vector-append salt salt)
           0
           password-check2
           0)
          (if (not (equal? (u8vector->list password-check1)
                           (u8vector->list password-check2)))
              #f
              (RFC1423-unpad
               (aes-decrypt-subu8vector
                econtent
                (+ (salt-len) (block-len))
                (u8vector-length econtent)
                key)
               (block-len)))))))

(define (write-encrypted-file u8vect filename password)
  (let* ((salt
          (make-salt (salt-len)))
         (key
          (PBKDF2 password salt (iter-count) (key-len)))
         (password-check
          (make-u8vector (block-len)))
         (padded-u8vect
          (RFC1423-pad u8vect (block-len)))
         (eu8vect
          (aes-encrypt-u8vector padded-u8vect key)))
    (aes-encrypt-ecb
     (u8vector->aes-context key)
     (u8vector-append salt salt)
     0
     password-check
     0)
    (genport-write-file
     (u8vector-append salt password-check eu8vect)
     filename)))

;;;============================================================================
