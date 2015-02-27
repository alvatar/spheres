;; File: "ttyui.scm", Time-stamp: <2007-04-05 00:54:59 feeley>
;; Modifications (REPL support), Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

(define (flush-console-input port)
  (input-port-timeout-set! port 0.001)
  (let loop () (if (not (eof-object? (read-line port))) (loop)))
  (input-port-timeout-set! port +inf.0))

(define (read-line-from-console)
  (let ((port (console-port)))
    (flush-console-input port)
    (read-line port)))

(define (tty-read-line-ascii)
  (let ((port (console-port)))
    (flush-console-input port)
    (let loop ((lst '()))
      (let ((c (read-char port)))
        (if (and (char? c)
                 (not (char=? c #\newline)))
            (if (or (char<? c #\space) (char>? c #\~))
                (loop lst)
                (loop (cons c lst)))
            (list->string (reverse lst)))))))

(define (tty-read-line-ascii-no-echo)
  ;; TODO: turn off echo!
  (tty-read-line-ascii))

(define* (enter-y-or-n (prompt "y/n? "))
  (let loop ()
    (println prompt)
    (let ((s (tty-read-line-ascii)))
      (cond ((or (string=? s "n") (string=? s "N"))
             #f)
            ((or (string=? s "y") (string=? s "Y"))
             #t)
            (else
             (println "You must enter y or n. Please try again.")
             (loop))))))

(define* (enter-line-ascii (prompt #f))
  (if prompt (print prompt))
  (tty-read-line-ascii))

(define* (enter-password (prompt "Password: "))
  (print prompt)
  (tty-read-line-ascii-no-echo))

(define (strong-password? password)
  (let ((classes
         (map (lambda (c)
                (cond ((char-alphabetic? c) 'alphabetic)
                      ((char-numeric? c)    'numeric)
                      (else                 'special)))
              (string->list password))))
    (and (>= (string-length password) 8)
         (memq 'alphabetic classes)
         (memq 'numeric classes)
         (memq 'special classes))))

(define* (enter-new-password (prompt "Password: "))
  (let loop ()
    (let ((password (enter-password prompt)))
      (cond ((string=? password "")
             #f)
            ((not (strong-password? password))
             (print
              "Password is weak.\n"
              "Please enter a password with at least 8 characters, and which includes\n"
              "a combination of letters, numerals and special characters.\n")
             (loop))
            (else
             (println "Please retype for confirmation.")
             (if (not (string=? password (enter-password prompt)))
                 (begin
                   (println "Password is not the same.  Please try again.")
                   (loop))
                 password))))))

;;;=============================================================================
