(include "autoffi-lexer.scm")

;; Token generators depend on these definitions
(define pp-mode #f)
(define (lexer-error c)
  (display "*** ERROR *** invalid token: ")
  (write c)
  (newline)
  (exit 1))

(define (tokenize #!optional
               (input-port (current-input-port))
               (output-port (current-output-port)))
  (lexer-init 'port input-port)
  (let loop ()
    (let ((tok (lexer)))
      (write tok output-port)
      (newline output-port)
      (if (not (eq? tok 'stop))
          (loop)))))
