;;!!! Logging
;;
;; .author Alvaro Castro-Castilla, 2014-2015

;;! Generic log message
(define* (log type (color: #f) . messages)
  (define (symbol->upcase-string sym)
    (list->string
     (map char-upcase (string->list (symbol->string sym)))))
  (let ((color-string
         (case color
           ((black) "\033[00;30m")
           ((dark-gray) "\033[01;30m")
           ((blue) "\033[00;34m")
           ((light-blue) "\033[01;34m")
           ((green) "\033[00;32m")
           ((light-green) "\033[01;32m")
           ((cyan) "\033[00;36m")
           ((light-cyan) "\033[01;36m")
           ((red) "\033[00;31m")
           ((light-red) "\033[01;31m")
           ((purple) "\033[00;35m")
           ((light-purple) "\033[01;35m")
           ((brown) "\033[00;33m")
           ((yellow) "\033[01;33m")
           ((light-gray) "\033[00;37m")
           ((white) "\033[01;37m")
           (else ""))))
    (define (print-message)
      (display (string-append
                "*** "
                (symbol->upcase-string type)
                " -- "
                color-string
                (append-strings messages)
                "\033[00m\n")))
    (case type
      ((INFO info)
       (print-message))
      ((WARNING warning ERROR error)
       (with-output-to-port (current-error-port) print-message))
      (else
       (error "Unknown log message type")))))

;;! Info message
(define* (info (color: #f) . message)
  (apply log 'info color: color message))

;;! Warn message
(define* (warn (color: 'brown) . message)
  (apply log 'warning color: color message))

;;! Error message
(define* (err (color: 'red) . message)
  (apply log 'error color: color message)
  (error "error, aborting"))
