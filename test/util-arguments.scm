;;; Most of this code was written by Per Eckerdal for the Blackhole module system
;;; Copyright (c) 2013-2014, Alvaro Castro-Castilla. All rights reserved.
;;; Argument processing for command-line programs

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))


(define (die/error . args)
  (let ((err (current-error-port)))
    (display "Error: " err)
    (for-each
     (lambda (arg)
       (display arg err)
       (display " " err))
     args)
    (display "\n") err)
  (exit 1))

;;!! Parse arguments
;; .parameter args List of arguments to process
;; .parameter options List of options following this structure:
;; '((#\g 0 "global")
;;   (#\f 1 "file"))
;; 1 means that takes an argument, 0 it doesn't
;; Example:
;; (define (main . args)
;;   (let ((commands
;;          `(("install" ,@install-cmd)
;;            ("uninstall" ,@uninstall-cmd)
;;            ("help" ,@help-cmd)
;;            ("update" ,@update-cmd)
;;            ("search" ,@search-cmd)
;;            ("set" ,@set-cmd)
;;            ("unknown-command" ,@unknown-cmd))))
;;     (parse-arguments
;;      args
;;      (lambda (actual-args-sans-opts opts)
;;        (let* ((args-sans-opts (if (null? actual-args-sans-opts)
;;                                   '("help")
;;                                   actual-args-sans-opts))
;;               (cmd-pair (assoc (car args-sans-opts) commands))
;;               (cmd (if cmd-pair
;;                        (cdr cmd-pair)
;;                        (cdr (assoc "unknown-command" commands)))))
;;          (cmd (car args-sans-opts)
;;               opts
;;               (cdr args-sans-opts))))
;;      *options*)))
;; (apply main (cdr (command-line)))
(define (parse-arguments args kont options)
  (define (string-contains haystack chr)
    (call/cc
     (lambda (ret)
       (let ((strlen (string-length haystack)))
         (let loop ((i 0))
           (if (>= i strlen)
               (ret #f)
               (let ((c (string-ref haystack i)))
                 (if (eq? c chr)
                     (ret i)
                     (loop (+ i 1))))))))))
  (define (opt? str)
    (and (> (string-length str) 1)
         (char=? #\- (string-ref str 0))))
  (define (long-opt? str)
    (and (> (string-length str) 2)
         (char=? #\- (string-ref str 0))
         (char=? #\- (string-ref str 1))))
  (define (short-opt? str)
    (and (opt? str)
         (not (long-opt? str))))
  (let loop ((args args)
             (args-sans-opts '())
             (opts '()))
    (define (consume-next-argument!)
      (if (or (null? (cdr args))
              (equal? "--" (cadr args)))
          (die/error "Expected an argument to" (car args)))
      (let ((val (cadr args)))
        (set-cdr! args (cddr args))
        val))
    (cond
     ((null? args)
      (kont (reverse args-sans-opts)
            (reverse opts)))
     ((equal? "--" (car args))
      (kont (append (reverse args-sans-opts)
                    (cdr args))
            (reverse opts)))
     ((long-opt? (car args))
      (let* ((=-pos (string-contains (car args) #\=))
             (opt-name
              (substring (car args)
                         2
                         (or =-pos
                             (string-length (car args)))))
             (opt-val
              (and =-pos
                   (substring (car args)
                              (+ 1 =-pos)
                              (string-length (car args))))))
        (loop (cdr args)
              args-sans-opts
              (cons (list opt-name
                          (string-append "--" opt-name)
                          opt-val)
                    opts))))
     ((short-opt? (car args))
      (let* ((str (car args))
             (len (string-length str)))
        (let inner-loop ((idx 1) (opts opts))
          (cond
           ((= len idx)
            (loop (cdr args)
                  args-sans-opts
                  opts))

           (else
            (let* ((opt-chr (string-ref str idx))
                   (opt (assq opt-chr options)))
              (if (not opt)
                  (die/error "Unrecognized option" (car args)))

              (let ((val
                     (cond
                      ((zero? (cadr opt))
                       #f)

                      ((not (= 2 len))
                       (die/error "Option that takes an argument must not be grouped"
                                  (car args)))

                      (else
                       (consume-next-argument!)))))
                (inner-loop (+ 1 idx)
                            (cons (list (caddr opt)
                                        (string #\- opt-chr)
                                        val)
                                  opts)))))))))
     (else
      (loop (cdr args)
            (cons (car args) args-sans-opts)
            opts)))))

(define (handle-opts! opts handlers)
  (for-each
   (lambda (opt)
     (let ((handler (assoc (car opt) handlers)))
       (if handler
           ((cdr handler) (caddr opt))
           (die/error "Option is not valid in this context:"
                      (cadr opt)))))
   opts))

(define (ensure-no-args! args)
  (if (not (null? args))
      (apply
       die/error
       (cons "Did not expect arguments:" args))))

(define (ensure-args! args)
  (if (null? args)
      (apply
       die/error
       (list "At least one argument is required."))))

(define (ensure-one-arg! args)
  (if (not (and (list? args)
                (= 1 (length args))))
      (apply
       die/error
       (cons "Expected exactly one argument:" args))))

