;;!! SRFI-38 External Representation for Data With Shared Structure


;;! write-with-shared-structure
(define* (write-with-shared-structure
          obj
          (port (current-output-port))
          (serialize? #f))
  (with-output-to-port
      port
    (lambda ()
      (let* ((p (current-output-port))
             (prev-readtable (output-port-readtable p)))
        (output-port-readtable-set! p
                                    (readtable-sharing-allowed?-set
                                     (output-port-readtable p)
                                     (if serialize? 'serialize #t)))
        (write obj p)
        (output-port-readtable-set! p prev-readtable)
        #!void))))

;;! read-with-shared-structure
(define* (read-with-shared-structure
          obj
          (port (current-input-port))
          (serialize? #f))
  (with-input-from-port
      port
    (lambda ()
      (let* ((p (current-input-port))
             (prev-readtable (input-port-readtable p)))
        (input-port-readtable-set! p
                                   (readtable-sharing-allowed?-set
                                    (input-port-readtable p)
                                    (if serialize? 'serialize #t)))
        (let ((result (read p)))
          (input-port-readtable-set! p prev-readtable)
          result)))))
