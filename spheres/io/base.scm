;;!!! Basic I/O extensions
;; .author Alvaro Castro-Castilla, 2013-2015

;;! Write an u8vector to a port
(define* (write-u8vector vec (port #f))
  (write-subu8vector
   vec
   0
   (u8vector-length vec)
   (or port (current-output-port))))

;;! Echo all u8vector data to a target u8vector port, return the u8vector if no
;; target given
(define* (echo-u8vector-port source-port (target-port #f))
  (if (not target-port)
      (call-with-output-u8vector
       '()
       (lambda (target-port)
         (echo-u8vector-port source-port target-port)))
      (let* ((tmp-bufsize (* 50 1024))
             (tmp-buffer (make-u8vector tmp-bufsize)))
        (let loop ()
          (let ((n (read-subu8vector tmp-buffer 0 tmp-bufsize source-port)))
            (if (> n 0)
                (begin
                  (write-subu8vector tmp-buffer 0 n target-port)
                  (loop))))))))
