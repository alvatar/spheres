;;! u8vector to file
(define (u8vector->file u8vector filename)
  (call-with-output-file
      `(path: ,filename)
    (lambda (port)
      (write-subu8vector u8vector 0 (u8vector-length u8vector) port))))

;;! File to u8vector
(define (file->u8vector filename)
  (call-with-output-u8vector
   '()
   (lambda (write-port)
     (let ((buf (make-u8vector 10240)))
       (call-with-input-file `(path: ,filename)
         (lambda (port)
           (let loop ()
             (let ((r (read-subu8vector buf 0 10240 port)))
               (write-subu8vector buf 0 r write-port)
               (if (< 0 r) (loop))))))))))
