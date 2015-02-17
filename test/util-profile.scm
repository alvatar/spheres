(import ../src/profile)

(define (ring n)
  (if (and (integer? n)
		(exact? n)
		(>= n 2)
		(<= n 1000000))
      (let ((m (quotient 1000000 n)))
	(joes-challenge n m))
      (error "invalid arg to ring")))

(define (joes-challenge n m)

    (define (iota n)
      (iota-aux n '()))

    (define (iota-aux n lst)
      (if (= n 0)
          lst
          (iota-aux (- n 1) (cons n lst))))

  (define (create-processes)
    (let* ((one-to-n
            (iota n))
           (channels
            (list->vector
             (map (lambda (i) (open-vector))
                  one-to-n)))
           (processes
            (map (lambda (i)
                   (let ((input (vector-ref channels (modulo (- i 1) n)))
                         (output (vector-ref channels (modulo i n))))
                     (make-thread
                      (lambda ()
                        (let loop ((j m))
                          (if (> j 0)
                              (let ((message (read input)))
                                (write message output)
                                (force-output output)
                                (loop (- j 1)))))))))
                 one-to-n)))
      (write 'go (vector-ref channels 1))
      (force-output (vector-ref channels 1))
      processes))

  (let* ((point1
          (cpu-time))
         (processes
          (create-processes))
         (point2
          (cpu-time)))

    (for-each thread-start! processes)
    (thread-join! (car processes))

    (let ((point3
           (cpu-time)))

      (display n)
      (display " ")
      (display (/ (- point2 point1) n))
      (display " ")
      (display (/ (- point3 point2) (* n m)))
      (newline))))

(profile-start!)
(ring 10000)
(profile-stop!)

(write-profile-report "ring")
