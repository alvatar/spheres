(load (spheres/util test))

;;-------------------------------------------------------------------------------
;; Multiple-instance tests

;; Node 2
(define (spawn-gambit code #!key
                      (flags-string #f)
                      (verbose #f))
  ;; This is a hack transforming all ' into ` since they work in all cases and makes
  ;; life easier when passed as a string with ' delimiters to bash
  (let ((quotes->semiquotes
         (lambda (string)
           (let ((length (string-length string))
                 (transformed (string-copy string)))
             (let recur ((n 0))
               (if (< n length)
                   (begin
                     (if (eq? #\' (string-ref string n))
                         (string-set! transformed n #\`))
                     (recur (+ n 1)))
                   transformed))))))
    (let ((code-string (quotes->semiquotes
                        (object->string (cons 'begin code)))))
      (and verbose
           (begin (println "gambit-eval-here input code: ") (pp code)
                  (println "gambit-eval-here string: ") (print code-string)))
      (open-process (list path: "gsi"
                          arguments: (if flags-string
                                         (list flags-string "-e" code-string)
                                         (list "-e" code-string))
                          stdout-redirection: #f)))))

(spawn-gambit '((load (spheres/concurrency termite))
                (define node1 (make-node "localhost" 3001))
                (define node2 (make-node "localhost" 3002))
                (node-init node2)
                (thread-sleep! 6)))

;; Wait to make sure Gambit node2 is up and running
(thread-sleep! 1)

;; Node 1
(load (spheres/concurrency termite))

(define node1 (make-node "localhost" 3001))
(define node2 (make-node "localhost" 3002))
(node-init node1)


;;(debug (current-node))
(on node2
    (lambda ()
      (write 'success)
      (newline)
      (println "Termite task migration test successful!")))

(define pid
  (spawn 
   (lambda () 
     (migrate-task node2)
     (termite-info 'migration-ok!))))



;;-------------------------------------------------------------------------------
;; Single-instance tests


(test-begin "termite" 1)

(test-equal "single instance messaging"
            (let ()
              (define (worker)
                (let* ((msg (?)) (pid (car msg)) (fun (cadr msg)) (arg (caddr msg)))
                  (! pid (fun arg))))
              (define (pmap fun lst)
                ;; spawn workers
                (for-each 
                 (lambda (x) (let ((p (spawn worker))) (! p (list (self) fun x))))
                 lst)
                ;; collect results
                (let loop ((i (length lst)) (result '()))
                  (if (= 0 i) (reverse result) (loop (- i 1) (cons (?) result)))))
              (define (f x) (thread-sleep! x) x)
              (pmap f (list 1 2 3 2 1)))
              '(1 1 2 2 3))

(test-end)
