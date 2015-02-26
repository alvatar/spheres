
(include "util.scm")
(include "tokenize.scm")
(include "parse.scm")
(include "analyze.scm")

(define (autoffi file . port)
  (let ((pipe1 (open-string))
        (pipe2 (open-string))
        (src (open-file file)))
    (tokenize src pipe1)
    (close-output-port pipe1)
    
    (parse pipe1 pipe2)
    (close-input-port pipe1)
    (close-output-port pipe2)
    
    (analyze pipe2 (or (and (pair? port)
                            (car port))
                       (current-output-port)))
    (close-input-port pipe2)
    (close-port src)))
