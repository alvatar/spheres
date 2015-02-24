(%load-library '(spheres/util test))

(%load-library '(spheres/fileformat json))
(%load-library '(spheres/algorithm list))


(test-begin "JSON reader/writer")

(let ((i 0))
  (for-each
   (lambda (pair)
     (with-input-from-string (car pair)
       (lambda ()
         (set! i (+ 1 i))
         (let ((parsed (json-read (current-input-port))))
           (test-equal (string-append "Number parsing: "
                                      (object->string pair) " --> "
                                      (object->string parsed))
                       parsed
                       (cadr pair))))))
   '(("42" 42)
     ("-42" -42)
     ;;("+42" 42)
     ("42." 42.)
     ("0" 0)
     ;;("+0" 0)
     ("-0" 0)
     ;;(".5" 0.5)
     ("0.5" 0.5)
     ;;("+.5" 0.5)
     ;;("-.5" -0.5)
     ("2e0" 2.)
     ("2e1" 20.)
     ("2E0" 2.)
     ("2E1" 20.)
     ("2e+0" 2.)
     ("2e-0" 2.)
     ("2.e0" 2.)
     ("2.e+0" 2.)
     ("2.e-0" 2.))))

(let ((i 0))
  (for-each
   (lambda (pair)
     (with-input-from-string (car pair)
       (lambda ()
         (set! i (+ 1 i))
         (let ((parsed (json-read (current-input-port))))
           (test-equal (string-append "String parsing: "
                                      (object->string pair) " --> "
                                      (object->string parsed))
                       parsed
                       (cadr pair))))))
   '(("\"text\"" "text")
     ("\"foo\\\"bar\"" "foo\"bar"))))

(let ((i 0))
  (for-each
   (lambda (pair)
     (with-input-from-string (car pair)
       (lambda ()
         (set! i (+ 1 i))
         (let ((parsed (json-read (current-input-port))))
           (test-equal (string-append "List parsing: "
                                      (object->string pair) " --> "
                                      (object->string parsed))
                       parsed
                       (cadr pair))))))
   '(("[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]"
      #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#()))))))))))))))))))))))))))
     ("[]" #())
     ("[1]" #(1))
     ("[1, 2]" #(1 2))
     ("[1, [2]]" #(1 #(2)))
     ;;("[1,]" (1))
     ;;("[1,2,]" (1 2))
     )))

(test-end)
