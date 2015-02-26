(test*

 (let ((p (make-parser '( (s (a) (s s)) )
                       (lambda (l) (map (lambda (x) (list x x)) l)))))

   (expect*
    (equal? (parse->trees (p '(a)) 's 0 1)
            '(((s 1) ((a 0) a)))))

   (expect*
    (equal? (parse->trees (p '(a a)) 's 0 2)
            '(((s 2) ((s 1) ((a 0) a))
                     ((s 1) ((a 0) a))))))

   (expect*
    (equal? (parse->trees (p '(a a a)) 's 0 3)
            '(((s 2) ((s 2) ((s 1) ((a 0) a))
                            ((s 1) ((a 0) a)))
                     ((s 1) ((a 0) a)))
              ((s 2) ((s 1) ((a 0) a))
                     ((s 2) ((s 1) ((a 0) a))
                            ((s 1) ((a 0) a)))))))

   (expect*
    (equal? 132
            (length (parse->trees (p '(a a a a a a a)) 's 0 7))))


   (expect*
    (equal? 1430
            (parse->nb-trees (p '(a a a a a a a a a)) 's 0 9))))

)
