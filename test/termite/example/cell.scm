(define-type cell
  id: 713cb0a4-16ea-4b18-a18e-7a9e33e7b92b
  unprintable:
  pid)

(define (cell obj)
  (make-cell
   (spawn
     (lambda ()
       (cell-loop obj)))))

(define (cell-loop obj)
  (recv
    ((from tag 'ref)
     (! from (list tag obj))
     (cell-loop obj))
   
    (('set! obj) 
     (cell-loop obj))))

(define (cell-ref c) (!? (cell-pid c) 'ref))
(define (cell-set! c obj) (! (cell-pid c) (list 'set! obj)))

(define c (cell 42))
(print (cell-ref c))
(newline)
(cell-set! c 123)
(print (cell-ref c))
