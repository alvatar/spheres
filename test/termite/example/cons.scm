(define-type kons
  id: 713cb0a4-16ea-4b18-a18e-7a9e33e7b92b
  unprintable:
  pid)

(define (kons e1 e2)
  (make-kons
   (spawn
     (lambda ()
       (kons-loop e1 e2)))))

(define (kons-loop e1 e2)
  (recv
    ((from tag 'pair) 
     (! from (list tag (cons e1 e2)))
     (kons-loop e1 e2))
    
    ((from tag 'kar)
     (! from (list tag e1))
     (kons-loop e1 e2))
   
    ((from tag 'kdr)
     (! from (list tag e2))
     (kons-loop e1 e2))
   
    (('set-kar! x) 
     (kons-loop x e2))
   
    (('set-kdr! x)
     (kons-loop e1 x))

    (x 
     (print 'unknown-message)
     (raise (list unknown-message: x)))))

(define (kons->cons k) (!? (kons-pid k) 'pair))
(define (kar k) (!? (kons-pid k) 'kar))
(define (kdr k) (!? (kons-pid k) 'kdr))
(define (set-kar! k val) (! (kons-pid k) (list 'set-kar! val)))
(define (set-kdr! k val) (! (kons-pid k) (list 'set-kdr! val)))

(define x (kons 1 2))

(pp x)
(pp (kar x))
(pp (kdr x))
(pp (kons->cons x))
(set-kar! x 123)
(pp (kons->cons x))

(? 1 'ok)
(pp 'done)
