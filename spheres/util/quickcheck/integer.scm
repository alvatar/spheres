(define (in-range #!key (step 1) (start 0) (stop 100))
  (generate! 
   (lambda (yield)
     (let range ((j start))
       (if (< j stop)
           (begin
             (yield j)
             (range (+ j step))))))))

(define (an-integer #!key (max 1000) (cases 100))
  (generate!
   (lambda (yield)
     (let integer ((c 0))
       (if (< c cases)
           (begin
             (yield (random-integer max))
             (integer (+ c 1))))))))

