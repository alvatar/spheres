
(include "../../sxml-tools/tests/xtest-maker.sch")


; Defines a test suite 
(define (xtest-suite . tests)
  (lambda active
    (xtest-filter
      (lambda(tst)
	(or (null? active)
	    (member (car tst) active)))
      tests))) 

; Writes out names for all test-cases in test-suite 
(define (xtest-names t-suite)
(for-each pp (map car (t-suite))))
 
; Writes out code for all test-cases in test-suite 
(define(xtest-write t-suite . fns)
(for-each 
       (lambda(x) (force (cdr x)))
       (apply t-suite fns)))
