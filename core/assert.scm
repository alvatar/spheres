;;!!! Assertion procedures
;; .author Álvaro Castro Castilla

;; Typed assertions:
;; 
;; (define-type condition extender: define-type-of-condition)
;; (define-type-of-condition compound (contitions read-only:))
;; (define-type-of-condition serious extender: define-type-of-serious)
;; (define-type-of-serious violation extender: define-type-of-violation)
;; (define-type-of-violation assertion constructor: make-assertion-violation)
;; (define-type-of-condition who constructor: make-who-condition (who read-only:))
;; (define-type-of-condition message constructor: make-message-condition (message read-only:))
;; (define-type-of-condition irritants constructor: make-irritants-condition (irritants read-only:))
;; (define condition
;;   (lambda conditions
;;     (make-compound (apply append (map simple-conditions conditions))))) 
;; (define simple-conditions
;;   (lambda (condition)
;;     (cond
;;      ((compound? condition) (compound-conditions condition))
;;      (else (list condition)))))
;; (define (assertion-violation who message . irritants)
;;   (raise 
;;     (if who
;;     (condition (make-who-condition who) 
;;         (make-message-condition message) 
;;         (make-irritants-condition irritants)
;;         (make-assertion-violation))
;;     (condition (make-message-condition message) 
;;         (make-irritants-condition irritants)
;;         (make-assertion-violation)))))

;; List-based assertion
(define (assertion-violation who message . irritants)
  (raise
   (if who
       `(assertion-violation in: ,who message: ,message irritants: ,@irritants)
       `(assertion-violation message: ,message irritants: ,@irritants))))

(define (assertion-errors-display . args)
  (for-each (lambda (x)
              (if (procedure? x)
                  (x)
                  (display x (current-error-port))))
            args))
