;;!!! Prototype-based object system for SchemeSpheres
;; Based on Tiny Talk, by Kenneth A Dicke
;; .author Kenneth A Dicke, 2008
;; .author Álvaro Castro-Castilla, 2010-2014, some macros needed porting to syntax-rules,
;; plus some variations of the system
;;
;; Note by K. A. Dicke: both the find-method procs (the 2nd is in 
;; make-dispatch-table) are where method caches would improve 
;; performance.  This exactly corresponds to the local and
;; global caches in some Smalltalk implementations.


;;! Object prototype
(define-type prototype-object
  constructor: make-prototype-object
  predicate: prototype-object?
  (dispatcher object-dispatcher read-only:))

;;! Send message to object
(define (-> sym obj)
  (define (error-not-applicable sym obj)
    ;; create and return a "method"
    (lambda args (error sym "not applicable to object" args (object->string obj))))
  (cond ((prototype:find-method sym obj))
        (((prototype:custom-method-finder) sym obj))
        (else (error-not-applicable sym obj))))

;;! Find the right message handler
(define prototype:custom-method-finder
  (let ((sim+obj->method (lambda (sym obj) #f)))
    (lambda rest
      (if (null? rest)
          sim+obj->method
          (let ((proc (car rest)))
            (unless (procedure? proc)
                    (error 'custom-method-finder
                           "requires a procedure (lambda (sym obj) ...)"
                           proc))
            (set! sim+obj->method proc)
            proc)))))

;; Search methods, look for delegate(s) if required
;; .returns method or #f
;; TODO: This is a place for further optimization
(define (prototype:find-method selector obj)
  (when (prototype-object? obj)                   ; return #f if not an object
        (let ((dispatcher (object-dispatcher obj)))
          (cond ((dispatcher selector)) ; find method
                ((dispatcher 'delegate) ; find delgate
                 => (lambda (m)
                      (let ((delegate (m obj)))
                        (if (list? delegate)
                            ;; multiple inheritance (First Found Rule)
                            (let loop ((delegates delegate))
                              (cond ((null? delegates) #f)
                                    ((prototype:find-method selector (car delegates)))
                                    (else (loop (cdr delegates)))))
                            ;; single inheritance
                            (prototype:find-method selector delegate)))))
                (else #f)))))

;;! Make a setter/getter procedure
;; (lambda) (self) or (self val)
(define prototype:make-setter-getter
  (case-lambda
   ((val)
    (case-lambda
     ((obj) val)
     ((obj new-val) (set! val new-val) new-val)))
   ((val filter-proc)
    (case-lambda
     ((obj) val)
     ((obj new-val) (set! val (filter-proc new-val)) new-val)))))

;;! Make a procedure which maps a symbol (selector) to a method
;; (lambda (self args ...)) or #f
(define prototype:make-dispatch-table
  (let ((remove (lambda (p l)
                  (let recur ((l l))
                    (cond ((null? l) '())
                          ((p (car l)) (recur (cdr l)))
                          (else (cons (car l) (recur (cdr l)))))))))
    (lambda (name-method-alist)
      (letrec
          ((smart-assq! (let ((short-count 2))
                          (lambda (sym)
                            (let count-loop ((count 0) (alist name-method-alist))
                              (cond
                               ((null? alist) #f) ;; failed
                               ((eq? sym (caar alist)) (car alist)) ; success
                               ((< count short-count)
                                (count-loop (+ count 1) (cdr alist)))
                               (else ;; ASSERT: (>= count short-count)
                                (let move-loop ((last alist) (current (cdr alist)))
                                  (cond
                                   ((null? current) #f)      ;; failed
                                   ((eq? sym (caar current)) ;; success
                                    ;; splice out found
                                    (set-cdr! last (cdr current))
                                    ;; move found to front
                                    (set-cdr! current name-method-alist)
                                    (set! name-method-alist current)
                                    ;; return found (name . method) pair
                                    (car current))
                                   (else (move-loop (cdr last) (cdr current)))))))))))
           (find-method
            (lambda (sym)
              (cond ((eq? sym 'lookup) (lambda (obj) prototype:find-method))
                    ((smart-assq! sym) => cdr)
                    ;; Default built-in behaviors
                    ((eq? sym 'methods) (lambda (obj) name-method-alist))
                    ((eq? sym 'add-method!) add-method!)
                    ((eq? sym 'remove-method!) remove-method!)
                    (else #f))))
           (add-method!
            (lambda (obj name method)
              (cond
               ((assq name name-method-alist) => (lambda (p) (set-cdr! p method)))
               (else
                (set! name-method-alist
                      (cons (cons name method) name-method-alist))))
              name))
           (remove-method!
            (lambda (obj to-remove)
              (assert (symbol? to-remove)
                      "bad method selector (not a symbol)")
              (assert (not (memq to-remove '(field-names
                                             prototype:shallow-clone
                                             prototype:deep-clone)))
                      "this method is required: can't be removed")
              (set! name-method-alist
                    (remove (lambda (pair)
                              (eq? to-remove (car pair)))
                            name-method-alist))
              (let* ((field-names-bucket
                      (assq 'field-names name-method-alist))
                     (field-names-method (cdr field-names-bucket))
                     (field-names-list (field-names-method obj)))
                (when (memq to-remove field-names-list)
                      (let ((new-field-names
                             (remove (lambda (m) (eq? m to-remove)) field-names-list)))
                        (set-cdr! field-names-bucket
                                  (lambda (self) new-field-names))))))))
        find-method))))


;;! Clone an object without recursively cloning its delegates
(define (prototype:shallow-clone obj)
  (define (clone-accessors al)
    (map (lambda (p) (cons (car p)
                      (prototype:make-setter-getter ((cdr p) obj))))
         al))
  (define (partition pred? list)
    (let loop ((yes '())
               (no '())
               (list list))
      (cond
       ((null? list) (values (reverse yes) (reverse no)))
       ((pred? (car list))
        (loop (cons (car list) yes) no (cdr list)))
       (else
        (loop yes (cons (car list) no) (cdr list))))))
  (assert (prototype-object? obj) "can't clone: this isn't an object")
  (let ((field-names ($ field-names obj)))
    (receive (fields methods)
             (partition (lambda (pair) (memq (car pair) field-names))
                        ($ methods obj))
             (make-prototype-object
              (prototype:make-dispatch-table
               (append
                (clone-accessors fields)
                ;; make a new a-list because add-method! uses set-cdr!
                (map (lambda (pair) (cons (car pair) (cdr pair))) methods)))))))
(define prototype:shallow-clone-method (lambda (self) (prototype:shallow-clone self)))

;;! Recursively clones delegates
(define (prototype:deep-clone obj)
  (let* ((cloned (prototype:shallow-clone obj))
         (del (assq 'delegate ($ methods cloned))))
    (when del
          (let* ((delegate ((cdr del) cloned))
                 (cloned-delegate
                  (if (list? delegate)
                      (map prototype:deep-clone delegate)
                      (prototype:deep-clone delegate))))
            (set-cdr! del (lambda (obj) cloned-delegate))))
    cloned))
(define prototype:deep-clone-method (lambda (self) (prototype:deep-clone self)))

;;! Return a string describing any object
(define (string<< thing)
  (let ((prototype-obj->string
         (lambda (obj)
           (let ((outp (open-output-string))
                 (field-names ($ field-names  obj))
                 (lookup (object-dispatcher obj)))
             (display "#[instance" outp)
             (for-each (lambda (name)
                         (display " " outp)
                         (display name outp)
                         (display ": " outp)
                         (display (->string ((lookup name) obj))
                                  outp))
                       field-names)
             (display "]" outp)
             (get-output-string outp)))))
    (cond
     ((prototype:find-method '->string thing) => (lambda (m) (m thing)))
     ((prototype-object? thing) (prototype-obj->string thing))
     (else (object->string thing)))))
