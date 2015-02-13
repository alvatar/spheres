;;!!! .title Gambit srfi-99 records procedural and inspection implementation
;; with r6rs optional extensions).
;; .author Arthur T Smyles
;; .author Álvaro Castro-Castilla
;;
;; Copyright (c) 2008, Arthur T Smyles
;; Copyright (c) 2014, Álvaro Castro-Castilla
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the <organization> nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY Arthur T Smyles ''AS IS'' AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL Arthur T Smyles BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;! Make a new record type
;; .parameter name Is a symbol, which matters only to the rtd-name procedure of the inspection layer.
;; .parameter fieldspecs Is a vector of field specifiers, where each field specifier is one of
;;   * a symbol naming the (mutable) field;
;;   * a list of the form (mutable name), where name is a symbol naming the mutable field;
;;   * a list of the form (immutable name), where name is a symbol naming the immutable field.
;; .parameter rest The optional parent is an rtd or #f. It is an error for any of the symbols in fieldspecs to name more than one of the fields specified by fieldspecs, but the field names in fieldspecs may shadow field names in the parent record-type.
(define (make-rtd name fieldspecs . rest)
  (define parent (and (pair? rest) (car rest)))
  (define sealed? (and (pair? rest) (memq 'sealed rest) #t))
  (define opaque? (and (pair? rest) (memq 'opaque rest) #t))
  (define uid (let [(uid (and (pair? rest) (memq 'uid rest)))] (and uid (car uid))))
  (define (parse-fields fields)
    (define (make-field-flags mutable? printable? equality? init?)
      (+ (if printable? 0 1)
         (if mutable? 0 2)
         (if equality? 0 4)
         (if init? 0 8)))
    (let* ((field-count (vector-length fields))
           (result (make-vector (* field-count 3))))
      (let process-fields ((i 0))
        (if (not (< i field-count)) result
            ;; gambit has other attributes for fields such as if they are printable and it's contribution to equality
            (let* ((field (vector-ref fields i))
                   (flags (make-field-flags (or (symbol? field) (eq? 'mutable (car field))) #t #t #f))
                   (field-name (if (symbol? field) field (cadr field)))
                   (j (* i 3)))
              (vector-set! result j field-name)
              (vector-set! result (+ j 1) flags)
              (vector-set! result (+ j 2) #f) ; used for setting initial value
              (process-fields (+ i 1)))))))
  (let* ((flags (##fixnum.+ (if opaque? 1 0) (if sealed? 0 2)))
         (uid (if uid uid (make-uninterned-symbol (symbol->string name)))))
    (##structure ##type-type uid name flags parent (parse-fields fieldspecs))))

;;! rtd?
(define rtd? ##type?)

;;! Constructor generator
;; .parameter rtd A record type to build the generator for
;; .parameter rest Fieldspecs is an optional vector of symbols. If no fieldspecs argument is supplied,
;; then rtd-constructor returns a procedure that expects one argument for each field of the record-type
;; described by rtd and returns an instance of that record-type with its fields initialized to the
;; corresponding arguments. Arguments that correspond to the fields of the record-type's parent (if any) come first.
(define (rtd-constructor rtd . rest) 
  ;; Computes permutation and allocates permutation buffer
  ;; when the constructor is created, not when the constructor
  ;; is called.  More error checking is recommended.
  (define (make-constructor fieldspecs allnames maker)
    (let* ((k (length fieldspecs))
           (n (length allnames))
           (buffer (make-vector n 'some-unspecified-value))
           (reverse-all-names (reverse allnames)))
      (define (position fieldname)
        (let ((names (memq fieldname reverse-all-names)))
          (assert names)
          (- (length names) 1)))
      (let ((indexes (map position fieldspecs)))
        ;; The following can be made quite efficient by
        ;; hand-coding it in some lower-level language,
        ;; e.g. Larceny's mal.  Even case-lambda would
        ;; be good enough in most systems.
        (lambda args
          (assert (= (length args) k))
          (for-each (lambda (arg posn)
                      (vector-set! buffer posn arg))
                    args indexes)
          (apply maker (vector->list buffer))))))
  (if (null? rest) (lambda fields (apply ##structure rtd fields))
      (begin
        (assert (null? (cdr rest)))
        (make-constructor
         (vector->list (car rest))
         (vector->list (rtd-all-field-names rtd))
         (rtd-constructor rtd)))))

;;! Predicate generator
;; .parameter rtd A record type to build the predicate for
(define (rtd-predicate rtd)
  (let ((uid (rtd-uid rtd)))
    (if (rtd-sealed? rtd)
        (lambda (obj) (##structure-direct-instance-of? obj uid))
        (lambda (obj) (##structure-instance-of? obj uid)))))

;;! Extension function: creates a generic deconstructor that returns all the fields as values
;; .parameter rtd A record type to build the deconstructor for
;; .paramter predicate The predicate of the rtd, if it's already built (otherwise generate)
(define (rtd-deconstructor rtd . predicate)
  (if (rtd? rtd)
      (let ((predicate? (if (null? predicate) (rtd-predicate rtd) (car predicate)))
            (rtd-indexer (eval `(lambda (field)
                                  (case field
                                    ,@(reverse (rtd-map-all rtd (lambda (i name . rest) `((,name) ,(+ i 1)))))))))
            (accessor (if (rtd-sealed? rtd) ##direct-structure-ref ##structure-ref))
            (decons (lambda (obj) (let ((result (##subvector obj 1 (##vector-length obj)))) (##subtype-set! result 5 #|boxvalues type|#) result))))
        (lambda (obj . fields)
          (cond
           ((not (predicate? obj)) (assertion-violation 'rtd-deconstructor (string-append "First argument must be of type " (symbol->string (rtd-name rtd))) obj))
           ((null? fields) (decons obj)) ;(apply values (rtd-map-all rtd (lambda (i . rest) (accessor obj (+ i 1) rtd #f)))))
           ((null? (cdr fields)) (values (accessor obj (rtd-indexer (car fields)) rtd #f)))
           (else (apply values (map (lambda (field) (accessor obj (rtd-indexer field) rtd #f)) fields))))))
      (assertion-violation 'rtd-deconstructor "first argument must be a record type descriptor" rtd)))

;;! Accessor generator
;; .parameter rtd A record type to build the accessor of a field for
;; .parameter field A field of the record
(define (rtd-accessor rtd field)
  (if (rtd? rtd)
      (call/cc
       (lambda (return)
         (rtd-for-each rtd 
                       (lambda (i name . rest)
                         (if (eq? field name)
                             (let ((index (+ i 1)))
                               (if (rtd-sealed? rtd)
                                   (return (lambda (obj) (##direct-structure-ref obj index rtd #f)))
                                   (return (lambda (obj) (##structure-ref obj index rtd #f))))))))
         (if (rtd-parent rtd) (return (rtd-accessor (rtd-parent rtd) field))
             (assertion-violation 'rtd-accessor "invalid field name" field))))
      (assertion-violation 'rtd-accessor "first argument must be a record type descriptor" rtd)))

;;! Mutator generator
;; .parameter rtd A record type to build the mutator of a field for
;; .parameter field A field of the record
(define (rtd-mutator rtd field)
  (if (rtd? rtd)
      (call/cc
       (lambda (return)
         (rtd-for-each
          rtd
          (lambda (i name flags init)
            (if (and (eq? name field) (rtd-field-flag-mutable? flags))
                (let ((index (+ i 1)))
                  (if (rtd-sealed? rtd)
                      (return (lambda (obj value) (##direct-structure-set! obj value index rtd #f)))
                      (return (lambda (obj value) (##structure-set! obj value index rtd #f))))))))
         (if (rtd-parent rtd) (return (rtd-mutator (rtd-parent rtd) field)) 
             (assertion-violation 'rtd-mutator "invalid field" field))))
      (assertion-violation 'rtd-accessor "first argument must be an record type descriptor" rtd)))

;;! record? predicate
;; .parameter obj An object (instance of a record)
(define (record? obj)
  (and (##structure? obj) (not (rtd-opaque? (##structure-type obj)))))

;;! Get the type of an object
;; .parameter obj An object (instance of a record)
(define (record-rtd obj)
  (if (record? obj) (##structure-type obj)
      (assertion-violation 'record-rtd "first argument must of type record" obj)))

;;! Get the name of a record type
;; .parameter rtd A record type
(define (rtd-name rtd) (if (rtd? rtd) (##type-name rtd)))

;;! Get the parent of a record type (#f if none)
;; .parameter rtd A record type
(define (rtd-parent rtd) (if (rtd? rtd) (##type-super rtd)))


;; Auxiliary functions
(define (rtd-map rtd proc . rest)
  (define direction (or (null? rest) (car rest)))
  (define fields (##type-fields rtd))
  (define compare (if direction < >=))
  (define increment (if direction  + -))
  (define offset (##type-field-count (rtd-parent rtd)))
  (let process-field ((i (if direction 0 (- (vector-length fields) 3)))
                      (l (if direction (vector-length fields) 0)))
    (if (compare i l)
        (cons
         (proc (+ offset (/ i 3)) (vector-ref fields i) (vector-ref fields (+ i 1)) (vector-ref fields (+ i 2)))
         (process-field (increment i 3) l))
        '())))
(define (rtd-for-each rtd proc . rest)
  (define direction (or (null? rest) (car rest)))
  (define fields (##type-fields rtd))
  (define compare (if direction < >=))
  (define increment (if direction  + -))
  (define offset (##type-field-count (rtd-parent rtd)))
  (let process-field ((i (if direction 0 (- (vector-length fields) 3)))
                      (l (if direction (vector-length fields) 0)))
    (if (compare i l)
        (begin
          (proc (+ offset (/ i 3)) (vector-ref fields i) (vector-ref fields (+ i 1)) (vector-ref fields (+ i 2)))
          (process-field (increment i 3) l))
        (void))))
(define (rtd-map-all rtd proc)
  ;; proc is (lambda (index name flags init))
  (if (rtd-parent rtd)
      (append (rtd-map-all (rtd-parent rtd) proc) (rtd-map rtd proc))
      (rtd-map rtd proc)))
(define (rtd-for-all rtd proc)
  ;; proc is (lambda (index name flags init))
  (if (rtd-parent rtd) (rtd-for-all (rtd-parent rtd) proc)) 
  (rtd-for-each rtd proc))

;;! Get the field names of the rtd
(define (rtd-field-names rtd)
  (list->vector (rtd-map rtd (lambda (i name . rest) name))))

;;! Get *all* the field names of the rtd
(define (rtd-all-field-names rtd)
  (list->vector (rtd-map-all rtd (lambda (i name . rest) name))))

;;! Query whether the given field of a rtd is mutable
(define (rtd-field-mutable? rtd field)
  (if (rtd? rtd)
      (call/cc
       (lambda (k)
         (rtd-for-each
          rtd
          (lambda (i name flags init)
            (if (eq? name field) (k (rtd-field-flag-mutable? flags))))))))) 

;;!! ERR5RS standard extensions

(define (rtd-uid rtd)
  (if (rtd? rtd) (##type-id rtd)))

(define (rtd-sealed? rtd)
  (if (rtd? rtd) (not (fxbit-set? 1 (##type-flags rtd)))))

(define (rtd-opaque? rtd)
  (if (rtd? rtd) (fxbit-set? 0 (##type-flags rtd))))

(define (rtd-field-flag-printable? flags)
  (not (fxbit-set? 0 flags)))

(define (rtd-field-flag-mutable? flags)
  (not (fxbit-set? 1 flags)))

(define (rtd-field-flag-equality? flags)
  (not (fxbit-set? 2 flags)))

(define (rtd-field-flag-init? flags)
  (not (fxbit-set? 3 flags)))
