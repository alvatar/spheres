;;!! SRFI-35 Conditions
;; Copyright (C) Richard Kelsey, Michael Sperber (2002). All Rights Reserved.

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

(define-record-type :condition-type
  (really-make-condition-type name supertype fields all-fields)
  condition-type?
  (name condition-type-name)
  (supertype condition-type-supertype)
  (fields condition-type-fields)
  (all-fields condition-type-all-fields))

;;! make-condition-type
(define (make-condition-type name supertype fields)
  (if (not (symbol? name))
      (error "make-condition-type: name is not a symbol"
             name))
  (if (not (condition-type? supertype))
      (error "make-condition-type: supertype is not a condition type"
             supertype))
  (if (not
       (null? (lset-intersection eq?
                                 (condition-type-all-fields supertype)
                                 fields)))
      (error "duplicate field name" ))
  (really-make-condition-type name
                              supertype
                              fields
                              (append (condition-type-all-fields supertype)
                                      fields)))

;;! condition-subtype?
(define (condition-subtype? subtype supertype)
  (let recur ((subtype subtype))
    (cond ((not subtype) #f)
          ((eq? subtype supertype) #t)
          (else
           (recur (condition-type-supertype subtype))))))

;;! conditioy-type-field-supertype
(define (condition-type-field-supertype condition-type field)
  (let loop ((condition-type condition-type))
    (cond ((not condition-type) #f)
          ((memq field (condition-type-fields condition-type))
           condition-type)
          (else
           (loop (condition-type-supertype condition-type))))))

;; The type-field-alist is of the form
;; ((<type> (<field-name> . <value>) ...) ...)
(define-record-type :condition
  (really-make-condition type-field-alist)
  condition?
  (type-field-alist condition-type-field-alist))

;;! make-condition
(define (make-condition type . field-plist)
  (let ((alist (let label ((plist field-plist))
                 (if (null? plist)
                            '()
                     (cons (cons (car plist)
                                 (cadr plist))
                           (label (cddr plist)))))))
    (if (not (lset= eq?
                    (condition-type-all-fields type)
                    (map car alist)))
        (error "condition fields don't match condition type"))
    (really-make-condition (list (cons type alist)))))

;;! condition-has-type?
(define (condition-has-type? condition type)
  (any (lambda (has-type)
         (condition-subtype? has-type type))
       (condition-types condition)))

;;! condition-ref
(define (condition-ref condition field)
  (type-field-alist-ref (condition-type-field-alist condition)
                        field))

;;! type-field-alist-ref
(define (type-field-alist-ref type-field-alist field)
  (let loop ((type-field-alist type-field-alist))
    (cond ((null? type-field-alist)
           (error "type-field-alist-ref: field not found"
                  type-field-alist field))
          ((assq field (cdr (car type-field-alist)))
           => cdr)
          (else
           (loop (cdr type-field-alist))))))

;;! make-compound-condition
(define (make-compound-condition condition-1 . conditions)
  (really-make-condition
   (apply append (map condition-type-field-alist
                      (cons condition-1 conditions)))))

;;! extract-condition
(define (extract-condition condition type)
  (let ((entry (find (lambda (entry)
                       (condition-subtype? (car entry) type))
                     (condition-type-field-alist condition))))
    (if (not entry)
        (error "extract-condition: invalid condition type"
               condition type))
    (really-make-condition
     (list (cons type
                 (map (lambda (field)
                        (assq field (cdr entry)))
                      (condition-type-all-fields type)))))))

;;! type-field-alist->condition
(define (type-field-alist->condition type-field-alist)
  (really-make-condition
   (map (lambda (entry)
          (cons (car entry)
                (map (lambda (field)
                       (or (assq field (cdr entry))
                           (cons field
                                 (type-field-alist-ref type-field-alist field))))
                     (condition-type-all-fields (car entry)))))
        type-field-alist)))

;;! condition-types
(define (condition-types condition)
  (map car (condition-type-field-alist condition)))

;;! check-condition-type-field-alist
(define (check-condition-type-field-alist the-type-field-alist)
  (let loop ((type-field-alist the-type-field-alist))
    (if (not (null? type-field-alist))
        (let* ((entry (car type-field-alist))
               (type (car entry))
               (field-alist (cdr entry))
               (fields (map car field-alist))
               (all-fields (condition-type-all-fields type)))
          (for-each (lambda (missing-field)
                      (let ((supertype
                             (condition-type-field-supertype type missing-field)))
                        (if (not
                             (any (lambda (entry)
                                    (let ((type (car entry)))
                                      (condition-subtype? type supertype)))
                                  the-type-field-alist))
                            (error "missing field in condition construction"
                                   type
                                   missing-field))))
                    (lset-difference eq? all-fields fields))
          (loop (cdr type-field-alist))))))

;;! &condition
(define &condition
  (really-make-condition-type '&condition
                              #f
                              '()
                              '()))

(define-condition-type &message &condition
  message-condition?
  (message condition-message))

(define-condition-type &serious &condition
  serious-condition?)

(define-condition-type &error &serious
  error?)


;;!! SRFI-36 I/O Conditions
;; Copyright (C) Michael Sperber (2002). All Rights Reserved.

;; &error 
;;   &i/o-error
;;     &i/o-port-error (has a port field)
;;       &i/o-read-error
;;       &i/o-write-error
;;       &i/o-closed-error
;;     &i/o-filename-error (has a filename field)
;;       &i/o-malformed-filename-error
;;       &i/o-file-protection-error 
;;         &i/o-file-is-read-only-error
;;       &i/o-file-already-exists-error
;;       &i/o-no-such-file-error
;;   &read-error

;;! This is a supertype for a set of more specific I/O errors.
(define-condition-type &i/o-error &error
  i/o-error?)

;;! This condition type specifies an I/O error that occurred
;; during an operation on a port. Condition objects belonging to
;; this type must specify a port in the port field.
(define-condition-type &i/o-port-error &i/o-error
  i/o-port-error?
  (port i/o-error-port))


;;! This condition type specifies a read error that occurred during
;; an operation on a port.
(define-condition-type &i/o-read-error &i/o-port-error
  i/o-read-error?)

;;! This condition type specifies a write error that occurred during
;; an operation on a port.
(define-condition-type &i/o-write-error &i/o-port-error
  i/o-write-error?)

;;! A condition of this type specifies that an operation tried to
;; operate on a closed port under the assumption that it is open.
(define-condition-type &i/o-closed-error &i/o-port-error
  i/o-closed-error?)

;;! This condition type specifies an I/O error that occurred during an
;; operation on a named file. Condition objects belonging to this type
;; must specify a file name in the filename field.
(define-condition-type &i/o-filename-error &i/o-error
  i/o-filename-error?
  (filename i/o-error-filename))

;;! This condition type indicates that a file name had an invalid format.
(define-condition-type &i/o-malformed-filename-error &i/o-filename-error
  i/o-malformed-filename-error?)

;;! A condition of this type specifies that an operation tried to operate
;; on a named file with insufficient access rights.
(define-condition-type &i/o-file-protection-error &i/o-filename-error
  i/o-file-protection-error?)

;;! A condition of this type specifies that an operation tried to operate
;; on a named read-only file under the assumption that it is writeable.
(define-condition-type &i/o-file-is-read-only-error &i/o-file-protection-error
  i/o-file-is-read-only-error?)

;;! A condition of this type specifies that an operation tried to operate
;; on an existing named file under the assumption that it does not exist.
(define-condition-type &i/o-file-already-exists-error &i/o-filename-error
  i/o-file-already-exists-error?)

;;! A condition of this type specifies that an operation tried to operate
;; on an non-existent named file under the assumption that it exists.
(define-condition-type &i/o-no-such-file-error &i/o-filename-error
  i/o-no-such-file-error?)

;;! A condition of this type specifies that a parse error happened during
;; a read operation. 
(define-condition-type &read-error &error
  read-error?
  (line read-error-line)
  (column read-error-column)
  (position read-error-position)
  (span read-error-span))
