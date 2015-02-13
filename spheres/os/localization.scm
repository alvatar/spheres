;;!!! SRFI-29 Localization
;; .author Scott G. Miller, 2002
;; .author Alvaro Castro-Castilla, 2014-2015

;; Copyright (C) Scott G. Miller (2002). All Rights Reserved.


;; The association list in which bundles will be stored
(define *localization-bundles* '())

;; The current-language and current-country functions provided
;; here must be rewritten for each Scheme system to default to the
;; actual locale of the session
(define current-language
  (let ((current-language-value 'en))
    (lambda args
      (if (null? args)
          current-language-value
          (set! current-language-value (car args))))))

(define current-country
  (let ((current-country-value 'us))
    (lambda args
      (if (null? args)
          current-country-value
          (set! current-country-value (car args))))))

;; The load-bundle! and store-bundle! both return #f in this
;; reference implementation.  A compliant implementation need
;; not rewrite these procedures.
(define load-bundle!
  (lambda (bundle-specifier)
    #f))

(define store-bundle!
  (lambda (bundle-specifier)
    #f))

;; Declare a bundle of templates with a given bundle specifier
(define declare-bundle!
  (letrec ((remove-old-bundle
            (lambda (specifier bundle)
              (cond ((null? bundle) '())
                    ((equal? (caar bundle) specifier)
                     (cdr bundle))
                    (else (cons (car bundle)
                                (remove-old-bundle specifier
                                                   (cdr bundle))))))))
    (lambda (bundle-specifier bundle-assoc-list)
      (set! *localization-bundles*
            (cons (cons bundle-specifier bundle-assoc-list)
                  (remove-old-bundle bundle-specifier
                                     *localization-bundles*))))))

;;Retrieve a localized template given its package name and a template name
(define localized-template
  (letrec ((rdc
            (lambda (ls)
              (if (null? (cdr ls))
                  '()
                  (cons (car ls) (rdc (cdr ls))))))
           (find-bundle
            (lambda (specifier template-name)
              (cond ((assoc specifier *localization-bundles*) =>
                     (lambda (bundle) bundle))
                    ((null? specifier) #f)
                    (else (find-bundle (rdc specifier)
                                       template-name))))))
    (lambda (package-name template-name)
      (let loop ((specifier (cons package-name
                                  (list (current-language)
                                        (current-country)))))
        (and (not (null? specifier))
             (let ((bundle (find-bundle specifier template-name)))
               (and bundle
                    (cond ((assq template-name bundle) => cdr)
                          ((null? (cdr specifier)) #f)
                          (else (loop (rdc specifier)))))))))))
