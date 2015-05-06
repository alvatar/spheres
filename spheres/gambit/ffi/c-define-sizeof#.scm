;;! Build a size-of value equivalent to the C operator
;; c-build-sizeof float -> sizeof-float
(define-macro (c-define-sizeof scheme-type . rest)
  (let ((c-type (%%get-key-arg rest c-type: (symbol->string scheme-type))))
    `(define ,(string->symbol (string-append (symbol->string scheme-type) "-size"))
       ((c-lambda () size-t
                  ,(string-append "___result = sizeof(" c-type ");"))))))
