;;! define types for structs, unions and arrays
;; .author Álvaro Castro-Castilla, based on code by Estevo Castro
;; (c-define-type* (struct MyStruct))
;; (c-define-type* (union MyUnion))
;; (c-define-type* myType)
(define-macro (c-define-type* type/struct/union)
  (let* ((type (if (pair? type/struct/union)
                   (cadr type/struct/union)
                   type/struct/union))
         (struct-or-union (if (pair? type/struct/union)
                              (car type/struct/union)
                              #f))
         (type-str (symbol->string type))
         (release-type-str (string-append "___release_" (%%scheme-name->c-name type-str)))
         (type* (%%generic-symbol-append type-str "*"))
         (type*/nonnull (%%generic-symbol-append type-str "*/nonnull"))
         (type*/release-rc (%%generic-symbol-append type-str "*/release-rc")))
    (let ((expansion
           (%%begin-top-level-forms
            (if struct-or-union
                `(c-define-type ,type (,struct-or-union ,type-str))
                '())
            `(c-define-type ,type* (pointer ,type (,type*)))
            `(c-define-type ,type*/nonnull (nonnull-pointer ,type (,type*)))
            `(c-define-type ,type*/release-rc (nonnull-pointer ,type (,type*) ,release-type-str)))))
      (if #f ;; #t for debugging
          (pp `(definition:
                 (c-define-extended-type ,type)
                 expansion:
                 ,expansion)))
      expansion)))
