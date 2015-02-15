;;!!! XHTML and HTML doctypes
;; .author Jim Ursetto
;; see HTML5 spec 8.1.1 "The DOCTYPE"
;; (http://dev.w3.org/html5/spec/Overview.html#the-doctype)

(define-library (spheres/fileformat html-doctype)
  (export doctype-html
          ;; legacy doctype string 
          doctype-html-legacy

          ;; obsolete permitted doctype strings
          doctype-html-4.01-strict
          doctype-xhtml-1.0-strict
          doctype-html-4.01-transitional
          doctype-xhtml-1.0-transitional
          doctype-html-4.01-frameset
          doctype-xhtml-1.0-frameset
          doctype-html-3.2
          doctype-html-2.0

          ;; sxml-transforms
          doctype-rules

          ;; deprecated aliases for users of older doctype egg
          html-4.01-strict
          xhtml-1.0-strict
          html-4.01-transitional
          xhtml-1.0-transitional
          html-4.01-frameset
          xhtml-1.0-frameset
          html-3.2
          html-2.0)

  (include "html-doctype.scm"))
