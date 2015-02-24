;;!!! XHTML and HTML doctypes
;; .author Jim Ursetto
;; see HTML5 spec 8.1.1 "The DOCTYPE"
;; (http://dev.w3.org/html5/spec/Overview.html#the-doctype)

;; generic html doctype (introduced in HTML5; backward compatible with all known browsers)
(define doctype-html
  "<!DOCTYPE HTML>")
;; legacy html doctype -- don't use, provided only for completeness
(define doctype-html-legacy
  "<!DOCTYPE HTML SYSTEM \"about:legacy-compat\">")

;; obsolete permitted html doctypes
(define doctype-html-4.01-strict
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")
(define doctype-xhtml-1.0-strict
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
(define doctype-html-4.01-transitional
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
(define doctype-xhtml-1.0-transitional
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
(define doctype-html-4.01-frameset
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">")
(define doctype-xhtml-1.0-frameset
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">")
(define doctype-html-3.2
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">")
(define doctype-html-2.0
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">")

;; deprecated aliases for this module
(define html-4.01-strict doctype-html-4.01-strict)
(define xhtml-1.0-strict doctype-xhtml-1.0-strict)
(define html-4.01-transitional doctype-html-4.01-transitional)
(define xhtml-1.0-transitional doctype-xhtml-1.0-transitional)
(define html-4.01-frameset doctype-html-4.01-frameset)
(define xhtml-1.0-frameset doctype-xhtml-1.0-frameset)
(define html-3.2 doctype-html-3.2)
(define html-2.0 doctype-html-2.0)

;; example rules for sxml-transforms
;; (define doctype-rules
;;   `((doctype-html *preorder* . ,(constantly doctype-html))
;;     (doctype-html-4.01-strict *preorder* . ,(constantly doctype-html-4.01-strict))
;;     (doctype-xhtml-1.0-strict *preorder* . ,(constantly doctype-xhtml-1.0-strict))
;;     (doctype-html-4.01-transitional *preorder* . ,(constantly doctype-html-4.01-transitional))
;;     (doctype-xhtml-1.0-transitional *preorder* . ,(constantly doctype-xhtml-1.0-transitional))
;;     (doctype-html-4.01-frameset *preorder* . ,(constantly doctype-html-4.01-frameset))
;;     (doctype-xhtml-1.0-frameset *preorder* . ,(constantly doctype-xhtml-1.0-frameset))
;;     (doctype-html-3.2 *preorder* . ,(constantly doctype-html-3.2))
;;     (doctype-html-2.0 *preorder* . ,(constantly doctype-html-2.0))
;;     ;; deprecated aliases (probably retained for the foreseeable future, though)
;;     (html-4.01-strict *preorder* . ,(constantly doctype-html-4.01-strict))
;;     (xhtml-1.0-strict *preorder* . ,(constantly doctype-xhtml-1.0-strict))
;;     (html-4.01-transitional *preorder* . ,(constantly doctype-html-4.01-transitional))
;;     (xhtml-1.0-transitional *preorder* . ,(constantly doctype-xhtml-1.0-transitional))
;;     (html-4.01-frameset *preorder* . ,(constantly doctype-html-4.01-frameset))
;;     (xhtml-1.0-frameset *preorder* . ,(constantly doctype-xhtml-1.0-frameset))
;;     (html-3.2 *preorder* . ,(constantly doctype-html-3.2))
;;     (html-2.0 *preorder* . ,(constantly doctype-html-2.0))))
