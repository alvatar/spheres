;;!!! SXPath with context / XPathLink - query language for a set of linked documents
;;
;; SXPath with context provides the effective implementation for XPath reverse
;; axes ("parent::", "ancestor::" and such) on SXML documents.
;;
;; The limitation of SXML is the absense of an upward link from a child to its
;; parent, which makes the straightforward evaluation of XPath reverse axes
;; ineffective. The previous approach for evaluating reverse axes in SXPath was
;; searching for a parent from the root of the SXML tree.
;;
;; SXPath with context provides the fast reverse axes, which is achieved by
;; storing previously visited ancestors of the context node in the context.
;; With a special static analysis of an XPath expression, only the minimal
;; required number of ancestors is stored in the context on each location step.
;;
;; XLink is a language for describing links between resources using XML attributes
;; and namespaces. XLink provides expressive means for linking information in
;; different XML documents. With XLink, practical XML application data can be
;; expressed as several linked XML documents, rather than a single complicated XML
;; document. Such a design makes it very attractive to have a query language that
;; would inherently recognize XLink links and provide a natural navigation
;; mechanism over them.
;;
;; Such a query language has been designed and implemented in Scheme. This
;; language is an extension to XPath with 3 additional axes. The implementation
;; is naturally an extended SXPath. We call this language XPath with XLink
;; support, or XPathLink.
;;
;; Additionally, an HTML <A> hyperlink can be considered as a particular case of
;; an XLink link. This observation makes it possible to query HTML documents with
;; XPathLink as well. Neil W. Van Dyke <neil@neilvandyke.org> and his permissive
;; HTML parser HtmlPrag have made this feature possible.

(define-library (spheres/markup sxpath-context)
  (export txpath-with-context
          txpath/c
          sxpath-with-context
          sxpath/c
          xlink:load-linked-docs-with-params
          xlink:documents
          xlink:documents-embed
          sxml:document)

  (import (spheres/core base)
          (spheres/markup html)
          (spheres/markup sxml-tools)
          (spheres/markup sxpath))

  ;; SSAX: we are using the original version for this. Has to be first.
  (include "sxpath-context-xlink/ssax.scm")
  (include "sxpath-context-xlink/id/access-remote.scm")
  (include "sxpath-context-xlink/id/id.scm")
  (include "sxpath-context-xlink/id/mime.scm")
  (include "sxpath-context-xlink/id/http.scm")
  (include "sxpath-context-xlink/multi-parser.scm")
  (include "sxpath-context-xlink/xlink-parser.scm")
  (include "sxpath-context-xlink/xlink.scm")
  (include "sxpath-context-xlink/xpath-ast.scm")
  (include "sxpath-context-xlink/sxpath-context.scm"))
