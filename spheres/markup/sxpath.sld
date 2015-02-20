;;!!! SXPath - SXML Query Language
;;
;; SXPath is a query language for SXML. It treats a location path as a composite
;; query over an XPath tree or its branch. A single step is a combination of a
;; projection, selection or a transitive closure. Multiple steps are combined via
;; join and union operations.
;;
;; Lower-level SXPath consists of a set of predicates, filters, selectors and
;; combinators, and higher-level abbreviated SXPath functions which are
;; implemented in terms of lower-level functions.
;;
;; Higher level SXPath functions are dealing with XPath expressions which may be
;; represented as a list of steps in the location path ("native" SXPath):
;;   (sxpath '(table (tr 3) td @ align))
;; or as a textual representation of XPath expressions which is compatible with
;; W3C XPath recommendation ("textual" SXPath):
;;   (sxpath "table/tr[3]/td/@align")
;;
;; An arbitrary converter implemented as a Scheme function may be used as a step
;; in location path of "native" SXPath, which makes it extremely powerful and
;; flexible tool. On other hand, a lot of W3C Recommendations such as XSLT,
;; XPointer, XLink depends on a textual XPath expressions.
;;
;; It is possible to combine "native" and "textual" location paths and location
;; step functions in one query, constructing an arbitrary XML query far beyond
;; capabilities of XPath. For example, the query
;;   (sxpath `("document/chapter[3]" ,relevant-links @ author)
;; makes a use of location step function relevant-links which implements an
;; arbitrary algorithm in Scheme.
;;
;; SXPath may be considered as a compiler from abbreviated XPath (extended with
;; native SXPath and location step functions) to SXPath primitives.

(define-library (spheres/markup sxpath)
  (export sxpath)

  (import (spheres/core base)
          (spheres/markup sxml-tools))

  ;; (include "sxpath/sxpath-lib.scm") ; included in sxml-tools
  (include "internal/xpath-parser.scm")
  (include "internal/sxpath-ext.scm")
  (include "internal/txpath.scm")
  (include "internal/sxpath-core.scm"))
