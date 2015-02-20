;;!!! DDO SXPath
;;
;; The optimized SXPath that implements distinct document order (DDO) of the
;; nodeset produced.
;;
;; Unlike conventional SXPath and SXPath with context, DDO SXPath guarantees that
;; the execution time is at worst polynomial of the XPath expression size and of
;; the SXML document size.
;;
;; The API of DDO SXPath is compatible of that in conventional SXPath. The main
;; following kinds of optimization methods are designed and implemented in DDO
;; SXPath:
;;
;; - All XPath axes are implemented to keep a nodeset in distinct document
;;   order (DDO). An axis can now be considered as a converter:
;;    nodeset_in_DDO --> nodeset_in_DDO
;; - Type inference for XPath expressions allows determining whether a
;;   predicate involves context-position implicitly;
;; - Faster evaluation for particular kinds of XPath predicates that involve
;;   context-position, like:  [position() > number] or [number];
;; - Sort-merge join algorithm implemented for XPath EqualityComparison of
;;   two nodesets;
;; - Deeply nested XPath predicates are evaluated at the very beginning of the
;;   evaluation phase, to guarantee that evaluation of deeply nested predicates
;;   is performed no more than once for each combination of
;;   (context-node, context-position, context-size)

(define-library (spheres/markup ddo-txpath)
  (export ddo:txpath
          ddo:xpath-expr
          ddo:sxpath)
  
  (import (spheres/markup sxml-tools)
          (spheres/markup sxpath-context-xlink))

  (include "ddo-txpath.scm"))
