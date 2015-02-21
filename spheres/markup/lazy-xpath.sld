;;!!! Lazy SXPath evaluation over lazy SXML documents
;;
;; In a lazy SXML document, each node may be a promise. If forced, the promise
;; results into an SXML node or a nodeset. For a nodeset, its members are SXML
;; nodes and promises in turn.
;; With every promise forced, a lazy SXML document must conform to SXML
;; Specification. In particular, an attribute node must occur before any child
;; nodes, attribute value must be atomic, etc.
;;
;; SXPath evaluation is lazy in that it results to a nodeset whose last member
;; may be a promise. Such a nodeset with a promise as its last member denotes
;; the first portion of the result. If the promise is forced, it is evaluated
;; into another nodeset, which corresponds to the next portion of the result.
;; SXPath evaluator returns the result in portions when some branch in the
;; document is to be forced in order to obtain the next part of the result.
;; However, a portion that is not the last one, must contain at least one result
;; node. To fulfill this requirement, branches of the document may be forced
;; until at least a result node for a portion is obtained.
;;
;; This software is in Public Domain.
;; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;;
;; Please send bug reports and comments to:
;;   lizorkin@hotbox.ru    Dmitry Lizorkin

(define-library (spheres/markup lazy-xpath)
  (export lazy-xpath:txpath
          lazy-xpath:xpath-expr
          lazy-xpath:sxpath)

  (import (spheres/core base)
          (spheres/string string)
          (spheres/markup sxpath-context-xlink))

  (include "lazy-xpath.scm"))
