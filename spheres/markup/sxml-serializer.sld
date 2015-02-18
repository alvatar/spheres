;;!!! SXML serializer into XML and HTML
;;
;; Partial conformance with
;; [1] XSLT 2.0 and XQuery 1.0 Serialization
;; W3C Candidate Recommendation 3 November 2005
;; http://www.w3.org/TR/2005/CR-xslt-xquery-serialization-20051103/
;;
;; This software is in Public Domain.
;; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.

(define-library (spheres/markup sxml-serializer)
  (export srl:sxml->string
          srl:display-sxml
          srl:parameterizable
          srl:sxml->xml
          srl:sxml->xml-noindent
          srl:sxml->html
          srl:sxml->html-noindent)

  (include "sxml-serializer.scm"))
