;;!!! STX - Scheme-enabled XSLT processor
;;
;; STX is an XML transformation tool based on XSLT and Scheme which combines
;; a processor for most common XSLT stylesheets and a framework for their
;; extension in Scheme and provides an environment for a general-purpose
;; transformation of XML data. It integrates two functional languages - Scheme
;; and XSLT-like transformation language on the basis of the common data model -
;; SXML.

(define-library (spheres/markup stx)

  (import (spheres/core base)
          (spheres/markup sxml-tools)
          (spheres/markup sxpath)
          (spheres/markup sxpath-context-xlink))

  (include "stx.scm"))
