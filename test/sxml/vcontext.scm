
(include "../../sxml-tools/tests/vsxpathlib.sch")
(include "../../sxml-tools/tests/xtest-harness.sch")
(include "../../sxml-tools/tests/vcontext.sch")

;; Validation tests for SXPath-with-context: "xpath-context.scm"
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;@ Document
(define vcntxt:old-doc
'(*TOP*
 (*PI* xml "version='1.0' encoding=\"Shift_JIS\"")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com")))))))
)

;------------------------------------------------
; The second step - creating an 'id-index' using 'id-attrs'

; This function forms an 'id-index' and inserts it in the document
;  document - a root node of the document (SXML presentation)
;  id-attrs - the result of the previous step
; A new SXML document is returned. It contains an auxiliary list with an
; 'id-index subtree. If the source document already contains such a
; subtree, it will be replaced. Other subtrees in an auxiliary list will
; remain unchanged.
; DL: local version of the function - prefixed with `vcntxt:'
(define (vcntxt:SXML->SXML+id document id-attrs)
  (let((aux-subtrees
        (let((aux ((select-kids (ntype?? '@@)) document)))
          (if(null? aux)
             '()
             (let rpt ((res '())
                       (to-see (cdar aux)))
               (cond
                 ((null? to-see) (reverse res))
                 ((equal? (caar to-see) 'id-index) (rpt res (cdr to-see)))
                 (else (rpt (cons (car to-see) res)
                            (cdr to-see)))))))))
    (let loop ((nodeset (list document))
               (id-index '()))
      (if(null? nodeset)
         (let((kids ((select-kids
                      (lambda (node)
                        (not (and (pair? node) (equal? (car node) '@@)))))
                     document)))
           (cons '*TOP*
                 (cons (cons '@@
                             (cons (cons 'id-index id-index)
                                   aux-subtrees))
                  kids)))
         (let((cur-node (car nodeset)))
           (cond
             ((not (pair? cur-node))  ; a text node
              (loop (cdr nodeset) id-index))
             ((assoc (car cur-node) id-attrs)
              =>
              (lambda (lst)
                (let((id-values
                      ((select-kids (lambda (x) #t))
                       ((sxml:filter (lambda (x) (member (car x) (cdr lst))))
                        ((select-kids (lambda (x) #t))
                         ((select-kids (ntype?? '@)) cur-node))))))
                  (loop
                   (append 
                    ((select-kids (ntype?? '*)) (car nodeset))
                    (cdr nodeset))
                   (append
                    id-index
                    (map
                     (lambda (x) (cons x cur-node))
                     id-values))))))
             (else
              (loop
               (append ((select-kids (ntype?? '*)) (car nodeset)) (cdr nodeset))
               id-index))))))))

(define vcntxt:doc
  (vcntxt:SXML->SXML+id
   vcntxt:old-doc
   '((item id) (chapter id) (section ID) (appendix id))))

;@ Namespace binding
(define vcntxt:ns-binding (list (cons 'xlink "http://www.w3.org/1999/xlink")))


;=========================================================================
; Test functions

(define (draft:test-xpath xpath-string . ns-binding)
  (let ((res (if (null? ns-binding)
                 (draft:xpath xpath-string)
                 (draft:xpath xpath-string (car ns-binding)))))
    (if (not res)
        '@error@    ; this can never be an expected result
        (res vcntxt:doc))))

(define (draft:test-xpointer xpath-string . ns-binding)  
  (let ((res (if (null? ns-binding)
	       (draft:xpointer xpath-string)
	       (draft:xpointer xpath-string (car ns-binding)))))
    (if (not res)
        '@error@    ; this can never be an expected result
        (res vcntxt:doc))))
       
(define (draft:test-xpointer+vars xpath-string var-binding . ns-binding)
  (let ((res (if (null? ns-binding)
                 (draft:xpointer xpath-string)
                 (draft:xpointer xpath-string (car ns-binding)))))
    (if (not res)
        '@error@    ; this can never be an expected result
        (res vcntxt:doc var-binding))))


;=========================================================================
; Test assertions are performed here

 ; SXPath testing
; sxml:xpath
(xtest-assert ; Expected result:
'("Text node")
; <--- of:
(draft:xpath ".")
'("Text node")
)

 ; These XPath expressions are equal:

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
draft:test-xpath
"child::*/child::*[2]"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
draft:test-xpath
"*/*[2]"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
draft:test-xpath
"/*/*[2]"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
draft:test-xpath
"descendant-or-self::node()[attribute::id ='toc1']"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
draft:test-xpath
"//*[attribute::* ='toc1']"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
draft:test-xpath
"/descendant::node()[attribute::id][1]"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
draft:test-xpath
"//*[ self::node() = id('toc1') ]"
)

 ; Node tests

; draft:test-xpath
(xtest-assert ; Expected result:
'((*PI* xml "version='1.0' encoding=\"Shift_JIS\""))
; <--- of:
draft:test-xpath
"descendant::processing-instruction()"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((*PI* xml "version='1.0' encoding=\"Shift_JIS\""))
; <--- of:
draft:test-xpath
"descendant::processing-instruction( 'xml' )"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'()
; <--- of:
draft:test-xpath
"descendant::processing-instruction( 'smth else' )"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'()
; <--- of:
draft:test-xpath
"//*[ self::processing-instruction('smth else') ]"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'("chapter1"
 "chapter2"
 "chapter3"
 "chapter4"
 "chapter5"
 "chapter6"
 "chapter7"
 "Abstract"
 "This document describes about XLink Engine..."
 "Introduction"
 "This document is written in XML (eXtensible Markup Language) ver.1.0."
 "What is XLink?"
 "hyperlink"
 "What is XPointer?"
 "XPointer is the fragment identifier of documents having the mime-type hogehoge."
 "Models for using XLink/XPointer "
 "There are important keywords."
 "samples"
 "Conclusion"
 "Thanks a lot."
 "Who1"
 "XML Linking Language (XLink)"
 "foo.com"
 "Who2"
 "XML Pointing Language (XPointer)"
 "boo.com")
; <--- of:
draft:test-xpath
"descendant-or-self::text()"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'("boo.com")
; <--- of:
draft:test-xpath
"descendant-or-self::text()[ self::node() = 'boo.com' ]"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'("chapter1" "chapter2" "chapter3" "chapter4" "chapter5" "chapter6" "chapter7")
; <--- of:
draft:test-xpath
"*/*/text()"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "extended")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "arc")
 (http://www.w3.org/1999/xlink:type "arc"))
; <--- of:
draft:test-xpath
"//attribute::xlink:type"
vcntxt:ns-binding
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:label "hoge")
 (http://www.w3.org/1999/xlink:to "hoge")
 (http://www.w3.org/1999/xlink:from "hoge"))
; <--- of:
draft:test-xpath
"//attribute::xlink:*[ self::* = 'hoge' ]"
vcntxt:ns-binding
)

; draft:test-xpath
(xtest-assert ; Expected result:
'()
; <--- of:
draft:test-xpath
"//attribute::xlink:*"
(list (cons 'xlink "http://www.else.com"))
)


;------------------------------------------------
; Axises

; draft:test-xpath
(xtest-assert ; Expected result:
'((id "chap3"))
; <--- of:
draft:test-xpath
"*[1]/*[9]/*[3]/@*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((id "toc3"))
; <--- of:
draft:test-xpath
"*[1]/*[4]/@id"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink")))
; <--- of:
draft:test-xpath
"*[1]/*[9]/*[3]/."
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((id "toc3"))
; <--- of:
draft:test-xpath
"*[1]/*[4]/@id/."
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com"))))))
; <--- of:
draft:test-xpath
"*[1]/*[9]/*[3]/.."
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3"))
; <--- of:
draft:test-xpath
"*[1]/*[4]/@id/.."
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (chapter (@ (id "chap6")) (title "samples"))
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com")))))
; <--- of:
draft:test-xpath
"*[1]/*[9]/*[3]/following-sibling::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'()
; <--- of:
draft:test-xpath
"*[1]/*[4]/@id/following-sibling::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine...")))
; <--- of:
draft:test-xpath
"*[1]/*[9]/*[3]/preceding-sibling::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'()
; <--- of:
draft:test-xpath
"*[1]/*[4]/@id/preceding-sibling::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 (p "Thanks a lot.")
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (author "Who1")
 (title "XML Linking Language (XLink)")
 (ref "foo.com")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (author "Who2")
 (title "XML Pointing Language (XPointer)")
 (ref "boo.com"))
; <--- of:
draft:test-xpath
"*[1]/*[9]/*[3]/following::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'("chapter3"
 (item (@ (id "toc4")) "chapter4")
 "chapter4"
 (item (@ (id "toc5")) "chapter5")
 "chapter5"
 (item (@ (id "toc6")) "chapter6")
 "chapter6"
 (item (@ (id "toc7")) "chapter7")
 "chapter7"
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (title "Abstract")
 "Abstract"
 (p "This document describes about XLink Engine...")
 "This document describes about XLink Engine..."
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (title "Introduction")
 "Introduction"
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 "This document is written in XML (eXtensible Markup Language) ver.1.0."
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 "What is XLink?"
 (p "hyperlink")
 "hyperlink"
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (title "What is XPointer?")
 "What is XPointer?"
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 "XPointer is the fragment identifier of documents having the mime-type hogehoge."
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 "Models for using XLink/XPointer "
 (p "There are important keywords.")
 "There are important keywords."
 (chapter (@ (id "chap6")) (title "samples"))
 (title "samples")
 "samples"
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "Conclusion")
 "Conclusion"
 (p "Thanks a lot.")
 "Thanks a lot."
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com"))))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (author "Who1")
 "Who1"
 (title "XML Linking Language (XLink)")
 "XML Linking Language (XLink)"
 (ref "foo.com")
 "foo.com"
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (author "Who2")
 "Who2"
 (title "XML Pointing Language (XPointer)")
 "XML Pointing Language (XPointer)"
 (ref "boo.com")
 "boo.com")
; <--- of:
draft:test-xpath
"*[1]/*[4]/@id/following::node()"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (p "This document describes about XLink Engine...")
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (item (@ (id "toc7")) "chapter7")
 (item (@ (id "toc6")) "chapter6")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc2")) "chapter2")
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))))
; <--- of:
draft:test-xpath
"*[1]/*[9]/*[3]/preceding::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2")
 (item (@ (id "toc1")) "chapter1")
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 cont")
   (http://www.w3.org/1999/xlink:role "text/xml")
   (http://www.w3.org/1999/xlink:label "hoge")
   (http://www.w3.org/1999/xlink:href "#chap1")))
 (loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (multidirectional
   (@ (http://www.w3.org/1999/xlink:type "extended"))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 toc")
     (http://www.w3.org/1999/xlink:role "booboo")
     (http://www.w3.org/1999/xlink:label "boo")
     (http://www.w3.org/1999/xlink:href "#toc1")))
   (loc
    (@
     (http://www.w3.org/1999/xlink:type "locator")
     (http://www.w3.org/1999/xlink:title "Chap.1 cont")
     (http://www.w3.org/1999/xlink:role "text/xml")
     (http://www.w3.org/1999/xlink:label "hoge")
     (http://www.w3.org/1999/xlink:href "#chap1")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "hoge")
     (http://www.w3.org/1999/xlink:title "Traversal to content page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "boo")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))
   (arc
    (@
     (http://www.w3.org/1999/xlink:type "arc")
     (http://www.w3.org/1999/xlink:to "boo")
     (http://www.w3.org/1999/xlink:title "Traversal to toc page")
     (http://www.w3.org/1999/xlink:show "replace")
     (http://www.w3.org/1999/xlink:from "hoge")
     (http://www.w3.org/1999/xlink:actuate "onRequest")))))
; <--- of:
draft:test-xpath
"*[1]/*[4]/@id/preceding::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com"))))))
; <--- of:
draft:test-xpath
"*[1]/*[9]/*[3]/parent::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3"))
; <--- of:
draft:test-xpath
"*[1]/*[4]/@id/parent::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 (*TOP*
  (@@
   (id-index
     ("toc1" item (@ (id "toc1")) "chapter1")
     ("toc2" item (@ (id "toc2")) "chapter2")
     ("toc3" item (@ (id "toc3")) "chapter3")
     ("toc4" item (@ (id "toc4")) "chapter4")
     ("toc5" item (@ (id "toc5")) "chapter5")
     ("toc6" item (@ (id "toc6")) "chapter6")
     ("toc7" item (@ (id "toc7")) "chapter7")
     ("chap1"
      chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
     ("chap2"
      chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
     ("sec2-1"
      section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0."))
     ("chap3"
      chapter
      (@ (id "chap3"))
      (title "What is XLink?")
      (p "hyperlink"))
     ("chap4"
      chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
     ("chap5"
      chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
     ("chap6" chapter (@ (id "chap6")) (title "samples"))
     ("chap7"
      chapter
      (@ (id "chap7"))
      (title "Conclusion")
      (p "Thanks a lot."))
     ("here"
      appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))
  (*PI* xml "version='1.0' encoding=\"Shift_JIS\"")
  (doc
   (multidirectional
     (@ (http://www.w3.org/1999/xlink:type "extended"))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 toc")
       (http://www.w3.org/1999/xlink:role "booboo")
       (http://www.w3.org/1999/xlink:label "boo")
       (http://www.w3.org/1999/xlink:href "#toc1")))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 cont")
       (http://www.w3.org/1999/xlink:role "text/xml")
       (http://www.w3.org/1999/xlink:label "hoge")
       (http://www.w3.org/1999/xlink:href "#chap1")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "hoge")
       (http://www.w3.org/1999/xlink:title "Traversal to content page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "boo")
       (http://www.w3.org/1999/xlink:actuate "onRequest")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "boo")
       (http://www.w3.org/1999/xlink:title "Traversal to toc page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "hoge")
       (http://www.w3.org/1999/xlink:actuate "onRequest"))))
   (item (@ (id "toc1")) "chapter1")
   (item (@ (id "toc2")) "chapter2")
   (item (@ (id "toc3")) "chapter3")
   (item (@ (id "toc4")) "chapter4")
   (item (@ (id "toc5")) "chapter5")
   (item (@ (id "toc6")) "chapter6")
   (item (@ (id "toc7")) "chapter7")
   (body
    (chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
    (chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
    (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
    (chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
    (chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
    (chapter (@ (id "chap6")) (title "samples"))
    (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
    (appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))))
; <--- of:
draft:test-xpath
"*[1]/*[9]/*[3]/ancestor::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 (*TOP*
  (@@
   (id-index
     ("toc1" item (@ (id "toc1")) "chapter1")
     ("toc2" item (@ (id "toc2")) "chapter2")
     ("toc3" item (@ (id "toc3")) "chapter3")
     ("toc4" item (@ (id "toc4")) "chapter4")
     ("toc5" item (@ (id "toc5")) "chapter5")
     ("toc6" item (@ (id "toc6")) "chapter6")
     ("toc7" item (@ (id "toc7")) "chapter7")
     ("chap1"
      chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
     ("chap2"
      chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
     ("sec2-1"
      section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0."))
     ("chap3"
      chapter
      (@ (id "chap3"))
      (title "What is XLink?")
      (p "hyperlink"))
     ("chap4"
      chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
     ("chap5"
      chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
     ("chap6" chapter (@ (id "chap6")) (title "samples"))
     ("chap7"
      chapter
      (@ (id "chap7"))
      (title "Conclusion")
      (p "Thanks a lot."))
     ("here"
      appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))
  (*PI* xml "version='1.0' encoding=\"Shift_JIS\"")
  (doc
   (multidirectional
     (@ (http://www.w3.org/1999/xlink:type "extended"))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 toc")
       (http://www.w3.org/1999/xlink:role "booboo")
       (http://www.w3.org/1999/xlink:label "boo")
       (http://www.w3.org/1999/xlink:href "#toc1")))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 cont")
       (http://www.w3.org/1999/xlink:role "text/xml")
       (http://www.w3.org/1999/xlink:label "hoge")
       (http://www.w3.org/1999/xlink:href "#chap1")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "hoge")
       (http://www.w3.org/1999/xlink:title "Traversal to content page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "boo")
       (http://www.w3.org/1999/xlink:actuate "onRequest")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "boo")
       (http://www.w3.org/1999/xlink:title "Traversal to toc page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "hoge")
       (http://www.w3.org/1999/xlink:actuate "onRequest"))))
   (item (@ (id "toc1")) "chapter1")
   (item (@ (id "toc2")) "chapter2")
   (item (@ (id "toc3")) "chapter3")
   (item (@ (id "toc4")) "chapter4")
   (item (@ (id "toc5")) "chapter5")
   (item (@ (id "toc6")) "chapter6")
   (item (@ (id "toc7")) "chapter7")
   (body
    (chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
    (chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
    (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
    (chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
    (chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
    (chapter (@ (id "chap6")) (title "samples"))
    (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
    (appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))))
; <--- of:
draft:test-xpath
"*[1]/*[4]/@id/ancestor::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (body
  (chapter
    (@ (id "chap1"))
    (title "Abstract")
    (p "This document describes about XLink Engine..."))
  (chapter
    (@ (id "chap2"))
    (title "Introduction")
    (section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
  (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
  (chapter
    (@ (id "chap4"))
    (title "What is XPointer?")
    (p
     "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
  (chapter
    (@ (id "chap5"))
    (title "Models for using XLink/XPointer ")
    (p "There are important keywords."))
  (chapter (@ (id "chap6")) (title "samples"))
  (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
  (appendix
    (@ (id "here"))
    (bibliographic
      (item
       (author "Who1")
       (title "XML Linking Language (XLink)")
       (ref "foo.com"))
      (item
       (author "Who2")
       (title "XML Pointing Language (XPointer)")
       (ref "boo.com")))))
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 (*TOP*
  (@@
   (id-index
     ("toc1" item (@ (id "toc1")) "chapter1")
     ("toc2" item (@ (id "toc2")) "chapter2")
     ("toc3" item (@ (id "toc3")) "chapter3")
     ("toc4" item (@ (id "toc4")) "chapter4")
     ("toc5" item (@ (id "toc5")) "chapter5")
     ("toc6" item (@ (id "toc6")) "chapter6")
     ("toc7" item (@ (id "toc7")) "chapter7")
     ("chap1"
      chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
     ("chap2"
      chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
     ("sec2-1"
      section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0."))
     ("chap3"
      chapter
      (@ (id "chap3"))
      (title "What is XLink?")
      (p "hyperlink"))
     ("chap4"
      chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
     ("chap5"
      chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
     ("chap6" chapter (@ (id "chap6")) (title "samples"))
     ("chap7"
      chapter
      (@ (id "chap7"))
      (title "Conclusion")
      (p "Thanks a lot."))
     ("here"
      appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))
  (*PI* xml "version='1.0' encoding=\"Shift_JIS\"")
  (doc
   (multidirectional
     (@ (http://www.w3.org/1999/xlink:type "extended"))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 toc")
       (http://www.w3.org/1999/xlink:role "booboo")
       (http://www.w3.org/1999/xlink:label "boo")
       (http://www.w3.org/1999/xlink:href "#toc1")))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 cont")
       (http://www.w3.org/1999/xlink:role "text/xml")
       (http://www.w3.org/1999/xlink:label "hoge")
       (http://www.w3.org/1999/xlink:href "#chap1")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "hoge")
       (http://www.w3.org/1999/xlink:title "Traversal to content page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "boo")
       (http://www.w3.org/1999/xlink:actuate "onRequest")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "boo")
       (http://www.w3.org/1999/xlink:title "Traversal to toc page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "hoge")
       (http://www.w3.org/1999/xlink:actuate "onRequest"))))
   (item (@ (id "toc1")) "chapter1")
   (item (@ (id "toc2")) "chapter2")
   (item (@ (id "toc3")) "chapter3")
   (item (@ (id "toc4")) "chapter4")
   (item (@ (id "toc5")) "chapter5")
   (item (@ (id "toc6")) "chapter6")
   (item (@ (id "toc7")) "chapter7")
   (body
    (chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
    (chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
    (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
    (chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
    (chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
    (chapter (@ (id "chap6")) (title "samples"))
    (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
    (appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))))
; <--- of:
draft:test-xpath
"*[1]/*[9]/*[3]/ancestor-or-self::*"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((id "toc3")
 (item (@ (id "toc3")) "chapter3")
 (doc
  (multidirectional
    (@ (http://www.w3.org/1999/xlink:type "extended"))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 toc")
      (http://www.w3.org/1999/xlink:role "booboo")
      (http://www.w3.org/1999/xlink:label "boo")
      (http://www.w3.org/1999/xlink:href "#toc1")))
    (loc
     (@
      (http://www.w3.org/1999/xlink:type "locator")
      (http://www.w3.org/1999/xlink:title "Chap.1 cont")
      (http://www.w3.org/1999/xlink:role "text/xml")
      (http://www.w3.org/1999/xlink:label "hoge")
      (http://www.w3.org/1999/xlink:href "#chap1")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "hoge")
      (http://www.w3.org/1999/xlink:title "Traversal to content page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "boo")
      (http://www.w3.org/1999/xlink:actuate "onRequest")))
    (arc
     (@
      (http://www.w3.org/1999/xlink:type "arc")
      (http://www.w3.org/1999/xlink:to "boo")
      (http://www.w3.org/1999/xlink:title "Traversal to toc page")
      (http://www.w3.org/1999/xlink:show "replace")
      (http://www.w3.org/1999/xlink:from "hoge")
      (http://www.w3.org/1999/xlink:actuate "onRequest"))))
  (item (@ (id "toc1")) "chapter1")
  (item (@ (id "toc2")) "chapter2")
  (item (@ (id "toc3")) "chapter3")
  (item (@ (id "toc4")) "chapter4")
  (item (@ (id "toc5")) "chapter5")
  (item (@ (id "toc6")) "chapter6")
  (item (@ (id "toc7")) "chapter7")
  (body
   (chapter
     (@ (id "chap1"))
     (title "Abstract")
     (p "This document describes about XLink Engine..."))
   (chapter
     (@ (id "chap2"))
     (title "Introduction")
     (section
       (@ (ID "sec2-1"))
       (p
        "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
   (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
   (chapter
     (@ (id "chap4"))
     (title "What is XPointer?")
     (p
      "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
   (chapter
     (@ (id "chap5"))
     (title "Models for using XLink/XPointer ")
     (p "There are important keywords."))
   (chapter (@ (id "chap6")) (title "samples"))
   (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
   (appendix
     (@ (id "here"))
     (bibliographic
       (item
        (author "Who1")
        (title "XML Linking Language (XLink)")
        (ref "foo.com"))
       (item
        (author "Who2")
        (title "XML Pointing Language (XPointer)")
        (ref "boo.com"))))))
 (*TOP*
  (@@
   (id-index
     ("toc1" item (@ (id "toc1")) "chapter1")
     ("toc2" item (@ (id "toc2")) "chapter2")
     ("toc3" item (@ (id "toc3")) "chapter3")
     ("toc4" item (@ (id "toc4")) "chapter4")
     ("toc5" item (@ (id "toc5")) "chapter5")
     ("toc6" item (@ (id "toc6")) "chapter6")
     ("toc7" item (@ (id "toc7")) "chapter7")
     ("chap1"
      chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
     ("chap2"
      chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
     ("sec2-1"
      section
      (@ (ID "sec2-1"))
      (p
       "This document is written in XML (eXtensible Markup Language) ver.1.0."))
     ("chap3"
      chapter
      (@ (id "chap3"))
      (title "What is XLink?")
      (p "hyperlink"))
     ("chap4"
      chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
     ("chap5"
      chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
     ("chap6" chapter (@ (id "chap6")) (title "samples"))
     ("chap7"
      chapter
      (@ (id "chap7"))
      (title "Conclusion")
      (p "Thanks a lot."))
     ("here"
      appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))
  (*PI* xml "version='1.0' encoding=\"Shift_JIS\"")
  (doc
   (multidirectional
     (@ (http://www.w3.org/1999/xlink:type "extended"))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 toc")
       (http://www.w3.org/1999/xlink:role "booboo")
       (http://www.w3.org/1999/xlink:label "boo")
       (http://www.w3.org/1999/xlink:href "#toc1")))
     (loc
      (@
       (http://www.w3.org/1999/xlink:type "locator")
       (http://www.w3.org/1999/xlink:title "Chap.1 cont")
       (http://www.w3.org/1999/xlink:role "text/xml")
       (http://www.w3.org/1999/xlink:label "hoge")
       (http://www.w3.org/1999/xlink:href "#chap1")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "hoge")
       (http://www.w3.org/1999/xlink:title "Traversal to content page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "boo")
       (http://www.w3.org/1999/xlink:actuate "onRequest")))
     (arc
      (@
       (http://www.w3.org/1999/xlink:type "arc")
       (http://www.w3.org/1999/xlink:to "boo")
       (http://www.w3.org/1999/xlink:title "Traversal to toc page")
       (http://www.w3.org/1999/xlink:show "replace")
       (http://www.w3.org/1999/xlink:from "hoge")
       (http://www.w3.org/1999/xlink:actuate "onRequest"))))
   (item (@ (id "toc1")) "chapter1")
   (item (@ (id "toc2")) "chapter2")
   (item (@ (id "toc3")) "chapter3")
   (item (@ (id "toc4")) "chapter4")
   (item (@ (id "toc5")) "chapter5")
   (item (@ (id "toc6")) "chapter6")
   (item (@ (id "toc7")) "chapter7")
   (body
    (chapter
      (@ (id "chap1"))
      (title "Abstract")
      (p "This document describes about XLink Engine..."))
    (chapter
      (@ (id "chap2"))
      (title "Introduction")
      (section
        (@ (ID "sec2-1"))
        (p
         "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
    (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
    (chapter
      (@ (id "chap4"))
      (title "What is XPointer?")
      (p
       "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
    (chapter
      (@ (id "chap5"))
      (title "Models for using XLink/XPointer ")
      (p "There are important keywords."))
    (chapter (@ (id "chap6")) (title "samples"))
    (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
    (appendix
      (@ (id "here"))
      (bibliographic
        (item
         (author "Who1")
         (title "XML Linking Language (XLink)")
         (ref "foo.com"))
        (item
         (author "Who2")
         (title "XML Pointing Language (XPointer)")
         (ref "boo.com"))))))))
; <--- of:
draft:test-xpath
"*[1]/*[4]/@id/ancestor-or-self::*"
)


;------------------------------------------------
; position() and last() functions

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
draft:test-xpath
"*/*[position()=2]"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc4")) "chapter4"))
; <--- of:
draft:test-xpath
"*/*[position()=last()-4]"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc6")) "chapter6"))
; <--- of:
draft:test-xpath
"*/*[position()>=4 and position()<last()-1]"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
draft:test-xpath
"*/*[9]/*[position()=last()-position()]"
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
draft:test-xpath
"*/*[9]/*[position()=4]"
)

;DL, 05.02.04: new test cases recently added for position-based location paths

(xtest-assert ; Expected result:
'((p "hyperlink")
 (p "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (p "There are important keywords."))
; <--- of:
draft:test-xpath
"doc/body/chapter[position()>=3 and position()<=5]/p[1]"
)

(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2")
 (item (author "Who2") (title "XML Pointing Language (XPointer)") (ref "boo.com")))
; <--- of:
draft:test-xpath
"//item[2]"
)


;------------------------------------------------
; Predicates

; draft:test-xpath
(xtest-assert ; Expected result:
'((loc
  (@
   (http://www.w3.org/1999/xlink:type "locator")
   (http://www.w3.org/1999/xlink:title "Chap.1 toc")
   (http://www.w3.org/1999/xlink:role "booboo")
   (http://www.w3.org/1999/xlink:label "boo")
   (http://www.w3.org/1999/xlink:href "#toc1")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
 (arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "boo")
   (http://www.w3.org/1999/xlink:title "Traversal to toc page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "hoge")
   (http://www.w3.org/1999/xlink:actuate "onRequest"))))
; <--- of:
draft:test-xpath
"*[1]/*[1]/*[attribute::xlink:*][attribute::* = 'boo']"
vcntxt:ns-binding
)

; draft:test-xpath
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink")))
; <--- of:
draft:test-xpath
"descendant-or-self::node()[self::chapter]\r\n                               [self::* = id(' chap1 chap3 chap6 chap7   ')]\r\n                               [*[1] = 'What is XLink?']"
)


;=========================================================================
; SXPointer testing

;------------------------------------------------
; Arithmetic operations

; draft:test-xpointer
(xtest-assert ; Expected result:
'20
; <--- of:
draft:test-xpointer
"xpointer( -2 + 4* (6-1) + 6 div 3 )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( 7>4 and not( false() ) and ('err'= 12 or true() ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( true() > false() )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( '  15 ' = 15 = true() )"
)


;------------------------------------------------
; Variable reference

; draft:test-xpointer+vars
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer+vars
"xpointer(   $x and true() )"
(list (cons 'x #t))
)

; draft:test-xpointer+vars
(xtest-assert ; Expected result:
'41
; <--- of:
draft:test-xpointer+vars
"xpointer(   $y - 4)"
(list (cons 'y 45) (cons 'z '()))
)

; draft:test-xpointer+vars
(xtest-assert ; Expected result:
'"variable value"
; <--- of:
draft:test-xpointer+vars
"xpointer(   $z )"
(list (cons 'z "variable value"))
)

; draft:test-xpointer+vars
(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2"))
; <--- of:
draft:test-xpointer+vars
"xpointer(   $var[2] | id('toc2') )"
(list (cons 'var '()))
)


;------------------------------------------------
; Datatype convertion

; draft:test-xpointer
(xtest-assert ; Expected result:
'-34
; <--- of:
draft:test-xpointer
"xpointer( number(' -34 ') )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'-34.67
; <--- of:
draft:test-xpointer
"xpointer( number(' -34.67 ') )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'1
; <--- of:
draft:test-xpointer
"xpointer( number( true() ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'0
; <--- of:
draft:test-xpointer
"xpointer( number(false()) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( boolean( -56 ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#f
; <--- of:
draft:test-xpointer
"xpointer( boolean( 0 ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( boolean( 'ere' ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#f
; <--- of:
draft:test-xpointer
"xpointer( boolean( '' ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'"0"
; <--- of:
draft:test-xpointer
"xpointer( string( 0 ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'"-76"
; <--- of:
draft:test-xpointer
"xpointer( string( -76 ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'"true"
; <--- of:
draft:test-xpointer
"xpointer( string( true() ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'"false"
; <--- of:
draft:test-xpointer
"xpointer( string( false() ) )"
)


;------------------------------------------------
; Core XPath function library

; draft:test-xpointer
(xtest-assert ; Expected result:
'9
; <--- of:
draft:test-xpointer
"xpointer( count( //item ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'0
; <--- of:
draft:test-xpointer
"xpointer( count( id('no_such_element') ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'0
; <--- of:
draft:test-xpointer
"xpointer( floor(.4) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'-3
; <--- of:
draft:test-xpointer
"xpointer( floor(-2.5) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'1
; <--- of:
draft:test-xpointer
"xpointer( ceiling(.4) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'-2
; <--- of:
draft:test-xpointer
"xpointer( ceiling(-2.5) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'0
; <--- of:
draft:test-xpointer
"xpointer( round(.4) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'5
; <--- of:
draft:test-xpointer
"xpointer( round(4.51) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'-3
; <--- of:
draft:test-xpointer
"xpointer( round(-2.51) )"
)


;------------------------------------------------
; Comparison operations (for simple datatypes)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#f
; <--- of:
draft:test-xpointer
"xpointer( false() = 12 )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( true() = 12 )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( true() != 0 )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( false() = '' )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#f
; <--- of:
draft:test-xpointer
"xpointer( false() = 'smth' )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#f
; <--- of:
draft:test-xpointer
"xpointer( true() != 'smth' )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( '12  ' = 12 )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( '123  ' != 12 )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( true() > 0 )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( false() < 1 )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#f
; <--- of:
draft:test-xpointer
"xpointer( '  12' <= 1 )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( '  12' >= '  -2 ' )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( 'ee' <= 1 )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#f
; <--- of:
draft:test-xpointer
"xpointer( 'aaa' < 'bbb' )"
)


;------------------------------------------------
; Union operation

; draft:test-xpointer
(xtest-assert ; Expected result:
'((item (@ (id "toc6")) "chapter6")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0."))))
; <--- of:
draft:test-xpointer
"xpointer( *[1]/*[7] | *[1]/*[9]/*[2] )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com")))))
; <--- of:
draft:test-xpointer
"xpointer( id('toc2') | id('chap4') | id('here') )"
)


;------------------------------------------------
; Nodeset comparison

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( */*[9]/*[5]/following::node() = 'ConclusionThanks a lot.')"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( */*[9]/*[5]/following::node() = //appendix//item[1] )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#f
; <--- of:
draft:test-xpointer
"xpointer( */*[9]/*[5]/following::node() = \r\n                    */*[9]/*[6]/preceding-sibling::node() )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#t
; <--- of:
draft:test-xpointer
"xpointer( (/descendant::text()[23] | /descendant::text()[24]) = \r\n\t\t( //*[self::node() = 'foo.com'] | *[1]/*[1] ) )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'#f
; <--- of:
draft:test-xpointer
"xpointer( //xlink:label[self::* = 'hoge'] > id( 'chap6' ) )"
vcntxt:ns-binding
)


;------------------------------------------------
; Child sequence

; draft:test-xpointer
(xtest-assert ; Expected result:
'((appendix
   (@ (id "here"))
   (bibliographic
     (item
      (author "Who1")
      (title "XML Linking Language (XLink)")
      (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com")))))
; <--- of:
draft:test-xpointer
"here"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'((item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com")))
; <--- of:
draft:test-xpointer
"here/1/1"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'()
; <--- of:
draft:test-xpointer
"toc1/2"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'((arc
  (@
   (http://www.w3.org/1999/xlink:type "arc")
   (http://www.w3.org/1999/xlink:to "hoge")
   (http://www.w3.org/1999/xlink:title "Traversal to content page")
   (http://www.w3.org/1999/xlink:show "replace")
   (http://www.w3.org/1999/xlink:from "boo")
   (http://www.w3.org/1999/xlink:actuate "onRequest"))))
; <--- of:
draft:test-xpointer
"/1/1/3"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'((item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com")))
; <--- of:
draft:test-xpointer
"/1/9/8/1/2"
)


;------------------------------------------------
; xmlns usage

; draft:test-xpointer
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "extended")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "arc")
 (http://www.w3.org/1999/xlink:type "arc"))
; <--- of:
draft:test-xpointer
"xmlns( xl = http://www.w3.org/1999/xlink) \r\n     xpointer( //attribute::xl:type )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:label "boo")
 (http://www.w3.org/1999/xlink:from "boo")
 (http://www.w3.org/1999/xlink:to "boo"))
; <--- of:
draft:test-xpointer
"xmlns( xl = http://www.w3.org/1999/xlink) \r\n     xpointer( descendant::node()/attribute::xl:*[self::node() = 'boo'] )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'()
; <--- of:
draft:test-xpointer
"xmlns( smth = http://smth ) \r\n     xpointer( descendant::node()/attribute::smth:*[self::node() = 'boo'] )"
)


;------------------------------------------------
; Several XPointer parts

; draft:test-xpointer
(xtest-assert ; Expected result:
'((title "Abstract")
 (title "Introduction")
 (title "What is XLink?")
 (title "What is XPointer?")
 (title "Models for using XLink/XPointer ")
 (title "samples")
 (title "Conclusion"))
; <--- of:
draft:test-xpointer
"xpointer( doc/multidirectional/loc[3] )\r\n     xpointer( doc/body/chapter/title )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:label "boo")
 (http://www.w3.org/1999/xlink:from "boo")
 (http://www.w3.org/1999/xlink:to "boo"))
; <--- of:
draft:test-xpointer
"xpointer( doc/multidirectional/loc[3] )\r\n     xmlns( xlink=http://www.w3.org/1999/xlink)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:*[self::* = 'boo'] )\r\n     xpointer( doc/body/chapter/title )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:actuate "onRequest")
 (http://www.w3.org/1999/xlink:actuate "onRequest"))
; <--- of:
draft:test-xpointer
"xmlns( xlink=http://first_try)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:actuate )\r\n     xmlns( xlink=http://second_try)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:actuate )\r\n     xmlns( xlink=http://www.w3.org/1999/xlink)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:actuate )\r\n     xpointer( false() )"
)

; draft:test-xpointer
(xtest-assert ; Expected result:
'((item (@ (id "toc7")) "chapter7"))
; <--- of:
draft:test-xpointer
"xpointer( id( id('toc4')/text() ) )\r\n     xpointer( id( 'toc4' )/following-sibling::*[self::* = id('toc7')] )\r\n     xpointer( false() )"
)

; sxml:xpath
(xtest-assert ; Expected result:
(list vcntxt:doc)
; <--- of:
(draft:xpath "/")
vcntxt:doc
)


; sxml:xpath+root+vars
(xtest-assert ; Expected result:
(list vcntxt:doc)
;"dgdgf"
; <--- of:
(draft:xpointer "xpointer(/)")
vcntxt:doc
'()
)


;=========================================================================
; SXPath native syntax with context support

(define vcntxt:tree1 
  '(html
    (head (title "Slides"))
    (body
     (p (@ (align "center"))
	(table (@ (style "font-size: x-large"))
	       (tr
		(td (@ (align "right")) "Talks ")
		(td (@ (align "center")) " = ")
		(td " slides + transition"))
	       (tr (td)
		   (td (@ (align "center")) " = ")
		   (td " data + control"))
	       (tr (td)
		   (td (@ (align "center")) " = ")
		   (td " programs"))))
     (ul
      (li (a (@ (href "slides/slide0001.gif")) "Introduction"))
      (li (a (@ (href "slides/slide0010.gif")) "Summary")))
     )))


; Example from a posting "Re: DrScheme and XML", 
; Shriram Krishnamurthi, comp.lang.scheme, Nov. 26. 1999.
; http://www.deja.com/getdoc.xp?AN=553507805
(define vcntxt:tree3
  '(poem (@ (title "The Lovesong of J. Alfred Prufrock")
	    (poet "T. S. Eliot"))
	 (stanza
	  (line "Let us go then, you and I,")
	  (line "When the evening is spread out against the sky")
	  (line "Like a patient etherized upon a table:"))
	 (stanza
	  (line "In the room the women come and go")
	  (line "Talking of Michaelangelo."))))
; Top-level cond-expand expanded automatically
(define cntxt:error error)

(define (test-draft-sxpath path)
  (let ((func (draft:sxpath path)))
    (if
     (not func)  ; parse error
     (lambda (node) '@error@)  ; this can never be an expected result
     (lambda (node) (func node)))))


; Location path, full form: child::para 
; Location path, abbreviated form: para
; selects the para element children of the context node

(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para (@) "second par"))
       )
      (expected '((para (@) "para") (para (@) "second par")))
      )
  (vcntxt:run-test (select-kids (ntype?? 'para)) tree expected)
  (vcntxt:run-test (test-draft-sxpath '(para)) tree expected)
)

; Location path, full form: child::* 
; Location path, abbreviated form: *
; selects all element children of the context node

(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par"))
       )
      (expected
       '((para (@) "para") (br (@)) (para "second par")))
      )
  (vcntxt:run-test (select-kids (ntype?? '*)) tree expected)
  (vcntxt:run-test (test-draft-sxpath '(*)) tree expected)
)



; Location path, full form: child::text() 
; Location path, abbreviated form: text()
; selects all text node children of the context node
(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par"))
       )
      (expected
       '("cdata"))
      )
  (vcntxt:run-test (select-kids (ntype?? '*text*)) tree expected)
  (vcntxt:run-test (test-draft-sxpath '(*text*)) tree expected)
)


; Location path, full form: child::node() 
; Location path, abbreviated form: node()
; selects all the children of the context node, whatever their node type
(let* ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par"))
       )
      (expected (cdr tree))
      )
  (vcntxt:run-test (select-kids (ntype?? '*any*)) tree expected)
  (vcntxt:run-test (test-draft-sxpath '(*any*)) tree expected)
)

; Location path, full form: child::*/child::para 
; Location path, abbreviated form: */para
; selects all para grandchildren of the context node

(let ((tree
       '(elem (@) (para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para "third para")))
       )
      (expected
       '((para "third para")))
      )
  (vcntxt:run-test
   (node-join (select-kids (ntype?? '*))
	      (select-kids (ntype?? 'para)))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '(* para)) tree expected)
)


; Location path, full form: attribute::name 
; Location path, abbreviated form: @name
; selects the 'name' attribute of the context node

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para (@) "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((name "elem")))
      )
  (vcntxt:run-test
   (node-join (select-kids (ntype?? '@))
	      (select-kids (ntype?? 'name)))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '(@ name)) tree expected)
)

; Location path, full form:  attribute::* 
; Location path, abbreviated form: @*
; selects all the attributes of the context node
(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((name "elem") (id "idz")))
      )
  (vcntxt:run-test
   (node-join (select-kids (ntype?? '@))
	      (select-kids (ntype?? '*)))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '(@ *)) tree expected)
)


; Location path, full form: descendant::para 
; Location path, abbreviated form: .//para
; selects the para element descendants of the context node

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((para (@) "para") (para "second par") (para (@) "third para")))
      )
  (vcntxt:run-test
   (node-closure (ntype?? 'para))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '(// para)) tree expected)
)

; Location path, full form: self::para 
; Location path, abbreviated form: _none_
; selects the context node if it is a para element; otherwise selects nothing

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      )
  (vcntxt:run-test (node-self (ntype?? 'para)) tree '())
  (vcntxt:run-test (node-self (ntype?? 'elem)) tree (list tree))
)

; Location path, full form: descendant-or-self::node()
; Location path, abbreviated form: //
; selects the context node, all the children (including attribute nodes)
; of the context node, and all the children of all the (element)
; descendants of the context node.
; This is _almost_ a powerset of the context node.
(let* ((tree
       '(para (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       (cons tree
	(append (cdr tree)
       '((@) "para" (@) "second par"
	 (@ (name "aa")) (para (@) "third para")
	 (@) "third para"))))
      )
  (vcntxt:run-test
   (node-or
    (node-self (ntype?? '*any*))
    (node-closure (ntype?? '*any*)))
   tree expected)
; DL: In draft:xpath the order is different, because descendant axis is used
;  (vcntxt:run-test (test-draft-sxpath '(//)) tree expected)
)

; Location path, full form: ancestor::div 
; Location path, abbreviated form: _none_
; selects all div ancestors of the context node
; This Location expression is equivalent to the following:
;	/descendant-or-self::div[descendant::node() = curr_node]
; This shows that the ancestor:: axis is actually redundant. Still,
; it can be emulated as the following test-sxpath expression demonstrates.

; The insight behind "ancestor::div" -- selecting all "div" ancestors
; of the current node -- is
;  S[ancestor::div] context_node =
;    { y | y=subnode*(root), context_node=subnode(subnode*(y)),
;          isElement(y), name(y) = "div" }
; We observe that
;    { y | y=subnode*(root), pred(y) }
; can be expressed in test-sxpath as 
;    ((node-or (node-self pred) (node-closure pred)) root-node)
; The composite predicate 'isElement(y) & name(y) = "div"' corresponds to 
; (node-self (ntype?? 'div)) in test-sxpath. Finally, filter
; context_node=subnode(subnode*(y)) is tantamount to
; (node-closure (node-eq? context-node)), whereas node-reduce denotes the
; the composition of converters-predicates in the filtering context.

(let*
    ((root
	 '(div (@ (name "elem") (id "idz")) 
		(para (@) "para") (br (@)) "cdata" (para (@) "second par")
		(div (@ (name "aa")) (para (@) "third para"))))
     (context-node	; /descendant::any()[child::text() == "third para"]
      (car
       ((node-closure 
	 (select-kids
	  (node-equal? "third para")))
       root)))
    (pred
     (node-reduce (node-self (ntype?? 'div))
		  (node-closure (node-eq? context-node))
		  ))
     )
  (vcntxt:run-test
   (node-or
     (node-self pred)
     (node-closure pred))
   root 
   (cons root
	 '((div (@ (name "aa")) (para (@) "third para")))))
)



; Location path, full form: child::div/descendant::para 
; Location path, abbreviated form: div//para
; selects the para element descendants of the div element
; children of the context node

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")
	     (div (para "fourth para"))))
       )
      (expected
       '((para (@) "third para") (para "fourth para")))
      )
  (vcntxt:run-test
   (node-join 
    (select-kids (ntype?? 'div))
    (node-closure (ntype?? 'para)))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '(div // para)) tree expected)
)


; Location path, full form: /descendant::olist/child::item 
; Location path, abbreviated form: //olist/item
; selects all the item elements that have an olist parent (which is not root)
; and that are in the same document as the context node
; See the following test.

; Location path, full form: /descendant::td/attribute::align 
; Location path, abbreviated form: //td/@align
; Selects 'align' attributes of all 'td' elements in vcntxt:tree1
(let ((tree vcntxt:tree1)
      (expected
       '((align "right") (align "center") (align "center") (align "center"))
      ))
  (vcntxt:run-test
   (node-join 
    (node-closure (ntype?? 'td))
    (select-kids (ntype?? '@))
    (select-kids (ntype?? 'align)))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '(// td @ align)) tree expected)
)


; Location path, full form: /descendant::td[attribute::align] 
; Location path, abbreviated form: //td[@align]
; Selects all td elements that have an attribute 'align' in vcntxt:tree1
(let ((tree vcntxt:tree1)
      (expected
       '((td (@ (align "right")) "Talks ") (td (@ (align "center")) " = ")
	 (td (@ (align "center")) " = ") (td (@ (align "center")) " = "))
       ))
  (vcntxt:run-test
   (node-reduce 
    (node-closure (ntype?? 'td))
    (sxml:filter
     (node-join
      (select-kids (ntype?? '@))
      (select-kids (ntype?? 'align)))))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath 
             `(// td
                  ,(lambda (node . var-binding)
                     ((node-self (test-draft-sxpath '(@ align))) node))))
             tree expected)
  (vcntxt:run-test (test-draft-sxpath '(// (td (@ align)))) tree expected)
  (vcntxt:run-test (test-draft-sxpath '(// ((td) (@ align)))) tree expected)
  ; note! (test-draft-sxpath ...) is a converter. Therefore, it can be used
  ; as any other converter, for example, in the full-form test-sxpath.
  ; Thus we can mix the full and abbreviated form test-sxpath's freely.
  (vcntxt:run-test
   (node-reduce 
    (node-closure (ntype?? 'td))
    (sxml:filter
     (test-draft-sxpath '(@ align))))
   tree expected)
)


; Location path, full form: /descendant::td[attribute::align = "right"] 
; Location path, abbreviated form: //td[@align = "right"]
; Selects all td elements that have an attribute align = "right" in vcntxt:tree1
(let ((tree vcntxt:tree1)
      (expected
       '((td (@ (align "right")) "Talks "))
       ))
  (vcntxt:run-test
   (node-reduce 
    (node-closure (ntype?? 'td))
    (sxml:filter
     (node-join
      (select-kids (ntype?? '@))
      (select-kids (node-equal? '(align "right"))))))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '(// (td (@ (equal? (align "right")))))) tree expected)
)

; Location path, full form: child::para[position()=1] 
; Location path, abbreviated form: para[1]
; selects the first para child of the context node
(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((para (@) "para"))
      ))
  (vcntxt:run-test
   (node-reduce
    (select-kids (ntype?? 'para))
    (node-pos 1))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '((para 1))) tree expected)
)

; Location path, full form: child::para[position()=last()] 
; Location path, abbreviated form: para[last()]
; selects the last para child of the context node
(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      (expected
       '((para "second par"))
      ))
  (vcntxt:run-test
   (node-reduce
    (select-kids (ntype?? 'para))
    (node-pos -1))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '((para -1))) tree expected)
)

; Illustrating the following Note of Sec 2.5 of XPath:
; "NOTE: The location path //para[1] does not mean the same as the
; location path /descendant::para[1]. The latter selects the first
; descendant para element; the former selects all descendant para
; elements that are the first para children of their parents."

(let ((tree
       '(elem (@ (name "elem") (id "idz")) 
	(para (@) "para") (br (@)) "cdata" (para "second par")
	(div (@ (name "aa")) (para (@) "third para")))
       )
      )
  (vcntxt:run-test
   (node-reduce	; /descendant::para[1] in test-sxpath
    (node-closure (ntype?? 'para))
    (node-pos 1))
   tree '((para (@) "para")))
  (vcntxt:run-test (test-draft-sxpath '(// (para 1))) tree
	    '((para (@) "para") (para (@) "third para")))
)

; Location path, full form: parent::node()
; Location path, abbreviated form: ..
; selects the parent of the context node. The context node may be
; an attribute node!
; For the last test:
; Location path, full form: parent::*/attribute::name
; Location path, abbreviated form: ../@name
; Selects the name attribute of the parent of the context node

(let* ((tree
	'(elem (@ (name "elem") (id "idz")) 
	       (para (@) "para") (br (@)) "cdata" (para "second par")
	       (div (@ (name "aa")) (para (@) "third para")))
	)
       (para1		; the first para node
	(car ((test-draft-sxpath '(para)) tree)))
       (para3		; the third para node
	(car ((test-draft-sxpath '(div para)) tree)))
       (div		; div node
	(car ((test-draft-sxpath '(// div)) tree)))
       )
  (vcntxt:run-test
   (node-parent tree)
   para1 (list tree))
  (vcntxt:run-test
   (node-parent tree)
   para3 (list div))
  (vcntxt:run-test		; checking the parent of an attribute node
   (node-parent tree)
   ((test-draft-sxpath '(@ name)) div) (list div))
  (vcntxt:run-test
   (node-join
    (node-parent tree)
    (select-kids (ntype?? '@))
    (select-kids (ntype?? 'name)))
   para3 '((name "aa")))
  (vcntxt:run-test
   (test-draft-sxpath `(,(lambda (node . var-binding)
                     ((node-parent tree) node))
                  @ name))
   para3 '((name "aa")))
)

; Location path, full form: following-sibling::chapter[position()=1]
; Location path, abbreviated form: none
; selects the next chapter sibling of the context node
; The path is equivalent to
;  let cnode = context-node
;    in
;	parent::* / child::chapter [take-after node_eq(self::*,cnode)] 
;		[position()=1]
(let* ((tree
       '(document
	 (preface "preface")
	 (chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (chapter (@ (id "four")) "Chap 4 text")
	 (epilogue "Epilogue text")
	 (appendix (@ (id "A")) "App A text")
	 (References "References"))
       )
       (a-node	; to be used as a context node
	(car ((test-draft-sxpath '(// (chapter (@ (equal? (id "two")))))) tree)))
       (expected
       '((chapter (@ (id "three")) "Chap 3 text")))
      )
  (vcntxt:run-test
   (node-reduce
    (node-join
     (node-parent tree)
     (select-kids (ntype?? 'chapter)))
    (take-after (node-eq? a-node))
    (node-pos 1)
    )
   a-node expected)
)

; preceding-sibling::chapter[position()=1]
; selects the previous chapter sibling of the context node
; The path is equivalent to
;  let cnode = context-node
;    in
;	parent::* / child::chapter [take-until node_eq(self::*,cnode)] 
;		[position()=-1]
(let* ((tree
       '(document
	 (preface "preface")
	 (chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (chapter (@ (id "four")) "Chap 4 text")
	 (epilogue "Epilogue text")
	 (appendix (@ (id "A")) "App A text")
	 (References "References"))
       )
       (a-node	; to be used as a context node
	(car ((test-draft-sxpath '(// (chapter (@ (equal? (id "three")))))) tree)))
       (expected
       '((chapter (@ (id "two")) "Chap 2 text")))
      )
  (vcntxt:run-test
   (node-reduce
    (node-join
     (node-parent tree)
     (select-kids (ntype?? 'chapter)))
    (take-until (node-eq? a-node))
    (node-pos -1)
    )
   a-node expected)
)


; /descendant::figure[position()=42]
; selects the forty-second figure element in the document
; See the next example, which is more general.

; Location path, full form:
;    child::table/child::tr[position()=2]/child::td[position()=3] 
; Location path, abbreviated form: table/tr[2]/td[3]
; selects the third td of the second tr of the table
(let ((tree ((node-closure (ntype?? 'p)) vcntxt:tree1))
      (expected
       '((td " data + control"))
       ))
  (vcntxt:run-test
   (node-join
    (select-kids (ntype?? 'table))
    (node-reduce (select-kids (ntype?? 'tr))
		 (node-pos 2))
    (node-reduce (select-kids (ntype?? 'td))
		 (node-pos 3)))
   tree expected)
  ((test-draft-sxpath '(table (tr 2) (td 3))) tree)
)


; Location path, full form:
;		child::para[attribute::type='warning'][position()=5] 
; Location path, abbreviated form: para[@type='warning'][5]
; selects the fifth para child of the context node that has a type
; attribute with value warning
(let ((tree
       '(chapter
	 (para "para1")
	 (para (@ (type "warning")) "para 2")
	 (para (@ (type "warning")) "para 3")
	 (para (@ (type "warning")) "para 4")
	 (para (@ (type "warning")) "para 5")
	 (para (@ (type "warning")) "para 6"))
       )
      (expected
       '((para (@ (type "warning")) "para 6"))
      ))
  (vcntxt:run-test
   (node-reduce
    (select-kids (ntype?? 'para))
    (sxml:filter
     (node-join
      (select-kids (ntype?? '@))
      (select-kids (node-equal? '(type "warning")))))
    (node-pos 5))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '( (((para (@ (equal? (type "warning"))))) 5 )  ))
	    tree expected)
  ((test-draft-sxpath '( (para (@ (equal? (type "warning"))) 5 )  ))
	    tree)
)


; Location path, full form:
;		child::para[position()=5][attribute::type='warning'] 
; Location path, abbreviated form: para[5][@type='warning']
; selects the fifth para child of the context node if that child has a 'type'
; attribute with value warning
(let ((tree
       '(chapter
	 (para "para1")
	 (para (@ (type "warning")) "para 2")
	 (para (@ (type "warning")) "para 3")
	 (para (@ (type "warning")) "para 4")
	 (para (@ (type "warning")) "para 5")
	 (para (@ (type "warning")) "para 6"))
       )
      (expected
       '((para (@ (type "warning")) "para 5"))
      ))
  (vcntxt:run-test
   (node-reduce
    (select-kids (ntype?? 'para))
    (node-pos 5)
    (sxml:filter
     (node-join
      (select-kids (ntype?? '@))
      (select-kids (node-equal? '(type "warning"))))))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '( (( (para 5))  (@ (equal? (type "warning"))))))
	    tree expected)
  (vcntxt:run-test (test-draft-sxpath '( (para 5 (@ (equal? (type "warning")))) ))
	    tree expected)
)

; Location path, full form:
;		child::*[self::chapter or self::appendix]
; Location path, semi-abbreviated form: *[self::chapter or self::appendix]
; selects the chapter and appendix children of the context node
(let ((tree
       '(document
	 (preface "preface")
	 (chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (epilogue "Epilogue text")
	 (appendix (@ (id "A")) "App A text")
	 (References "References"))
       )
      (expected
       '((chapter (@ (id "one")) "Chap 1 text")
	 (chapter (@ (id "two")) "Chap 2 text")
	 (chapter (@ (id "three")) "Chap 3 text")
	 (appendix (@ (id "A")) "App A text"))
      ))
  (vcntxt:run-test
   (node-join
    (select-kids (ntype?? '*))
    (sxml:filter
     (node-or
      (node-self (ntype?? 'chapter))
      (node-self (ntype?? 'appendix)))))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath `(* ,(lambda (node . var-binding)
                                ((node-or
                                  (node-self (ntype?? 'chapter))
				  (node-self (ntype?? 'appendix)))
                                 node))))
	    tree expected)
)


; Location path, full form: child::chapter[child::title='Introduction'] 
; Location path, abbreviated form: chapter[title = 'Introduction']
; selects the chapter children of the context node that have one or more
; title children with string-value equal to Introduction
; See a similar example: //td[@align = "right"] above.

; Location path, full form: child::chapter[child::title] 
; Location path, abbreviated form: chapter[title]
; selects the chapter children of the context node that have one or
; more title children
; See a similar example //td[@align] above.

(cerr "\nExample with tree3: extracting the first lines of every stanza\n")
(let ((tree vcntxt:tree3)
      (expected
       '("Let us go then, you and I," "In the room the women come and go")
      ))
  (vcntxt:run-test
   (node-join
    (node-closure (ntype?? 'stanza))
    (node-reduce 
     (select-kids (ntype?? 'line)) (node-pos 1))
    (select-kids (ntype?? '*text*)))
   tree expected)
  (vcntxt:run-test (test-draft-sxpath '(// stanza (line 1) *text*)) tree expected)
)

(cout nl "XPath-with-context tests passed successfully!" nl)
