
(include "../../sxml-tools/tests/xtest-harness.sch")

;; Validation tests for SXPath textual syntax: "txpath.scm" 
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;@ Document
(define vtxp:old-doc
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
; DL: local version of the function - prefixed with `vtxp:'
(define (vtxp:SXML->SXML+id document id-attrs)
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

(define vtxp:doc
  (vtxp:SXML->SXML+id
   vtxp:old-doc
   '((item id) (chapter id) (section ID) (appendix id))))

;@ Namespace binding
(define vtxp:ns-binding (list (cons 'xlink "http://www.w3.org/1999/xlink")))


;=========================================================================
; Test functions

(define (sxml:test-xpath+index xpath-string . ns-binding)
  (let ((lst (if (null? ns-binding)
                 (sxml:xpath+index xpath-string)
                 (sxml:xpath+index xpath-string (car ns-binding)))))
    (if (not lst)
        '@error@    ; this can never be an expected result
        ((car lst) vtxp:doc))))

(define (sxml:test-xpointer+index xpath-string . ns-binding)
  (let ((lst (if (null? ns-binding)
	       (sxml:xpointer+index xpath-string)
	       (sxml:xpointer+index xpath-string (car ns-binding)))))
    (if (not lst)
        '@error@    ; this can never be an expected result
        ((car lst) vtxp:doc))))
       

(define (sxml:test-xpath+root+vars xpath-string var-binding . ns-binding)
  (let ((lst (if (null? ns-binding)
                 (sxml:xpath+root+vars xpath-string)
                 (sxml:xpath+root+vars xpath-string (car ns-binding)))))
    (if (not lst)
        '@error@    ; this can never be an expected result
        (lst vtxp:doc (cons `(*root* . ,vtxp:doc) var-binding)))))

(define (sxml:test-xpath+root xpath-string . ns-binding)
  (let ((lst (if (null? ns-binding)
                 (sxml:xpath+root xpath-string)
                 (sxml:xpath+root xpath-string (car ns-binding)))))
    (if (not lst)
        '@error@    ; this can never be an expected result
        (lst vtxp:doc `((*root* . ,vtxp:doc))))))

(define (sxml:test-xpointer+root+vars xpath-string var-binding . ns-binding)
  (let ((lst (if (null? ns-binding)
                 (sxml:xpointer+root+vars xpath-string)
                 (sxml:xpointer+root+vars xpath-string (car ns-binding)))))
    (if (not lst)
        '@error@    ; this can never be an expected result
        (lst vtxp:doc (cons `(*root* . ,vtxp:doc) var-binding)))))


;=========================================================================
; Test assertions are performed here

 ; SXPath testing

; sxml:xpath
(xtest-assert ; Expected result:
'("Text node")
; <--- of:
(sxml:xpath ".")
'("Text node")
)

 ; These XPath expressions are equal:

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"child::*/child::*[2]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"*/*[2]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"/*/*[2]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"descendant-or-self::node()[attribute::id ='toc1']"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"//*[attribute::* ='toc1']"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"/descendant::node()[attribute::id][1]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"//*[ self::node() = id('toc1') ]"
)

 ; Node tests

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((*PI* xml "version='1.0' encoding=\"Shift_JIS\""))
; <--- of:
sxml:test-xpath+index
"descendant::processing-instruction()"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((*PI* xml "version='1.0' encoding=\"Shift_JIS\""))
; <--- of:
sxml:test-xpath+index
"descendant::processing-instruction( 'xml' )"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpath+index
"descendant::processing-instruction( 'smth else' )"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpath+index
"//*[ self::processing-instruction('smth else') ]"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"descendant-or-self::text()"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'("boo.com")
; <--- of:
sxml:test-xpath+index
"descendant-or-self::text()[ self::node() = 'boo.com' ]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'("chapter1" "chapter2" "chapter3" "chapter4" "chapter5" "chapter6" "chapter7")
; <--- of:
sxml:test-xpath+index
"*/*/text()"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "extended")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "arc")
 (http://www.w3.org/1999/xlink:type "arc"))
; <--- of:
sxml:test-xpath+index
"//attribute::xlink:type"
vtxp:ns-binding
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:label "hoge")
 (http://www.w3.org/1999/xlink:to "hoge")
 (http://www.w3.org/1999/xlink:from "hoge"))
; <--- of:
sxml:test-xpath+index
"//attribute::xlink:*[ self::* = 'hoge' ]"
vtxp:ns-binding
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpath+index
"//attribute::xlink:*"
(list (cons 'xlink "http://www.else.com"))
)


;------------------------------------------------
; Axises

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((id "chap3"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/@*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((id "toc3"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink")))
; <--- of:
sxml:test-xpath+index
"*[1]/*[9]/*[3]/."
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((id "toc3"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/."
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[9]/*[3]/.."
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/.."
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[9]/*[3]/following-sibling::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/following-sibling::*"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[9]/*[3]/preceding-sibling::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/preceding-sibling::*"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[9]/*[3]/following::*"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[4]/@id/following::node()"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[9]/*[3]/preceding::*"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[4]/@id/preceding::*"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[9]/*[3]/parent::*"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3"))
; <--- of:
sxml:test-xpath+index
"*[1]/*[4]/@id/parent::*"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[9]/*[3]/ancestor::*"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[4]/@id/ancestor::*"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[9]/*[3]/ancestor-or-self::*"
)

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[4]/@id/ancestor-or-self::*"
)


;------------------------------------------------
; position() and last() functions

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
sxml:test-xpath+index
"*/*[position()=2]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc4")) "chapter4"))
; <--- of:
sxml:test-xpath+index
"*/*[position()=last()-4]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc6")) "chapter6"))
; <--- of:
sxml:test-xpath+index
"*/*[position()>=4 and position()<last()-1]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
sxml:test-xpath+index
"*/*[9]/*[position()=last()-position()]"
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
sxml:test-xpath+index
"*/*[9]/*[position()=4]"
)

;DL, 05.02.04: new test cases recently added for position-based location paths

(xtest-assert ; Expected result:
'((p "hyperlink")
 (p "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (p "There are important keywords."))
; <--- of:
sxml:test-xpath+index
"doc/body/chapter[position()>=3 and position()<=5]/p[1]"
)

(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2")
 (item (author "Who2") (title "XML Pointing Language (XPointer)") (ref "boo.com")))
; <--- of:
sxml:test-xpath+index
"//item[2]"
)


;------------------------------------------------
; Predicates

; sxml:test-xpath+index
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
sxml:test-xpath+index
"*[1]/*[1]/*[attribute::xlink:*][attribute::* = 'boo']"
vtxp:ns-binding
)

; sxml:test-xpath+index
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink")))
; <--- of:
sxml:test-xpath+index
"descendant-or-self::node()[self::chapter]\r\n                               [self::* = id(' chap1 chap3 chap6 chap7   ')]\r\n                               [*[1] = 'What is XLink?']"
)


;=========================================================================
; SXPointer testing

;------------------------------------------------
; Arithmetic operations

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'20
; <--- of:
sxml:test-xpointer+index
"xpointer( -2 + 4* (6-1) + 6 div 3 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( 7>4 and not( false() ) and ('err'= 12 or true() ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( true() > false() )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( '  15 ' = 15 = true() )"
)


;------------------------------------------------
; Variable reference

; sxml:test-xpointer+root+vars
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+root+vars
"xpointer(   $x and true() )"
(list (cons 'x #t))
)

; sxml:test-xpointer+root+vars
(xtest-assert ; Expected result:
'41
; <--- of:
sxml:test-xpointer+root+vars
"xpointer(   $y - 4)"
(list (cons 'y 45) (cons 'z '()))
)

; sxml:test-xpointer+root+vars
(xtest-assert ; Expected result:
'"variable value"
; <--- of:
sxml:test-xpointer+root+vars
"xpointer(   $z )"
(list (cons 'z "variable value"))
)

; sxml:test-xpointer+root+vars
(xtest-assert ; Expected result:
'((item (@ (id "toc2")) "chapter2"))
; <--- of:
sxml:test-xpointer+root+vars
"xpointer(   $var[2] | id('toc2') )"
(list (cons 'var '()))
)


;------------------------------------------------
; Datatype convertion

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'-34
; <--- of:
sxml:test-xpointer+index
"xpointer( number(' -34 ') )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'-34.67
; <--- of:
sxml:test-xpointer+index
"xpointer( number(' -34.67 ') )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'1
; <--- of:
sxml:test-xpointer+index
"xpointer( number( true() ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'0
; <--- of:
sxml:test-xpointer+index
"xpointer( number(false()) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( boolean( -56 ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( boolean( 0 ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( boolean( 'ere' ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( boolean( '' ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'"0"
; <--- of:
sxml:test-xpointer+index
"xpointer( string( 0 ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'"-76"
; <--- of:
sxml:test-xpointer+index
"xpointer( string( -76 ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'"true"
; <--- of:
sxml:test-xpointer+index
"xpointer( string( true() ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'"false"
; <--- of:
sxml:test-xpointer+index
"xpointer( string( false() ) )"
)


;------------------------------------------------
; Core XPath function library

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'9
; <--- of:
sxml:test-xpointer+index
"xpointer( count( //item ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'0
; <--- of:
sxml:test-xpointer+index
"xpointer( count( id('no_such_element') ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'0
; <--- of:
sxml:test-xpointer+index
"xpointer( floor(.4) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'-3
; <--- of:
sxml:test-xpointer+index
"xpointer( floor(-2.5) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'1
; <--- of:
sxml:test-xpointer+index
"xpointer( ceiling(.4) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'-2
; <--- of:
sxml:test-xpointer+index
"xpointer( ceiling(-2.5) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'0
; <--- of:
sxml:test-xpointer+index
"xpointer( round(.4) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'5
; <--- of:
sxml:test-xpointer+index
"xpointer( round(4.51) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'-3
; <--- of:
sxml:test-xpointer+index
"xpointer( round(-2.51) )"
)


;------------------------------------------------
; Comparison operations (for simple datatypes)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( false() = 12 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( true() = 12 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( true() != 0 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( false() = '' )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( false() = 'smth' )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( true() != 'smth' )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( '12  ' = 12 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( '123  ' != 12 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( true() > 0 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( false() < 1 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( '  12' <= 1 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( '  12' >= '  -2 ' )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( 'ee' <= 1 )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( 'aaa' < 'bbb' )"
)


;------------------------------------------------
; Union operation

; sxml:test-xpointer+index
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
sxml:test-xpointer+index
"xpointer( *[1]/*[7] | *[1]/*[9]/*[2] )"
)

; sxml:test-xpointer+index
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
sxml:test-xpointer+index
"xpointer( id('toc2') | id('chap4') | id('here') )"
)


;------------------------------------------------
; Nodeset comparison

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( */*[9]/*[5]/following::node() = 'ConclusionThanks a lot.')"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( */*[9]/*[5]/following::node() = //appendix//item[1] )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( */*[9]/*[5]/following::node() = \r\n                    */*[9]/*[6]/preceding-sibling::node() )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:test-xpointer+index
"xpointer( (/descendant::text()[23] | /descendant::text()[24]) = \r\n\t\t( //*[self::node() = 'foo.com'] | *[1]/*[1] ) )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:test-xpointer+index
"xpointer( //xlink:label[self::* = 'hoge'] > id( 'chap6' ) )"
vtxp:ns-binding
)


;------------------------------------------------
; Child sequence

; sxml:test-xpointer+index
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
sxml:test-xpointer+index
"here"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com")))
; <--- of:
sxml:test-xpointer+index
"here/1/1"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpointer+index
"toc1/2"
)

; sxml:test-xpointer+index
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
sxml:test-xpointer+index
"/1/1/3"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com")))
; <--- of:
sxml:test-xpointer+index
"/1/9/8/1/2"
)


;------------------------------------------------
; xmlns usage

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "extended")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "arc")
 (http://www.w3.org/1999/xlink:type "arc"))
; <--- of:
sxml:test-xpointer+index
"xmlns( xl = http://www.w3.org/1999/xlink) \r\n     xpointer( //attribute::xl:type )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:label "boo")
 (http://www.w3.org/1999/xlink:from "boo")
 (http://www.w3.org/1999/xlink:to "boo"))
; <--- of:
sxml:test-xpointer+index
"xmlns( xl = http://www.w3.org/1999/xlink) \r\n     xpointer( descendant::node()/attribute::xl:*[self::node() = 'boo'] )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'()
; <--- of:
sxml:test-xpointer+index
"xmlns( smth = http://smth ) \r\n     xpointer( descendant::node()/attribute::smth:*[self::node() = 'boo'] )"
)


;------------------------------------------------
; Several XPointer parts

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((title "Abstract")
 (title "Introduction")
 (title "What is XLink?")
 (title "What is XPointer?")
 (title "Models for using XLink/XPointer ")
 (title "samples")
 (title "Conclusion"))
; <--- of:
sxml:test-xpointer+index
"xpointer( doc/multidirectional/loc[3] )\r\n     xpointer( doc/body/chapter/title )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:label "boo")
 (http://www.w3.org/1999/xlink:from "boo")
 (http://www.w3.org/1999/xlink:to "boo"))
; <--- of:
sxml:test-xpointer+index
"xpointer( doc/multidirectional/loc[3] )\r\n     xmlns( xlink=http://www.w3.org/1999/xlink)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:*[self::* = 'boo'] )\r\n     xpointer( doc/body/chapter/title )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:actuate "onRequest")
 (http://www.w3.org/1999/xlink:actuate "onRequest"))
; <--- of:
sxml:test-xpointer+index
"xmlns( xlink=http://first_try)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:actuate )\r\n     xmlns( xlink=http://second_try)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:actuate )\r\n     xmlns( xlink=http://www.w3.org/1999/xlink)\r\n     xpointer( doc/multidirectional/*/attribute::xlink:actuate )\r\n     xpointer( false() )"
)

; sxml:test-xpointer+index
(xtest-assert ; Expected result:
'((item (@ (id "toc7")) "chapter7"))
; <--- of:
sxml:test-xpointer+index
"xpointer( id( id('toc4')/text() ) )\r\n     xpointer( id( 'toc4' )/following-sibling::*[self::* = id('toc7')] )\r\n     xpointer( false() )"
)

; sxml:xpath
(xtest-assert ; Expected result:
(list vtxp:doc)
; <--- of:
(sxml:xpath "/")
vtxp:doc
)


; sxml:xpath+root+vars
(xtest-assert ; Expected result:
(list vtxp:doc)
;"dgdgf"
; <--- of:
(sxml:xpath+root+vars "/")
vtxp:doc
'()
)

(cout nl "TXPath tests passed successfully!" nl)
