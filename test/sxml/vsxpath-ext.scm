
(include "../../sxml-tools/tests/xtest-harness.sch")

;; Validation tests for "sxpath-ext.scm"
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;@ test-document
(define xt-doc
'(*TOP*
 (*PI* xml "version='1.0'")
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

 ; Some particular nodes from a document above

;@node '(chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
(define node1
  (car ((sxpath '(doc body (chapter 3))) xt-doc xt-doc)))

;@ node: '(title "XML Pointing Language (XPointer)")
(define node2
  (car ((sxpath '(doc body appendix bibliographic (item 2) title)) xt-doc xt-doc)))

;@ node: '(id "toc3")
(define node3
  (car ((sxpath '(doc (item 3) @ id)) xt-doc xt-doc)))

; sxml:string
(xtest-assert ; Expected result:
"some string"
; <--- of:
sxml:string
"some string"
)

; sxml:string
(xtest-assert ; Expected result:
"0"
; <--- of:
sxml:string
0
)

; sxml:string
(xtest-assert ; Expected result:
"-76"
; <--- of:
sxml:string
-76
)

; sxml:string
(xtest-assert ; Expected result:
"true"
; <--- of:
sxml:string
#t
)

; sxml:string
(xtest-assert ; Expected result:
"false"
; <--- of:
sxml:string
#f
)

; sxml:string
(xtest-assert ; Expected result:
"What is XLink?hyperlink"
; <--- of:
sxml:string
(list node1 node2)
)

; sxml:string
(xtest-assert ; Expected result:
""
; <--- of:
sxml:string
'()
)

; sxml:boolean
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:boolean
#t
)

; sxml:boolean
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:boolean
-56
)

; sxml:boolean
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:boolean
0
)

; sxml:boolean
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:boolean
"word"
)

; sxml:boolean
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:boolean
""
)

; sxml:boolean
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:boolean
(list node1 node2)
)

; sxml:boolean
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:boolean
'()
)

; sxml:number
(xtest-assert ; Expected result:
45
; <--- of:
sxml:number
45
)

; sxml:number
(xtest-assert ; Expected result:
-34
; <--- of:
sxml:number
"  -34  "
)

; sxml:number
(xtest-assert ; Expected result:
-34.654
; <--- of:
sxml:number
"  -34.654  "
)

; sxml:number
(xtest-assert ; Expected result:
1
; <--- of:
sxml:number
#t
)

; sxml:number
(xtest-assert ; Expected result:
0
; <--- of:
sxml:number
#f
)

;------------------------------------------------
; sxml:string-value

; sxml:string-value
(xtest-assert ; Expected result:
"erer"
; <--- of:
sxml:string-value
"erer"
)

; sxml:string-value
(xtest-assert ; Expected result:
'"What is XLink?hyperlink"
; <--- of:
sxml:string-value
node1
)

; sxml:string-value
(xtest-assert ; Expected result:
'"XML Pointing Language (XPointer)"
; <--- of:
sxml:string-value
node2
)

; sxml:string-value
(xtest-assert ; Expected result:
'"version='1.0'chapter1chapter2chapter3chapter4chapter5chapter6chapter7AbstractThis document describes about XLink Engine...IntroductionThis document is written in XML (eXtensible Markup Language) ver.1.0.What is XLink?hyperlinkWhat is XPointer?XPointer is the fragment identifier of documents having the mime-type hogehoge.Models for using XLink/XPointer There are important keywords.samplesConclusionThanks a lot.Who1XML Linking Language (XLink)foo.comWho2XML Pointing Language (XPointer)boo.com"
; <--- of:
sxml:string-value
xt-doc
)


;=========================================================================
; XPath object comparison

; sxml:equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:equal?
#f
12
)

; sxml:equal?
(xtest-assert ; Expected result:
'#t
; <--- of:
sxml:equal?
#t
12
)

; sxml:not-equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:not-equal?
#t
0
)

; sxml:equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:equal?
#f
""
)

; sxml:equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:equal?
#f
"something"
)

; sxml:not-equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:not-equal?
#t
"something"
)

; sxml:equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:equal?
"  12 "
12
)

; sxml:not-equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:not-equal?
"  123 "
12
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp >)
#t
0
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp <)
#f
1
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#f
; <--- of:
(sxml:relational-cmp <=)
" 12   "
1
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp >=)
" 12   "
" -2 "
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp <=)
"foo"
1
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#f
; <--- of:
(sxml:relational-cmp <)
"5 "
"  -53"
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#f
; <--- of:
(sxml:relational-cmp <)
"5 "
"eee"
)

;------------------------------------------------
 ;; Nodeset comparison:
 
;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp >)
"  100"
(list node1 node2)
)

; sxml:equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:equal?
(list node1 node2)
7
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#f
; <--- of:
(sxml:relational-cmp >=)
(list node1 node2)
2
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp <=)
(list node1 node2)
3
)

; sxml:equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:equal?
(list node1 node2)
#t
)

; sxml:equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:equal?
'()
#t
)

; sxml:equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:equal?
'()
#f
)

; sxml:equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:equal?
(list node1 node2)
(list node2 node3)
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp >=)
(list node1 node2)
(list node2 node3)
)

;@ sxml:relational-cmp
(xtest-assert ; Expected result:
#t
; <--- of:
(sxml:relational-cmp <=)
(list node1 node2)
(list node2 node3)
)

; sxml:not-equal?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:not-equal?
(list node1 node2)
(list node2 node3)
)

; sxml:equal?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:equal?
(list node1 node2 )
(list node3)
)

; sxml:equal?
(xtest-assert ; Expected result:
'#f
; <--- of:
sxml:equal?
(list xt-doc)
(list node3)
)

;=========================================================================
; Node tests

; sxml:node?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:node?
node1
)

; sxml:node?
(xtest-assert ; Expected result:
#f
; <--- of:
sxml:node?
'(@ (id "chap3"))
)

; sxml:node?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:node?
node2
)

; sxml:node?
(xtest-assert ; Expected result:
#t
; <--- of:
sxml:node?
"XML Pointing Language (XPointer)"
)

 ; XPath axises

;@ sxml:ancestor 
(xtest-assert ; Expected result:
(list
 '(body
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
 '(doc
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
 xt-doc)
; <--- of:
((sxml:ancestor (ntype?? '*)) xt-doc)
node1
)

;@ sxml:ancestor
(xtest-assert ; Expected result:
`((item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
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
 ,xt-doc)
; <--- of:
((sxml:ancestor (ntype?? '*)) xt-doc)
node2
)

;@ sxml:ancestor
(xtest-assert ; Expected result:
`((item (@ (id "toc3")) "chapter3")
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
 ,xt-doc)
; <--- of:
((sxml:ancestor (ntype?? '*)) xt-doc)
node3
)

;@ sxml:ancestor
(xtest-assert ; Expected result:
`((item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
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
 ,xt-doc
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
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
 ,xt-doc
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
 ,xt-doc)
; <--- of:
((sxml:ancestor (ntype?? '*)) xt-doc)
(list node2 node2 node3)
)

;@ sxml:ancestor-or-self 
(xtest-assert ; Expected result:
`((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
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
 ,xt-doc)
; <--- of:
((sxml:ancestor-or-self (ntype?? '*)) xt-doc)
node1
)

;@ sxml:ancestor-or-self
(xtest-assert ; Expected result:
`((title "XML Pointing Language (XPointer)")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
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
 ,xt-doc)
; <--- of:
((sxml:ancestor-or-self (ntype?? '*)) xt-doc)
node2
)

;@ sxml:ancestor-or-self
(xtest-assert ; Expected result:
`((id "toc3")
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
  ,xt-doc)
; <--- of:
((sxml:ancestor-or-self (ntype?? '*)) xt-doc)
node3
)

;@ sxml:ancestor-or-self 
(xtest-assert ; Expected result:
`((title "XML Pointing Language (XPointer)")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
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
 ,xt-doc
 (title "XML Pointing Language (XPointer)")
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (bibliographic
   (item
    (author "Who1")
    (title "XML Linking Language (XLink)")
    (ref "foo.com"))
   (item
    (author "Who2")
    (title "XML Pointing Language (XPointer)")
    (ref "boo.com")))
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
 ,xt-doc
 (id "toc3")
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
 ,xt-doc)
; <--- of:
((sxml:ancestor-or-self (ntype?? '*)) xt-doc)
(list node2 node2 node3)
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'((id "chap3"))
; <--- of:
(sxml:attribute (ntype?? '*))
node1
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'((id "chap3"))
; <--- of:
(sxml:attribute (ntype?? 'id))
node1
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'()
; <--- of:
(sxml:attribute (ntype?? '*))
node2
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'()
; <--- of:
(sxml:attribute (ntype?? 'absent))
'(empty-node)
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:title "Chap.1 toc")
 (http://www.w3.org/1999/xlink:role "booboo")
 (http://www.w3.org/1999/xlink:label "boo")
 (http://www.w3.org/1999/xlink:href "#toc1"))
; <--- of:
(sxml:attribute (ntype?? '*))
'(loc
 (@
  (http://www.w3.org/1999/xlink:type "locator")
  (http://www.w3.org/1999/xlink:title "Chap.1 toc")
  (http://www.w3.org/1999/xlink:role "booboo")
  (http://www.w3.org/1999/xlink:label "boo")
  (http://www.w3.org/1999/xlink:href "#toc1")))
)

;@ sxml:attribute 
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:title "Chap.1 toc")
 (http://www.w3.org/1999/xlink:role "booboo")
 (http://www.w3.org/1999/xlink:label "boo")
 (http://www.w3.org/1999/xlink:href "#toc1")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:title "Chap.1 cont")
 (http://www.w3.org/1999/xlink:role "text/xml")
 (http://www.w3.org/1999/xlink:label "hoge")
 (http://www.w3.org/1999/xlink:href "#chap1"))
; <--- of:
(sxml:attribute (ntype?? '*))
'((loc
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
   (http://www.w3.org/1999/xlink:href "#chap1"))))
)

;@ sxml:descendant
(xtest-assert ; Expected result:
'((title "What is XLink?") (p "hyperlink"))
; <--- of:
(sxml:descendant (ntype?? '*))
node1
)

;@ sxml:descendant
(xtest-assert ; Expected result:
'((title "What is XLink?") "What is XLink?" (p "hyperlink") "hyperlink")
; <--- of:
(sxml:descendant sxml:node?)
node1
)

;@ sxml:descendant
(xtest-assert ; Expected result:
'((doc
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
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
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
       (ref "boo.com")))))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (title "Abstract")
 (p "This document describes about XLink Engine...")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 (p "hyperlink")
 (chapter
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
(sxml:descendant (ntype?? '*))
xt-doc
)

;@ sxml:descendant
(xtest-assert ; Expected result:
'((title "What is XLink?")
 "What is XLink?"
 (p "hyperlink")
 "hyperlink"
 "XML Pointing Language (XPointer)")
; <--- of:
(sxml:descendant sxml:node?)
(list node1 node2)
)

;@ sxml:descendant
(xtest-assert ; Expected result:
'((title "Abstract")
 (p "This document describes about XLink Engine...")
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (title "What is XLink?")
 (p "hyperlink")
 (title "What is XPointer?")
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 (title "samples")
 (title "Conclusion")
 (p "Thanks a lot."))
; <--- of:
(sxml:descendant (ntype?? '*))
'((chapter
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
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot.")))
)


;@ sxml:descendant-or-self
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 (p "hyperlink"))
; <--- of:
(sxml:descendant-or-self (ntype?? '*))
node1
)

;@ sxml:descendant-or-self
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 "What is XLink?"
 (p "hyperlink")
 "hyperlink")
; <--- of:
(sxml:descendant-or-self sxml:node?)
node1
)

;@ sxml:descendant-or-self 
(xtest-assert ; Expected result:
`(,xt-doc
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
   (http://www.w3.org/1999/xlink:actuate "onRequest")))
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
       (ref "boo.com")))))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (title "Abstract")
 (p "This document describes about XLink Engine...")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 (p "hyperlink")
 (chapter
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
(sxml:descendant-or-self (ntype?? '*))
xt-doc
)


;@ sxml:descendant-or-self
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 "What is XLink?"
 (p "hyperlink")
 "hyperlink"
 (title "XML Pointing Language (XPointer)")
 "XML Pointing Language (XPointer)")
; <--- of:
(sxml:descendant-or-self sxml:node?)
(list node1 node2)
)

;@ sxml:descendant-or-self
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (title "Abstract")
 (p "This document describes about XLink Engine...")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 (p "hyperlink")
 (chapter
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
 (p "Thanks a lot."))
; <--- of:
(sxml:descendant-or-self (ntype?? '*))
'((chapter
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
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot.")))
)
 
;@ sxml:following 
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
((sxml:following (ntype?? '*)) xt-doc)
node1
)

;@ sxml:following sxml:node?
(xtest-assert ; Expected result:
'((chapter
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
((sxml:following sxml:node?) xt-doc)
node1
)

;@ sxml:following
(xtest-assert ; Expected result:
'((ref "boo.com"))
; <--- of:
((sxml:following (ntype?? '*)) xt-doc)
node2
)

;@ sxml:following
(xtest-assert ; Expected result:
'((ref "boo.com") "boo.com")
; <--- of:
((sxml:following sxml:node?) xt-doc)
node2
)

;@ sxml:following 
(xtest-assert ; Expected result:
'((item (@ (id "toc4")) "chapter4")
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
       (ref "boo.com")))))
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (title "Abstract")
 (p "This document describes about XLink Engine...")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (title "What is XLink?")
 (p "hyperlink")
 (chapter
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
((sxml:following (ntype?? '*)) xt-doc)
node3
)

;@ sxml:following
(xtest-assert ; Expected result:
'((chapter
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
 "boo.com"
 (ref "boo.com")
 "boo.com")
; <--- of:
((sxml:following sxml:node?) xt-doc)
(list node1 node2)
)

;@ sxml:following-sibling
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
((sxml:following-sibling (ntype?? '*)) xt-doc)
node1
)

;@ sxml:following-sibling
(xtest-assert ; Expected result:
'((ref "boo.com"))
; <--- of:
((sxml:following-sibling (ntype?? '*)) xt-doc)
node2
)

;@ sxml:following-sibling
(xtest-assert ; Expected result:
'()
; <--- of:
((sxml:following-sibling (ntype?? '*)) xt-doc)
node3
)

;@ sxml:following-sibling
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
      (ref "boo.com"))))
 (ref "boo.com"))
; <--- of:
((sxml:following-sibling (ntype?? '*)) xt-doc)
(list node1 node2)
)

;@ sxml:namespace
(xtest-assert ; Expected result:
'()
; <--- of:
(sxml:namespace (ntype?? '*))
node1
)

;@ sxml:namespace
(xtest-assert ; Expected result:
'((pref "http://www.pref.org") (npref "http://www.npref.org"))
; <--- of:
(sxml:namespace (ntype?? '*))
'(pref:tag (@) (@@ (*NAMESPACES* (pref "http://www.pref.org")
				 (npref "http://www.npref.org"))))
)

;@ sxml:namespace
(xtest-assert ; Expected result:
'((pref "http://www.pref.org"))
; <--- of:
(sxml:namespace (ntype?? 'pref))
'(pref:tag (@) (@@ (*NAMESPACES* (pref "http://www.pref.org")
				 (npref "http://www.npref.org"))))
)

;@ sxml:parent
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
((sxml:parent (ntype?? '*)) xt-doc)
node1
)

;@ sxml:parent
(xtest-assert ; Expected result:
'((item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com")))
; <--- of:
((sxml:parent (ntype?? '*)) xt-doc)
node2
)


;@ sxml:parent 
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3"))
; <--- of:
((sxml:parent (ntype?? '*)) xt-doc)
node3
)

;@ sxml:parent 
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
 (item
  (author "Who2")
  (title "XML Pointing Language (XPointer)")
  (ref "boo.com"))
 (item (@ (id "toc3")) "chapter3"))
; <--- of:
((sxml:parent (ntype?? '*)) xt-doc)
(list node1 node2 node3)
)

;@ sxml:preceding
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
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
   )
; <--- of:
((sxml:preceding (ntype?? '*)) xt-doc)
node1
)


;@ sxml:preceding
(xtest-assert ; Expected result:
'("This document is written in XML (eXtensible Markup Language) ver.1.0."
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 "Introduction"
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 "This document describes about XLink Engine..."
 (p "This document describes about XLink Engine...")
 "Abstract"
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 "chapter7"
 (item (@ (id "toc7")) "chapter7")
 "chapter6"
 (item (@ (id "toc6")) "chapter6")
 "chapter5"
 (item (@ (id "toc5")) "chapter5")
 "chapter4"
 (item (@ (id "toc4")) "chapter4")
 "chapter3"
 (item (@ (id "toc3")) "chapter3")
 "chapter2"
 (item (@ (id "toc2")) "chapter2")
 "chapter1"
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
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
 (*PI* xml "version='1.0'"))   
; <--- of:
((sxml:preceding sxml:node?) xt-doc)
node1
)

;@ sxml:preceding
(xtest-assert ; Expected result:
'((author "Who2")
 (ref "foo.com")
 (title "XML Linking Language (XLink)")
 (author "Who1")
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 (p "Thanks a lot.")
 (title "Conclusion")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (title "samples")
 (chapter (@ (id "chap6")) (title "samples"))
 (p "There are important keywords.")
 (title "Models for using XLink/XPointer ")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 (title "What is XPointer?")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 (p "hyperlink")
 (title "What is XLink?")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
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
((sxml:preceding (ntype?? '*)) xt-doc)
node2
)

;@ sxml:preceding
(xtest-assert ; Expected result:
'("Who2"
 (author "Who2")
 "foo.com"
 (ref "foo.com")
 "XML Linking Language (XLink)"
 (title "XML Linking Language (XLink)")
 "Who1"
 (author "Who1")
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 "Thanks a lot."
 (p "Thanks a lot.")
 "Conclusion"
 (title "Conclusion")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 "samples"
 (title "samples")
 (chapter (@ (id "chap6")) (title "samples"))
 "There are important keywords."
 (p "There are important keywords.")
 "Models for using XLink/XPointer "
 (title "Models for using XLink/XPointer ")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 "XPointer is the fragment identifier of documents having the mime-type hogehoge."
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 "What is XPointer?"
 (title "What is XPointer?")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 "hyperlink"
 (p "hyperlink")
 "What is XLink?"
 (title "What is XLink?")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 "This document is written in XML (eXtensible Markup Language) ver.1.0."
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 "Introduction"
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 "This document describes about XLink Engine..."
 (p "This document describes about XLink Engine...")
 "Abstract"
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 "chapter7"
 (item (@ (id "toc7")) "chapter7")
 "chapter6"
 (item (@ (id "toc6")) "chapter6")
 "chapter5"
 (item (@ (id "toc5")) "chapter5")
 "chapter4"
 (item (@ (id "toc4")) "chapter4")
 "chapter3"
 (item (@ (id "toc3")) "chapter3")
 "chapter2"
 (item (@ (id "toc2")) "chapter2")
 "chapter1"
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
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
 (*PI* xml "version='1.0'"))
; <--- of:
((sxml:preceding sxml:node?) xt-doc)
node2
)

;@ sxml:preceding
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
((sxml:preceding (ntype?? '*)) xt-doc)
node3
)

;@ sxml:preceding
(xtest-assert ; Expected result:
'("This document is written in XML (eXtensible Markup Language) ver.1.0."
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 "Introduction"
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 "This document describes about XLink Engine..."
 (p "This document describes about XLink Engine...")
 "Abstract"
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 "chapter7"
 (item (@ (id "toc7")) "chapter7")
 "chapter6"
 (item (@ (id "toc6")) "chapter6")
 "chapter5"
 (item (@ (id "toc5")) "chapter5")
 "chapter4"
 (item (@ (id "toc4")) "chapter4")
 "chapter3"
 (item (@ (id "toc3")) "chapter3")
 "chapter2"
 (item (@ (id "toc2")) "chapter2")
 "chapter1"
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
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
 (*PI* xml "version='1.0'")
 "Who2"
 (author "Who2")
 "foo.com"
 (ref "foo.com")
 "XML Linking Language (XLink)"
 (title "XML Linking Language (XLink)")
 "Who1"
 (author "Who1")
 (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
 "Thanks a lot."
 (p "Thanks a lot.")
 "Conclusion"
 (title "Conclusion")
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 "samples"
 (title "samples")
 (chapter (@ (id "chap6")) (title "samples"))
 "There are important keywords."
 (p "There are important keywords.")
 "Models for using XLink/XPointer "
 (title "Models for using XLink/XPointer ")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 "XPointer is the fragment identifier of documents having the mime-type hogehoge."
 (p
  "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
 "What is XPointer?"
 (title "What is XPointer?")
 (chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
 "hyperlink"
 (p "hyperlink")
 "What is XLink?"
 (title "What is XLink?")
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
 "This document is written in XML (eXtensible Markup Language) ver.1.0."
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
 "Introduction"
 (title "Introduction")
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 "This document describes about XLink Engine..."
 (p "This document describes about XLink Engine...")
 "Abstract"
 (title "Abstract")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 "chapter7"
 (item (@ (id "toc7")) "chapter7")
 "chapter6"
 (item (@ (id "toc6")) "chapter6")
 "chapter5"
 (item (@ (id "toc5")) "chapter5")
 "chapter4"
 (item (@ (id "toc4")) "chapter4")
 "chapter3"
 (item (@ (id "toc3")) "chapter3")
 "chapter2"
 (item (@ (id "toc2")) "chapter2")
 "chapter1"
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
     (http://www.w3.org/1999/xlink:actuate "onRequest"))))
 (*PI* xml "version='1.0'"))
; <--- of:
((sxml:preceding sxml:node?) xt-doc)
(list node1 node2)
)

;@ sxml:preceding-sibling
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
((sxml:preceding-sibling (ntype?? '*)) xt-doc)
node1
)

;@ sxml:preceding-sibling
(xtest-assert ; Expected result:
'((author "Who2"))
; <--- of:
((sxml:preceding-sibling (ntype?? '*)) xt-doc)
node2
)

;@ sxml:preceding-sibling
(xtest-assert ; Expected result:
'()
; <--- of:
((sxml:preceding-sibling (ntype?? '*)) xt-doc)
node3
)

;@ sxml:preceding-sibling
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
   (p "This document describes about XLink Engine..."))
 (author "Who2"))
; <--- of:
((sxml:preceding-sibling (ntype?? '*)) xt-doc)
(list node1 node2)
)

(cout nl "SXPath-ext tests passed successfully!" nl)
