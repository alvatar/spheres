
(include "../../sxml-tools/tests/xtest-harness.sch")

;; Validation tests for DDO SXPath: "ddo-axes.scm" + "ddo-xpath.scm"
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

; We define a document
(define vddo:old-doc
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
           (ref "boo.com"))))))))

; Function for attaching id-index to document
; DL: local version of the function - prefixed with `vddo:'
(define (vddo:SXML->SXML+id document id-attrs)
  (let ((aux-subtrees
          (let ((aux ((select-kids (ntype?? '@@)) document)))
            (if (null? aux)
              '()
              (let rpt ((res '()) (to-see (cdar aux)))
                (cond
                 ((null? to-see) (reverse res))
                 ((equal? (caar to-see) 'id-index) (rpt res (cdr to-see)))
                 (else (rpt (cons (car to-see) res) (cdr to-see)))))))))
    (let loop ((nodeset (list document)) (id-index '()))
      (if (null? nodeset)
        (let ((kids
               ((select-kids
                  (lambda (node)
                    (not (and (pair? node) (equal? (car node) '@@)))))
                document)))
          (cons
           '*TOP*
           (cons
            (cons '@@ (cons (cons 'id-index id-index) aux-subtrees))
            kids)))
        (let ((cur-node (car nodeset)))
          (cond
           ((not (pair? cur-node)) (loop (cdr nodeset) id-index))
           ((assoc (car cur-node) id-attrs)
            =>
            (lambda (lst)
              (let ((id-values
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
                   (map (lambda (x) (cons x cur-node)) id-values))))))
           (else
            (loop
             (append ((select-kids (ntype?? '*)) (car nodeset)) (cdr nodeset))
             id-index))))))))

; Document with id-index added
(define vddo:doc
  (vddo:SXML->SXML+id vddo:old-doc '((item id) (chapter id) (section ID) (appendix id))))

; Namespace binding
(define vddo:ns-binding (list (cons 'xlink "http://www.w3.org/1999/xlink")))

; (lambda (x) x)
(xtest-assert ; Expected result:
'(*TOP*
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
    ("chap3" chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
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
    ("chap7" chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
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
; <--- of:
(lambda (x) x)
vddo:doc
)


;=========================================================================
; Location Path testing

; sxml:xpath
(xtest-assert ; Expected result:
'("Text node")
; <--- of:
(ddo:txpath ".")
'("Text node")
)

;------------------------------------------------
; Equal results for all these calls

; (ddo:txpath child::*/child::*[2])
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
(ddo:txpath "child::*/child::*[2]")
vddo:doc
)

; (ddo:txpath */*[2])
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
(ddo:txpath "*/*[2]")
vddo:doc
)

; (ddo:txpath /*/*[2])
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
(ddo:txpath "/*/*[2]")
vddo:doc
)

; (ddo:txpath descendant-or-self::node()[attribute::id ='toc1'])
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
(ddo:txpath "descendant-or-self::node()[attribute::id ='toc1']")
vddo:doc
)

; (ddo:txpath //*[attribute::* ='toc1'])
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
(ddo:txpath "//*[attribute::* ='toc1']")
vddo:doc
)

; (ddo:txpath //node()[attribute::id][1])
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1")
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine...")))
; <--- of:
(ddo:txpath "//node()[attribute::id][1]")
vddo:doc
)

; (ddo:txpath //*[ self::node() = id('toc1') ])
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
(ddo:txpath "//*[ self::node() = id('toc1') ]")
vddo:doc
)


;------------------------------------------------
; Node tests

; (ddo:txpath descendant::processing-instruction())
(xtest-assert ; Expected result:
'((*PI* xml "version='1.0'"))
; <--- of:
(ddo:txpath "descendant::processing-instruction()")
vddo:doc
)

; (ddo:txpath descendant::processing-instruction( 'xml' ))
(xtest-assert ; Expected result:
'((*PI* xml "version='1.0'"))
; <--- of:
(ddo:txpath "descendant::processing-instruction( 'xml' )")
vddo:doc
)

; (ddo:txpath descendant::processing-instruction( 'smth else' ))
(xtest-assert ; Expected result:
'()
; <--- of:
(ddo:txpath "descendant::processing-instruction( 'smth else' )")
vddo:doc
)

; (ddo:txpath //*[ self::processing-instruction('smth else') ])
(xtest-assert ; Expected result:
'()
; <--- of:
(ddo:txpath "//*[ self::processing-instruction('smth else') ]")
vddo:doc
)

; (ddo:txpath descendant-or-self::text())
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
(ddo:txpath "descendant-or-self::text()")
vddo:doc
)

; (ddo:txpath descendant-or-self::text()[ self::node() = 'boo.com' ])
(xtest-assert ; Expected result:
'("boo.com")
; <--- of:
(ddo:txpath "descendant-or-self::text()[ self::node() = 'boo.com' ]")
vddo:doc
)

; (ddo:txpath */*/text())
(xtest-assert ; Expected result:
'("chapter1" "chapter2" "chapter3" "chapter4" "chapter5" "chapter6" "chapter7")
; <--- of:
(ddo:txpath "*/*/text()")
vddo:doc
)

; (ddo:txpath //attribute::xlink:type ns-binding)
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:type "extended")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "locator")
 (http://www.w3.org/1999/xlink:type "arc")
 (http://www.w3.org/1999/xlink:type "arc"))
; <--- of:
(ddo:txpath "//attribute::xlink:type" vddo:ns-binding)
vddo:doc
)

; (ddo:txpath //attribute::xlink:*[ self::* = 'hoge' ] ns-binding)
(xtest-assert ; Expected result:
'((http://www.w3.org/1999/xlink:label "hoge")
 (http://www.w3.org/1999/xlink:to "hoge")
 (http://www.w3.org/1999/xlink:from "hoge"))
; <--- of:
(ddo:txpath "//attribute::xlink:*[ self::* = 'hoge' ]" vddo:ns-binding)
vddo:doc
)

; (ddo:txpath //attribute::xlink:* (quote ((xlink . http://www.else.com))))
(xtest-assert ; Expected result:
'()
; <--- of:
(ddo:txpath "//attribute::xlink:*" '((xlink . "http://www.else.com")))
vddo:doc
)


;------------------------------------------------
; Axes

; (ddo:txpath *[1]/*[9]/*[3]/@*)
(xtest-assert ; Expected result:
'((id "chap3"))
; <--- of:
(ddo:txpath "*[1]/*[9]/*[3]/@*")
vddo:doc
)

; (ddo:txpath *[1]/*[4]/@id)
(xtest-assert ; Expected result:
'((id "toc3"))
; <--- of:
(ddo:txpath "*[1]/*[4]/@id")
vddo:doc
)

; (ddo:txpath *[1]/*[9]/*[3]/.)
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink")))
; <--- of:
(ddo:txpath "*[1]/*[9]/*[3]/.")
vddo:doc
)

; (ddo:txpath *[1]/*[4]/@id/.)
(xtest-assert ; Expected result:
'((id "toc3"))
; <--- of:
(ddo:txpath "*[1]/*[4]/@id/.")
vddo:doc
)

; (ddo:txpath *[1]/*[9]/*[3]/..)
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
(ddo:txpath "*[1]/*[9]/*[3]/..")
vddo:doc
)

; (ddo:txpath *[1]/*[4]/@id/..)
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3"))
; <--- of:
(ddo:txpath "*[1]/*[4]/@id/..")
vddo:doc
)

; (ddo:txpath *[1]/*[9]/*[3]/following-sibling::*)
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
(ddo:txpath "*[1]/*[9]/*[3]/following-sibling::*")
vddo:doc
)

; (ddo:txpath *[1]/*[4]/@id/following-sibling::*)
(xtest-assert ; Expected result:
'()
; <--- of:
(ddo:txpath "*[1]/*[4]/@id/following-sibling::*")
vddo:doc
)

; (ddo:txpath *[1]/*[9]/*[3]/preceding-sibling::*)
(xtest-assert ; Expected result:
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
      "This document is written in XML (eXtensible Markup Language) ver.1.0."))))
; <--- of:
(ddo:txpath "*[1]/*[9]/*[3]/preceding-sibling::*")
vddo:doc
)

; (ddo:txpath *[1]/*[4]/@id/preceding-sibling::*)
(xtest-assert ; Expected result:
'()
; <--- of:
(ddo:txpath "*[1]/*[4]/@id/preceding-sibling::*")
vddo:doc
)

; (ddo:txpath *[1]/*[9]/*[3]/following::*)
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
(ddo:txpath "*[1]/*[9]/*[3]/following::*")
vddo:doc
)

; (ddo:txpath *[1]/*[4]/@id/following::node())
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
(ddo:txpath "*[1]/*[4]/@id/following::node()")
vddo:doc
)

; (ddo:txpath *[1]/*[9]/*[3]/preceding::*)
(xtest-assert ; Expected result:
'((multidirectional
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
 (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
; <--- of:
(ddo:txpath "*[1]/*[9]/*[3]/preceding::*")
vddo:doc
)

; (ddo:txpath *[1]/*[4]/@id/preceding::*)
(xtest-assert ; Expected result:
'((multidirectional
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
 (item (@ (id "toc2")) "chapter2"))
; <--- of:
(ddo:txpath "*[1]/*[4]/@id/preceding::*")
vddo:doc
)

; (ddo:txpath *[1]/*[9]/*[3]/parent::*)
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
(ddo:txpath "*[1]/*[9]/*[3]/parent::*")
vddo:doc
)

; (ddo:txpath *[1]/*[4]/@id/parent::*)
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3"))
; <--- of:
(ddo:txpath "*[1]/*[4]/@id/parent::*")
vddo:doc
)

; (ddo:txpath *[1]/*[9]/*[3]/ancestor::*)
(xtest-assert ; Expected result:
`(,vddo:doc  ; document node
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
; <--- of:
(ddo:txpath "*[1]/*[9]/*[3]/ancestor::*")
vddo:doc
)

; (ddo:txpath *[1]/*[4]/@id/ancestor::*)
(xtest-assert ; Expected result:
`(,vddo:doc  ; document node
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
 (item (@ (id "toc3")) "chapter3"))
; <--- of:
(ddo:txpath "*[1]/*[4]/@id/ancestor::*")
vddo:doc
)

; (ddo:txpath *[1]/*[9]/*[3]/ancestor-or-self::*)
(xtest-assert ; Expected result:
`(,vddo:doc  ; document node
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
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink")))
; <--- of:
(ddo:txpath "*[1]/*[9]/*[3]/ancestor-or-self::*")
vddo:doc
)

; (ddo:txpath *[1]/*[4]/@id/ancestor-or-self::*)
(xtest-assert ; Expected result:
`(,vddo:doc  ; document node
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
 (item (@ (id "toc3")) "chapter3")
 (id "toc3"))
; <--- of:
(ddo:txpath "*[1]/*[4]/@id/ancestor-or-self::*")
vddo:doc
)


;------------------------------------------------
; position() and last() functions

; (ddo:txpath */*[position()=2])
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1"))
; <--- of:
(ddo:txpath "*/*[position()=2]")
vddo:doc
)

; (ddo:txpath */*[position()=last()-4])
(xtest-assert ; Expected result:
'((item (@ (id "toc4")) "chapter4"))
; <--- of:
(ddo:txpath "*/*[position()=last()-4]")
vddo:doc
)

; (ddo:txpath */*[position()>=4 and position()<last()-1])
(xtest-assert ; Expected result:
'((item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc6")) "chapter6"))
; <--- of:
(ddo:txpath "*/*[position()>=4 and position()<last()-1]")
vddo:doc
)

; (ddo:txpath */*[9]/*[position()=last()-position()])
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
(ddo:txpath "*/*[9]/*[position()=last()-position()]")
vddo:doc
)

; (ddo:txpath */*[9]/*[position()=4])
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
(ddo:txpath "*/*[9]/*[position()=4]")
vddo:doc
)


;------------------------------------------------
; Predicates

; (ddo:txpath *[1]/*[1]/*[attribute::xlink:*][attribute::* = 'boo'] ns-binding)
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
(ddo:txpath "*[1]/*[1]/*[attribute::xlink:*][attribute::* = 'boo']" vddo:ns-binding)
vddo:doc
)

; (ddo:txpath descendant-or-self::node()[self::chapter][self::* = id(' chap1 chap3 chap6 chap7   ')][*[1] = 'What is XLink?'])
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink")))
; <--- of:
(ddo:txpath
  "descendant-or-self::node()[self::chapter][self::* = id(' chap1 chap3 chap6 chap7   ')][*[1] = 'What is XLink?']")
vddo:doc
)


;=========================================================================
; Testing distinct document order

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3])
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
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (id "chap2")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (id "chap5"))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]")
vddo:doc
)


;------------------------------------------------
; Axes without position-based predicates

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/ancestor::node())
(xtest-assert ; Expected result:
`(,vddo:doc  ; document node
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
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords.")))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/ancestor::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/ancestor-or-self::node())
(xtest-assert ; Expected result:
`(,vddo:doc  ; document node
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
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (id "chap2")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (id "chap5"))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/ancestor-or-self::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/attribute::node())
(xtest-assert ; Expected result:
'((id "chap2") (id "chap5"))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/attribute::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/child::node())
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 "chap2"
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
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
 "chap5"
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 (chapter (@ (id "chap6")) (title "samples"))
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
 (appendix
   (@ (id "here"))
   (bibliographic
     (item (author "Who1") (title "XML Linking Language (XLink)") (ref "foo.com"))
     (item
      (author "Who2")
      (title "XML Pointing Language (XPointer)")
      (ref "boo.com")))))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/child::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/descendant::node())
(xtest-assert ; Expected result:
'((chapter
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
 "boo.com"
 "chap2"
 (title "Models for using XLink/XPointer ")
 "Models for using XLink/XPointer "
 (p "There are important keywords.")
 "There are important keywords."
 "chap5")
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/descendant::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/descendant-or-self::node())
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
 "boo.com"
 (id "chap2")
 "chap2"
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (title "Models for using XLink/XPointer ")
 "Models for using XLink/XPointer "
 (p "There are important keywords.")
 "There are important keywords."
 (id "chap5")
 "chap5")
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/descendant-or-self::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/following::node())
(xtest-assert ; Expected result:
'((title "Introduction")
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
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/following::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/following-sibling::node())
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
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
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/following-sibling::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/namespace::node())
(xtest-assert ; Expected result:
'()
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/namespace::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/parent::node())
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
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords.")))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/parent::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/preceding::node())
(xtest-assert ; Expected result:
'((*PI* xml "version='1.0'")
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
 "chapter1"
 (item (@ (id "toc2")) "chapter2")
 "chapter2"
 (item (@ (id "toc3")) "chapter3")
 "chapter3"
 (item (@ (id "toc4")) "chapter4")
 "chapter4"
 (item (@ (id "toc5")) "chapter5")
 "chapter5"
 (item (@ (id "toc6")) "chapter6")
 "chapter6"
 (item (@ (id "toc7")) "chapter7")
 "chapter7"
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
 "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/preceding::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/preceding-sibling::node())
(xtest-assert ; Expected result:
'((multidirectional
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
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
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
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/preceding-sibling::node()")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/self::node())
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
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (id "chap2")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (id "chap5"))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/self::node()")
vddo:doc
)


;------------------------------------------------
; Axes with position-based predicate

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/ancestor::node()[position()])
(xtest-assert ; Expected result:
`(,vddo:doc  ; document node
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
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords.")))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/ancestor::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/ancestor-or-self::node()[position()])
(xtest-assert ; Expected result:
`(,vddo:doc  ; document node
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
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (id "chap2")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (id "chap5"))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/ancestor-or-self::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/attribute::node()[position()])
(xtest-assert ; Expected result:
'((id "chap2") (id "chap5"))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/attribute::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/child::node()[position()])
(xtest-assert ; Expected result:
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
 (title "Introduction")
 (section
   (@ (ID "sec2-1"))
   (p "This document is written in XML (eXtensible Markup Language) ver.1.0."))
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
      (ref "boo.com"))))
 "chap2"
 (title "Models for using XLink/XPointer ")
 (p "There are important keywords.")
 "chap5")
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/child::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/descendant::node()[position()])
(xtest-assert ; Expected result:
'((chapter
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
 "boo.com"
 "chap2"
 "chap5")
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/descendant::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/descendant-or-self::node()[position()])
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
 "boo.com"
 (id "chap2")
 "chap2"
 (id "chap5")
 "chap5")
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/descendant-or-self::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/following::node()[position()])
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
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
 "boo.com"
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
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/following::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/following-sibling::node()[position()])
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
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
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/following-sibling::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/namespace::node()[position()])
(xtest-assert ; Expected result:
'()
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/namespace::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/parent::node()[position()])
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
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords.")))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/parent::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/preceding::node()[position()])
(xtest-assert ; Expected result:
'((*PI* xml "version='1.0'")
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
 "chapter1"
 (item (@ (id "toc2")) "chapter2")
 "chapter2"
 (item (@ (id "toc3")) "chapter3")
 "chapter3"
 (item (@ (id "toc4")) "chapter4")
 "chapter4"
 (item (@ (id "toc5")) "chapter5")
 "chapter5"
 (item (@ (id "toc6")) "chapter6")
 "chapter6"
 (item (@ (id "toc7")) "chapter7")
 "chapter7"
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
 "XPointer is the fragment identifier of documents having the mime-type hogehoge.")
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/preceding::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/preceding-sibling::node()[position()])
(xtest-assert ; Expected result:
'((multidirectional
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
 (chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
 (item (@ (id "toc7")) "chapter7")
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
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/preceding-sibling::node()[position()]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/self::node()[position()])
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
 (chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0.")))
 (id "chap2")
 (chapter
   (@ (id "chap5"))
   (title "Models for using XLink/XPointer ")
   (p "There are important keywords."))
 (id "chap5"))
; <--- of:
(ddo:txpath
  "doc/body/chapter[position()=2 or position()=5]/@id/ancestor-or-self::node()[position()<=3]/self::node()[position()]")
vddo:doc
)


;=========================================================================
; XPath Expr


;------------------------------------------------
; Arithmetic operations

; (ddo:xpath-expr  -2 + 4* (6-1) + 6 div 3 )
(xtest-assert ; Expected result:
'20
; <--- of:
(ddo:xpath-expr " -2 + 4* (6-1) + 6 div 3 ")
'()
)

; (ddo:xpath-expr  7>4 and not( false() ) and ('err'= 12 or true() ) )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " 7>4 and not( false() ) and ('err'= 12 or true() ) ")
'()
)

; (ddo:xpath-expr  true() > false() )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " true() > false() ")
'()
)

; (ddo:xpath-expr  '  15 ' = 15 = true())
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " '  15 ' = 15 = true()")
'()
)


;------------------------------------------------
; Variable reference

; (ddo:xpath-expr    $xlink and true() )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "   $xlink and true() ")
'()
'((xlink . #t))
)

; (ddo:xpath-expr    $x - 4)
(xtest-assert ; Expected result:
'41
; <--- of:
(ddo:xpath-expr "   $x - 4")
'()
'((x . 45))
)

; (ddo:xpath-expr    $y )
(xtest-assert ; Expected result:
'"variable value"
; <--- of:
(ddo:xpath-expr "   $y ")
'()
'((y . "variable value"))
)


;------------------------------------------------
; Datatype convertion

; (ddo:xpath-expr  number(' -34 ') )
(xtest-assert ; Expected result:
'-34
; <--- of:
(ddo:xpath-expr " number(' -34 ') ")
'()
)

; (ddo:xpath-expr  number(' -34.67 ') )
(xtest-assert ; Expected result:
'-34.67
; <--- of:
(ddo:xpath-expr " number(' -34.67 ') ")
'()
)

; (ddo:xpath-expr  number( true() ) )
(xtest-assert ; Expected result:
'1
; <--- of:
(ddo:xpath-expr " number( true() ) ")
'()
)

; (ddo:xpath-expr  number(false()) )
(xtest-assert ; Expected result:
'0
; <--- of:
(ddo:xpath-expr " number(false()) ")
'()
)

; (ddo:xpath-expr  boolean( -56 ) )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " boolean( -56 ) ")
'()
)

; (ddo:xpath-expr  boolean( 0 ) )
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr " boolean( 0 ) ")
'()
)

; (ddo:xpath-expr  boolean( 'ere' ) )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " boolean( 'ere' ) ")
'()
)

; (ddo:xpath-expr  boolean( '' ) )
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr " boolean( '' ) ")
'()
)

; (ddo:xpath-expr  string( 0 ) )
(xtest-assert ; Expected result:
'"0"
; <--- of:
(ddo:xpath-expr " string( 0 ) ")
'()
)

; (ddo:xpath-expr  string( -76 ) )
(xtest-assert ; Expected result:
'"-76"
; <--- of:
(ddo:xpath-expr " string( -76 ) ")
'()
)

; (ddo:xpath-expr  string( true() ) )
(xtest-assert ; Expected result:
'"true"
; <--- of:
(ddo:xpath-expr " string( true() ) ")
'()
)

; (ddo:xpath-expr  string( false() ) )
(xtest-assert ; Expected result:
'"false"
; <--- of:
(ddo:xpath-expr " string( false() ) ")
'()
)


;------------------------------------------------
; Comparison operations (for simple datatypes)

; (ddo:xpath-expr  false() = 12 )
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr " false() = 12 ")
'()
)

; (ddo:xpath-expr  true() = 12 )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " true() = 12 ")
'()
)

; (ddo:xpath-expr true() != 0)
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "true() != 0")
'()
)

; (ddo:xpath-expr  false() = '' )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " false() = '' ")
'()
)

; (ddo:xpath-expr  false() = 'smth' )
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr " false() = 'smth' ")
'()
)

; (ddo:xpath-expr  true() != 'smth' )
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr " true() != 'smth' ")
'()
)

; (ddo:xpath-expr  '12  ' = 12 )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " '12  ' = 12 ")
'()
)

; (ddo:xpath-expr  '123  ' != 12 )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " '123  ' != 12 ")
'()
)

; (ddo:xpath-expr  true() > 0 )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " true() > 0 ")
'()
)

; (ddo:xpath-expr ( false() < 1 ))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "( false() < 1 )")
'()
)

; (ddo:xpath-expr  '  12' <= 1 )
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr " '  12' <= 1 ")
'()
)

; (ddo:xpath-expr ( '  12' >= '  -2 ' ))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "( '  12' >= '  -2 ' )")
'()
)

; (ddo:xpath-expr  'ee' <= 1 )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " 'ee' <= 1 ")
'()
)

; (ddo:xpath-expr ( 'aaa' < 'bbb' ))
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr "( 'aaa' < 'bbb' )")
'()
)


;------------------------------------------------
; Union operation

; (ddo:xpath-expr *[1]/*[7] | *[1]/*[9]/*[2] )
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
(ddo:xpath-expr "*[1]/*[7] | *[1]/*[9]/*[2] ")
vddo:doc
)

; (ddo:xpath-expr  id('toc2') | id('chap4') | id('here') )
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
(ddo:xpath-expr " id('toc2') | id('chap4') | id('here') ")
vddo:doc
)


;------------------------------------------------
; Nodeset comparison

; (ddo:xpath-expr  */*[9]/*[5]/following::node() = 'ConclusionThanks a lot.')
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " */*[9]/*[5]/following::node() = 'ConclusionThanks a lot.'")
vddo:doc
)

; (ddo:xpath-expr  */*[9]/*[5]/following::node() = //appendix//item[1] )
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr " */*[9]/*[5]/following::node() = //appendix//item[1] ")
vddo:doc
)

; (ddo:xpath-expr  */*[9]/*[5]/following::node() = */*[9]/*[6]/preceding-sibling::node() )
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr
  " */*[9]/*[5]/following::node() = */*[9]/*[6]/preceding-sibling::node() ")
vddo:doc
)

; (ddo:xpath-expr  (//text()[23] | //text()[24]) = ( //*[self::node() = 'foo.com'] | *[1]/*[1] ) )
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr
  " (//text()[23] | //text()[24]) = ( //*[self::node() = 'foo.com'] | *[1]/*[1] ) ")
vddo:doc
)

; (ddo:xpath-expr  //xlink:label[self::* = 'hoge'] > id( 'chap6' )  ns-binding)
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr " //xlink:label[self::* = 'hoge'] > id( 'chap6' ) " vddo:ns-binding)
vddo:doc
)


;------------------------------------------------
; Filter expression with position predicate

; (ddo:xpath-expr (doc/body/*)[position()])
(xtest-assert ; Expected result:
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
(ddo:xpath-expr "(doc/body/*)[position()]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[position()=4])
(xtest-assert ; Expected result:
'(chapter
  (@ (id "chap4"))
  (title "What is XPointer?")
  (p
   "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
; <--- of:
(ddo:xpath-expr "(doc/body/*)[position()=4]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[4])
(xtest-assert ; Expected result:
'(chapter
  (@ (id "chap4"))
  (title "What is XPointer?")
  (p
   "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
; <--- of:
(ddo:xpath-expr "(doc/body/*)[4]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[4=position()])
(xtest-assert ; Expected result:
'(chapter
  (@ (id "chap4"))
  (title "What is XPointer?")
  (p
   "XPointer is the fragment identifier of documents having the mime-type hogehoge."))
; <--- of:
(ddo:xpath-expr "(doc/body/*)[4=position()]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[last()])
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
(ddo:xpath-expr "(doc/body/*)[last()]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[position()<=6])
(xtest-assert ; Expected result:
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
 (chapter (@ (id "chap6")) (title "samples")))
; <--- of:
(ddo:xpath-expr "(doc/body/*)[position()<=6]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[position()<6])
(xtest-assert ; Expected result:
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
   (p "There are important keywords.")))
; <--- of:
(ddo:xpath-expr "(doc/body/*)[position()<6]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[position()>=6])
(xtest-assert ; Expected result:
'((chapter (@ (id "chap6")) (title "samples"))
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
(ddo:xpath-expr "(doc/body/*)[position()>=6]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[position()>6])
(xtest-assert ; Expected result:
'((chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot."))
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
(ddo:xpath-expr "(doc/body/*)[position()>6]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[3>position()])
(xtest-assert ; Expected result:
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
      "This document is written in XML (eXtensible Markup Language) ver.1.0."))))
; <--- of:
(ddo:xpath-expr "(doc/body/*)[3>position()]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[3>=position()])
(xtest-assert ; Expected result:
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
 (chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink")))
; <--- of:
(ddo:xpath-expr "(doc/body/*)[3>=position()]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[3<position()])
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
(ddo:xpath-expr "(doc/body/*)[3<position()]")
vddo:doc
)

; (ddo:xpath-expr (doc/body/*)[3<=position()])
(xtest-assert ; Expected result:
'((chapter (@ (id "chap3")) (title "What is XLink?") (p "hyperlink"))
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
; <--- of:
(ddo:xpath-expr "(doc/body/*)[3<=position()]")
vddo:doc
)


;------------------------------------------------
; Deeply nested predicates

; (ddo:txpath doc/body/chapter[@id[../title[../p[count(ancestor::*)=4]]]])
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine..."))
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
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot.")))
; <--- of:
(ddo:txpath "doc/body/chapter[@id[../title[../p[count(ancestor::*)=4]]]]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[title[count(following-sibling::*[self::section or self::p[text()[1]]])=1]])
(xtest-assert ; Expected result:
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
 (chapter (@ (id "chap7")) (title "Conclusion") (p "Thanks a lot.")))
; <--- of:
(ddo:txpath
  "doc/body/chapter[title[count(following-sibling::*[self::section or self::p[text()[1]]])=1]]")
vddo:doc
)

; (ddo:txpath doc/item[following-sibling::body[chapter[title[../parent::body]]]])
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1")
 (item (@ (id "toc2")) "chapter2")
 (item (@ (id "toc3")) "chapter3")
 (item (@ (id "toc4")) "chapter4")
 (item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc6")) "chapter6")
 (item (@ (id "toc7")) "chapter7"))
; <--- of:
(ddo:txpath
  "doc/item[following-sibling::body[chapter[title[../parent::body]]]]")
vddo:doc
)

; (ddo:txpath doc/item[@id[../text()[parent::item[count(preceding-sibling::item[position()<5])=4]]]])
(xtest-assert ; Expected result:
'((item (@ (id "toc5")) "chapter5")
 (item (@ (id "toc6")) "chapter6")
 (item (@ (id "toc7")) "chapter7"))
; <--- of:
(ddo:txpath
  "doc/item[@id[../text()[parent::item[count(preceding-sibling::item[position()<5])=4]]]]")
vddo:doc
)

; (ddo:txpath doc/body/chapter[title[../@id[../section[@ID[../p[text()[1]]]]]]])
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap2"))
   (title "Introduction")
   (section
     (@ (ID "sec2-1"))
     (p
      "This document is written in XML (eXtensible Markup Language) ver.1.0."))))
; <--- of:
(ddo:txpath
  "doc/body/chapter[title[../@id[../section[@ID[../p[text()[1]]]]]]]")
vddo:doc
)

; (ddo:txpath para[self::*[self::*[self::*[1]]]])
(xtest-assert ; Expected result:
'((para "text"))
; <--- of:
(ddo:txpath "para[self::*[self::*[self::*[1]]]]")
'(*TOP* (para "text"))
)

; (ddo:txpath stanza/line[text()[self::text() = ../parent::*[preceding-sibling::stanza[parent::poem[1]]]/line][1]])
(xtest-assert ; Expected result:
'((line "In the room the women come and go") (line "Talking of Michaelangelo."))
; <--- of:
(ddo:txpath
  "stanza/line[text()[self::text() = ../parent::*[preceding-sibling::stanza[parent::poem[1]]]/line][1]]")
'(poem
  (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
  (stanza
    (line "Let us go then, you and I,")
    (line "When the evening is spread out against the sky")
    (line "Like a patient etherized upon a table:"))
  (stanza
    (line "In the room the women come and go")
    (line "Talking of Michaelangelo.")))
)


;=========================================================================
; Core XPath function library


;------------------------------------------------
; Nodeset functions

; (ddo:xpath-expr  count( //item ) )
(xtest-assert ; Expected result:
'9
; <--- of:
(ddo:xpath-expr " count( //item ) ")
vddo:doc
)

; (ddo:xpath-expr  count( id('no_such_element') ) )
(xtest-assert ; Expected result:
'0
; <--- of:
(ddo:xpath-expr " count( id('no_such_element') ) ")
vddo:doc
)

; (ddo:xpath-expr  count( doc/body/chapter/p/text() ) )
(xtest-assert ; Expected result:
'5
; <--- of:
(ddo:xpath-expr " count( doc/body/chapter/p/text() ) ")
vddo:doc
)

; (ddo:xpath-expr id('chap4'))
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap4"))
   (title "What is XPointer?")
   (p
    "XPointer is the fragment identifier of documents having the mime-type hogehoge.")))
; <--- of:
(ddo:xpath-expr "id('chap4')")
vddo:doc
)

; (ddo:xpath-expr id('toc1 toc5 chap6 here'))
(xtest-assert ; Expected result:
'((item (@ (id "toc1")) "chapter1")
 (item (@ (id "toc5")) "chapter5")
 (chapter (@ (id "chap6")) (title "samples"))
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
(ddo:xpath-expr "id('toc1 toc5 chap6 here')")
vddo:doc
)

; (ddo:xpath-expr id('not-exist'))
(xtest-assert ; Expected result:
'()
; <--- of:
(ddo:xpath-expr "id('not-exist')")
vddo:doc
)

; (ddo:xpath-expr local-name(*))
(xtest-assert ; Expected result:
'"doc"
; <--- of:
(ddo:xpath-expr "local-name(*)")
vddo:doc
)

; (ddo:xpath-expr local-name(doc/body/*[position()<4]))
(xtest-assert ; Expected result:
'"chapter"
; <--- of:
(ddo:xpath-expr "local-name(doc/body/*[position()<4])")
vddo:doc
)

; (ddo:xpath-expr local-name(empty))
(xtest-assert ; Expected result:
'""
; <--- of:
(ddo:xpath-expr "local-name(empty)")
vddo:doc
)

; (ddo:xpath-expr local-name(descendant::text()[1]))
(xtest-assert ; Expected result:
'""
; <--- of:
(ddo:xpath-expr "local-name(descendant::text()[1])")
vddo:doc
)

; (ddo:xpath-expr local-name(doc/multidirectional/@*))
(xtest-assert ; Expected result:
'"type"
; <--- of:
(ddo:xpath-expr "local-name(doc/multidirectional/@*)")
vddo:doc
)

; (ddo:xpath-expr namespace-uri(doc/multidirectional/@*))
(xtest-assert ; Expected result:
'"http://www.w3.org/1999/xlink"
; <--- of:
(ddo:xpath-expr "namespace-uri(doc/multidirectional/@*)")
vddo:doc
)

; (ddo:xpath-expr namespace-uri(*))
(xtest-assert ; Expected result:
'""
; <--- of:
(ddo:xpath-expr "namespace-uri(*)")
vddo:doc
)

; (ddo:xpath-expr name(doc/multidirectional/@*))
(xtest-assert ; Expected result:
'"http://www.w3.org/1999/xlink:type"
; <--- of:
(ddo:xpath-expr "name(doc/multidirectional/@*)")
vddo:doc
)

; (ddo:xpath-expr name(empty))
(xtest-assert ; Expected result:
'""
; <--- of:
(ddo:xpath-expr "name(empty)")
vddo:doc
)

; (ddo:xpath-expr name(descendant::text()[1]))
(xtest-assert ; Expected result:
'""
; <--- of:
(ddo:xpath-expr "name(descendant::text()[1])")
vddo:doc
)


;------------------------------------------------
; String functions

; (ddo:xpath-expr string(0))
(xtest-assert ; Expected result:
'"0"
; <--- of:
(ddo:xpath-expr "string(0)")
'()
)

; (ddo:xpath-expr string(13))
(xtest-assert ; Expected result:
'"13"
; <--- of:
(ddo:xpath-expr "string(13)")
'()
)

; (ddo:xpath-expr string(666.666))
(xtest-assert ; Expected result:
'"666.666"
; <--- of:
(ddo:xpath-expr "string(666.666)")
'()
)

; (ddo:xpath-expr string(1 div 2))
(xtest-assert ; Expected result:
'"1.5"
; <--- of:
(ddo:xpath-expr "string(3 div 2)")
'()
)

; (ddo:xpath-expr string( true() ))
(xtest-assert ; Expected result:
'"true"
; <--- of:
(ddo:xpath-expr "string( true() )")
'()
)

; (ddo:xpath-expr string( false() ))
(xtest-assert ; Expected result:
'"false"
; <--- of:
(ddo:xpath-expr "string( false() )")
'()
)

; (ddo:xpath-expr string( doc/body/chapter[6] ))
(xtest-assert ; Expected result:
'"samples"
; <--- of:
(ddo:xpath-expr "string( doc/body/chapter[6] )")
vddo:doc
)

; (ddo:xpath-expr string( doc/body/chapter[1] ))
(xtest-assert ; Expected result:
'"AbstractThis document describes about XLink Engine..."
; <--- of:
(ddo:xpath-expr "string( doc/body/chapter[1] )")
vddo:doc
)

; (ddo:xpath-expr string( doc/body/chapter ))
(xtest-assert ; Expected result:
'"AbstractThis document describes about XLink Engine..."
; <--- of:
(ddo:xpath-expr "string( doc/body/chapter )")
vddo:doc
)

; (ddo:xpath-expr concat( 'one', ' ', 'beer' ))
(xtest-assert ; Expected result:
'"one beer"
; <--- of:
(ddo:xpath-expr "concat( 'one', ' ', 'beer' )")
'()
)

; (ddo:xpath-expr concat( doc/item[1], ' & ', doc/item[7] ))
(xtest-assert ; Expected result:
'"chapter1 & chapter7"
; <--- of:
(ddo:xpath-expr "concat( doc/item[1], ' & ', doc/item[7] )")
vddo:doc
)

; (ddo:xpath-expr starts-with( 'one gin', 'one' ))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "starts-with( 'one gin', 'one' )")
'()
)

; (ddo:xpath-expr starts-with( 'one', 'another' ))
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr "starts-with( 'one', 'another' )")
'()
)

; (ddo:xpath-expr starts-with( 'str1', 'str2' ))
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr "starts-with( 'str1', 'str2' )")
'()
)

; (ddo:xpath-expr starts-with( 'text', 'text' ))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "starts-with( 'text', 'text' )")
'()
)

; (ddo:xpath-expr contains( 'pirate', 'ira' ))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "contains( 'pirate', 'ira' )")
'()
)

; (ddo:xpath-expr contains( 'one string', 'different string' ))
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr "contains( 'one string', 'different string' )")
'()
)

; (ddo:xpath-expr contains( 'any', '' ))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "contains( 'any', '' )")
'()
)

; (ddo:xpath-expr doc/body/chapter[contains( p , 'XLink')])
(xtest-assert ; Expected result:
'((chapter
   (@ (id "chap1"))
   (title "Abstract")
   (p "This document describes about XLink Engine...")))
; <--- of:
(ddo:xpath-expr "doc/body/chapter[contains( p , 'XLink')]")
vddo:doc
)

; (ddo:xpath-expr substring-before( '1999/04/01', '/'))
(xtest-assert ; Expected result:
'"1999"
; <--- of:
(ddo:xpath-expr "substring-before( '1999/04/01', '/')")
'()
)

; (ddo:xpath-expr substring-before( '1999/04/01', ':'))
(xtest-assert ; Expected result:
'""
; <--- of:
(ddo:xpath-expr "substring-before( '1999/04/01', ':')")
'()
)

; (ddo:xpath-expr substring-before( doc/body/chapter[2] , 'XML'))
(xtest-assert ; Expected result:
'"IntroductionThis document is written in "
; <--- of:
(ddo:xpath-expr "substring-before( doc/body/chapter[2] , 'XML')")
vddo:doc
)

; (ddo:xpath-expr substring-after( '1999/04/01', '/'))
(xtest-assert ; Expected result:
'"04/01"
; <--- of:
(ddo:xpath-expr "substring-after( '1999/04/01', '/')")
'()
)

; (ddo:xpath-expr substring-after( '1999/04/01', '19'))
(xtest-assert ; Expected result:
'"99/04/01"
; <--- of:
(ddo:xpath-expr "substring-after( '1999/04/01', '19')")
'()
)

; (ddo:xpath-expr substring-after( '1999/04/01', ':'))
(xtest-assert ; Expected result:
'""
; <--- of:
(ddo:xpath-expr "substring-after( '1999/04/01', ':')")
'()
)

; (ddo:xpath-expr substring-after( doc/body/chapter[2] , 'XML'))
(xtest-assert ; Expected result:
'" (eXtensible Markup Language) ver.1.0."
; <--- of:
(ddo:xpath-expr "substring-after( doc/body/chapter[2] , 'XML')")
vddo:doc
)

; (ddo:xpath-expr substring('12345', 1.5, 2.6) )
(xtest-assert ; Expected result:
'"234"
; <--- of:
(ddo:xpath-expr "substring('12345', 1.5, 2.6) ")
'()
)

; (ddo:xpath-expr substring('12345', 0, 3))
(xtest-assert ; Expected result:
'"12"
; <--- of:
(ddo:xpath-expr "substring('12345', 0, 3)")
'()
)

; (ddo:xpath-expr substring('12345', 2))
(xtest-assert ; Expected result:
'"2345"
; <--- of:
(ddo:xpath-expr "substring('12345', 2)")
'()
)

; (ddo:xpath-expr substring('12345', -10, 100))
(xtest-assert ; Expected result:
'"12345"
; <--- of:
(ddo:xpath-expr "substring('12345', -10, 100)")
'()
)

; (ddo:xpath-expr substring('12345', 10, 2))
(xtest-assert ; Expected result:
'""
; <--- of:
(ddo:xpath-expr "substring('12345', 10, 2)")
'()
)

; (ddo:xpath-expr string-length('26.08.2004'))
(xtest-assert ; Expected result:
'10
; <--- of:
(ddo:xpath-expr "string-length('26.08.2004')")
'()
)

; (ddo:xpath-expr string-length(doc/item[position()=1]))
(xtest-assert ; Expected result:
'8
; <--- of:
(ddo:xpath-expr "string-length(doc/item[position()=1])")
vddo:doc
)

; (ddo:xpath-expr doc/item[string-length()-1])
(xtest-assert ; Expected result:
'((item (@ (id "toc7")) "chapter7"))
; <--- of:
(ddo:xpath-expr "doc/item[string-length()-1]")
vddo:doc
)

; (ddo:xpath-expr normalize-space('   Want   to    have   a lunch       '))
(xtest-assert ; Expected result:
'"Want to have a lunch"
; <--- of:
(ddo:xpath-expr "normalize-space('   Want   to    have   a lunch       ')")
'()
)

; (ddo:xpath-expr normalize-space('         '))
(xtest-assert ; Expected result:
'""
; <--- of:
(ddo:xpath-expr "normalize-space('         ')")
'()
)

; (ddo:xpath-expr normalize-space(''))
(xtest-assert ; Expected result:
'""
; <--- of:
(ddo:xpath-expr "normalize-space('')")
'()
)

; (ddo:xpath-expr normalize-space(.))
(xtest-assert ; Expected result:
'"Want to have a lunch"
; <--- of:
(ddo:xpath-expr "normalize-space(.)")
'(p "   Want   to    have   a lunch       ")
)

; (ddo:xpath-expr translate('bar', 'abc', 'ABC'))
(xtest-assert ; Expected result:
'"BAr"
; <--- of:
(ddo:xpath-expr "translate('bar', 'abc', 'ABC')")
'()
)

; (ddo:xpath-expr translate('bar', 'abc', 'ABCDEFGHR'))
(xtest-assert ; Expected result:
'"BAr"
; <--- of:
(ddo:xpath-expr "translate('bar', 'abc', 'ABCDEFGHR')")
'()
)

; (ddo:xpath-expr translate('--aaa--', 'abc-', 'ABC') )
(xtest-assert ; Expected result:
'"AAA"
; <--- of:
(ddo:xpath-expr "translate('--aaa--', 'abc-', 'ABC') ")
'()
)

; (ddo:xpath-expr translate(doc/body/chapter[1]/title, 'Aact', 'a.') )
(xtest-assert ; Expected result:
'"absr."
; <--- of:
(ddo:xpath-expr "translate(doc/body/chapter[1]/title, 'Aact', 'a.') ")
vddo:doc
)

; (ddo:xpath-expr translate(doc/body/chapter[2]/title, doc/body/chapter[2]/title, doc/body/chapter[1]/title) )
(xtest-assert ; Expected result:
'"Abstractsrb"
; <--- of:
(ddo:xpath-expr
  "translate(doc/body/chapter[2]/title, doc/body/chapter[2]/title, doc/body/chapter[1]/title) ")
vddo:doc
)


;------------------------------------------------
; Boolean functions

; (ddo:xpath-expr boolean(*/*))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "boolean(*/*)")
vddo:doc
)

; (ddo:xpath-expr boolean(*/no_such))
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr "boolean(*/no_such)")
vddo:doc
)

; (ddo:xpath-expr boolean( name(*) ))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "boolean( name(*) )")
vddo:doc
)

; (ddo:xpath-expr boolean( name(doc/item[1]/text()) ))
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr "boolean( name(doc/item[1]/text()) )")
vddo:doc
)

; (ddo:xpath-expr boolean( 1 ))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "boolean( 1 )")
'()
)

; (ddo:xpath-expr boolean( 0 ))
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr "boolean( 0 )")
'()
)

; (ddo:xpath-expr true())
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "true()")
'()
)

; (ddo:xpath-expr false())
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr "false()")
'()
)

; (ddo:xpath-expr not( true() ))
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr "not( true() )")
'()
)

; (ddo:xpath-expr not( false() ))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "not( false() )")
'()
)

; (ddo:xpath-expr lang('en'))
(xtest-assert ; Expected result:
'#f
; <--- of:
(ddo:xpath-expr "lang('en')")
((ddo:txpath "doc/body/chapter[1]" -1) vddo:doc)
)

; (ddo:xpath-expr lang('en'))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "lang('en')")
'(div (@ (xml:lang "en")) (para))
)

; (ddo:xpath-expr lang('en'))
(xtest-assert ; Expected result:
'#t
; <--- of:
(ddo:xpath-expr "lang('en')")
((ddo:txpath "para" -1) '(div (@ (xml:lang "en")) (para)))
)


;------------------------------------------------
; Number functions

; (ddo:xpath-expr number('12'))
(xtest-assert ; Expected result:
'12
; <--- of:
(ddo:xpath-expr "number('12')")
'()
)

; (ddo:xpath-expr number('-12.45'))
(xtest-assert ; Expected result:
'-12.45
; <--- of:
(ddo:xpath-expr "number('-12.45')")
'()
)

; (ddo:xpath-expr number( true() ))
(xtest-assert ; Expected result:
'1
; <--- of:
(ddo:xpath-expr "number( true() )")
'()
)

; (ddo:xpath-expr number( false() ))
(xtest-assert ; Expected result:
'0
; <--- of:
(ddo:xpath-expr "number( false() )")
'()
)

; (ddo:xpath-expr number( self::* ))
(xtest-assert ; Expected result:
'67
; <--- of:
(ddo:xpath-expr "number( self::* )")
'(para "67")
)

; (ddo:xpath-expr number( self::* ))
(xtest-assert ; Expected result:
'67.12
; <--- of:
(ddo:xpath-expr "number( self::* )")
'(para (high "67") (low ".12"))
)

; (ddo:xpath-expr sum( self::text() ))
(xtest-assert ; Expected result:
'8
; <--- of:
(ddo:xpath-expr "sum( self::text() )")
'("1" "2" "5")
)

; (ddo:xpath-expr sum( * ))
(xtest-assert ; Expected result:
'20.12
; <--- of:
(ddo:xpath-expr "sum( * )")
'(add (p "4") (p "15") (p (high "1") (low ".12")))
)

; (ddo:xpath-expr floor(.4))
(xtest-assert ; Expected result:
'0
; <--- of:
(ddo:xpath-expr "floor(.4)")
'()
)

; (ddo:xpath-expr floor(-2.5))
(xtest-assert ; Expected result:
'-3
; <--- of:
(ddo:xpath-expr "floor(-2.5)")
'()
)

; (ddo:xpath-expr  ceiling(.4))
(xtest-assert ; Expected result:
'1
; <--- of:
(ddo:xpath-expr " ceiling(.4)")
'()
)

; (ddo:xpath-expr ceiling(-2.5) )
(xtest-assert ; Expected result:
'-2
; <--- of:
(ddo:xpath-expr "ceiling(-2.5) ")
'()
)

; (ddo:xpath-expr round(.4))
(xtest-assert ; Expected result:
'0
; <--- of:
(ddo:xpath-expr "round(.4)")
'()
)

; (ddo:xpath-expr  round(4.51) )
(xtest-assert ; Expected result:
'5
; <--- of:
(ddo:xpath-expr " round(4.51) ")
'()
)

; (ddo:xpath-expr round(-2.51) )
(xtest-assert ; Expected result:
'-3
; <--- of:
(ddo:xpath-expr "round(-2.51) ")
'()
)

(cout nl "DDO SXPath tests passed successfully!" nl)
