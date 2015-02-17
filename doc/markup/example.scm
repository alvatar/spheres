
(include "example.sch")


;-------------------------------------------------
; Examples

(pp-code-eval
 
 "Examples that illustrate the primary high-level functions of the"
 "SSAX-SXML package"
 (newline)

 "Obtaining an SXML document from XML"
 (sxml:document "XML/poem.xml")

 "Accessing parts of the document with SXPath"
 ((sxpath "poem/stanza[2]/line/text()")
  (sxml:document "XML/poem.xml"))

 ;; The equivalent semantics can be reflected with SXPath native syntax
 ;((sxpath '(poem (stanza 2) line *text*))
 ; (sxml:document "XML/poem.xml"))
 
 "-------------------------------------"
 "SXML Transformations"
 (newline)

 "Transforming the document according to XSLT stylesheet"
 (apply string-append
        (sxml:clean-feed
         (stx:transform-dynamic
          (sxml:add-parents (sxml:document "XML/poem.xml"))
          (stx:make-stx-stylesheet
           (sxml:document
            "XML/poem2html.xsl"
            '[(xsl . "http://www.w3.org/1999/XSL/Transform")])))))

 "Expressing the same transformation in pre-post-order"
 (pre-post-order
  (sxml:document "XML/poem.xml")
  `((*TOP* *macro* . ,(lambda top
                        (car ((sxpath '(*)) top))))
    (poem . ,(lambda elem
               `(html
                 (head
                  (title ,((sxpath "string(@title)") elem)))
                 (body
                  (h1 ,((sxpath "string(@title)") elem))
                  ,@((sxpath "node()") elem)
                  (i ,((sxpath "string(@poet)") elem))))))
    (@ *preorder* . ,(lambda x x))
    (stanza . ,(lambda (tag . content)
                 `(p ,@(map-union
                        (lambda (x) x)
                        content))))
    (line . ,(lambda (tag . content)
               (append content '((br)))))
    (*text* . ,(lambda (tag text) text))))
 
 "-------------------------------------"
 "XPathLink: a query language with XLink support"
 (newline)
 
 "Returning a chapter element that is linked with the first item"
 "in the table of contents" 
 ((sxpath/c "doc/item[1]/traverse::chapter")
  (xlink:documents "XML/doc.xml"))
 
 "-------------------------------------"
 "SXML Modifications"
 (newline)
 
 (cond-expand
  (gambit
   (if
    (not (equal? (cadar '(("a" ("b" "c")) ("e" "f")))
                 '("b" "c")))
    (begin
      (display "You have to fix the ``cadar bug'' in Gambit for running")
      (newline)
      (display "the remaining examples.")
      (newline)
      (display "Visit the following link for more information:")
      (newline)
      (display "http://mailman.iro.umontreal.ca/pipermail/gambit-list/2005-July/000315.html")
      (newline)
      (exit -1))
    #t))
  (else #t))
       
 "Modifying the SXML representation of the document"
 ((sxml:modify
   '("/poem/stanza[2]" move-preceding "preceding-sibling::stanza" ))
  (sxml:document "XML/poem.xml"))
 
 "-------------------------------------"
 "DDO SXPath: the optimized XPath implementation"
 (newline)
 
 "Return all text nodes that follow the keyword ``XPointer'' and"
 "that are not descendants of the element appendix"
 ((ddo:sxpath
   "//text()[contains(., 'XPointer')]/
   following::text()[not(./ancestor::appendix)]")
  (sxml:document "XML/doc.xml"))

 "-------------------------------------"
 "Lazy XML processing"
 (newline)

 "Lazy XML-to-SXML conversion"
 (define doc
   (lazy:xml->sxml (open-input-file "XML/poem.xml") '()))
 doc

 "Querying a lazy SXML document, lazyly"
 (define res ((lazy:sxpath "poem/stanza/line[1]") doc))
 res

 "Obtain the next portion of the result"
 (force (cadr res))

 "Converting the lazy result to a conventional SXML nodeset"
 (lazy:result->list res)
 
 "-------------------------------------"
 "SXML Serialization"
 (newline)

 "Returns the string that contains the serialized representation of the"
 "SXML element into XML"
 [srl:sxml->xml
  '(doc (title "Hello world"))]
 
 "Writing the serialized representation of the SXML document to the"
 "output port"
 [srl:sxml->xml
  '(*TOP*
    (@ (*NAMESPACES*
        (foo "http://www.foo.net")))
    (*PI* xml "version='1.0'")
    (shipment (@ (weight 100) (unit "kg") (delivered))
     (*COMMENT* "Comment node")      
     (description "Shipment" (& 32) "description")
     (foo:empty)))
  (current-output-port)]
 
 "Serialization into HTML"
 [srl:sxml->html
  '(table (@ (border))
          (tr (td "Item 1")
              (td (@ (rowspan 2)) "Item 2")
              (td "Item 3"))
          (tr (td "Item 4") (td "Item 5")))
  (current-output-port)]

 "Parameterizing serializer"
 [srl:parameterizable
  '(tag (@ (attr1 "value1") (attr2 "value2"))
        (nested "text node")
        (empty))
  (current-output-port)
  '(method . xml)
  '(indent . "    ")
  '(omit-xml-declaration . #f)
  '(standalone . #t)]

 "=========================================================="
 "Querying remote resources"
 "The following examples may not produce the expected result "
 "if your computer is not connected to the Internet"
 (newline)

 "Obtaining the SXML representation of the remote XML document"
 (sxml:document "http://modis.ispras.ru/Lizorkin/XML/poem.xml")

 "Obtaining/querying HTML documents"
 ((sxpath "html/head/title")
  (sxml:document "http://modis.ispras.ru/Lizorkin/index.html"))

 "Traversing between documents with XPathLink"
 ((sxpath/c
   "descendant::a[.='XPathLink']/traverse::html/
   descendant::blockquote[1]/node()")
  (xlink:documents "http://modis.ispras.ru/Lizorkin/index.html"))
 
 )
