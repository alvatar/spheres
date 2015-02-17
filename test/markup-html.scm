;;; The HtmlPrag test suite can be enabled by editing the source code file and
;;; loading [Testeez]; the test suite is disabled by default.

(define (%htmlprag:test)
  (%htmlprag:testeez
   "HtmlPrag"

   (test-define "" lf (string (%htmlprag:a2c 10)))

   (test/equal "" (html->shtml "<a>>") `(,shtml-top-symbol (a ">")))
   (test/equal "" (html->shtml "<a<>") `(,shtml-top-symbol (a "<" ">")))

   (test/equal "" (html->shtml "<>")      `(,shtml-top-symbol "<" ">"))
   (test/equal "" (html->shtml "< >")     `(,shtml-top-symbol "<" ">"))
   (test/equal "" (html->shtml "< a>")    `(,shtml-top-symbol (a)))
   (test/equal "" (html->shtml "< a / >") `(,shtml-top-symbol (a)))

   (test/equal "" (html->shtml "<a<")  `(,shtml-top-symbol (a "<")))
   (test/equal "" (html->shtml "<a<b") `(,shtml-top-symbol (a (b))))

   (test/equal "" (html->shtml "><a>") `(,shtml-top-symbol ">" (a)))

   (test/equal "" (html->shtml "</>") `(,shtml-top-symbol))

   (test/equal "" (html->shtml "<\">") `(,shtml-top-symbol "<" "\"" ">"))

   (test/equal ""
               (html->shtml (string-append "<a>xxx<plaintext>aaa" lf
                                           "bbb" lf
                                           "c<c<c"))
               `(,shtml-top-symbol
                 (a "xxx" (plaintext ,(string-append "aaa" lf)
                                     ,(string-append "bbb" lf)
                                     "c<c<c"))))

   (test/equal ""
               (html->shtml "aaa<!-- xxx -->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol " xxx ")   "bbb"))

   (test/equal ""
               (html->shtml "aaa<! -- xxx -->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol " xxx ")   "bbb"))

   (test/equal ""
               (html->shtml "aaa<!-- xxx --->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol " xxx -")  "bbb"))

   (test/equal ""
               (html->shtml "aaa<!-- xxx ---->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol " xxx --") "bbb"))

   (test/equal ""
               (html->shtml "aaa<!-- xxx -y-->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol " xxx -y") "bbb"))

   (test/equal ""
               (html->shtml "aaa<!----->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol "-")       "bbb"))

   (test/equal ""
               (html->shtml "aaa<!---->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol "")        "bbb"))

   (test/equal ""
               (html->shtml "aaa<!--->bbb")
               `(,shtml-top-symbol "aaa" (,shtml-comment-symbol "->bbb")))

   (test/equal "" (html->shtml "<hr>")   `(,shtml-top-symbol (hr)))
   (test/equal "" (html->shtml "<hr/>")  `(,shtml-top-symbol (hr)))
   (test/equal "" (html->shtml "<hr />") `(,shtml-top-symbol (hr)))

   (test/equal ""
               (html->shtml "<hr noshade>")
               `(,shtml-top-symbol (hr (@ (noshade)))))
   (test/equal ""
               (html->shtml "<hr noshade/>")
               `(,shtml-top-symbol (hr (@ (noshade)))))
   (test/equal ""
               (html->shtml "<hr noshade />")
               `(,shtml-top-symbol (hr (@ (noshade)))))
   (test/equal ""
               (html->shtml "<hr noshade / >")
               `(,shtml-top-symbol (hr (@ (noshade)))))
   (test/equal ""
               (html->shtml "<hr noshade=1 />")
               `(,shtml-top-symbol (hr (@ (noshade "1")))))
   (test/equal ""
               (html->shtml "<hr noshade=1/>")
               `(,shtml-top-symbol (hr (@ (noshade "1/")))))

   (test/equal ""
               (html->shtml "<q>aaa<p/>bbb</q>ccc</p>ddd")
               `(,shtml-top-symbol (q "aaa" (p) "bbb") "ccc" "ddd"))

   (test/equal "" (html->shtml "&lt;") `(,shtml-top-symbol "<"))
   (test/equal "" (html->shtml "&gt;") `(,shtml-top-symbol ">"))

   (test/equal ""
               (html->shtml "Gilbert &amp; Sullivan")
               `(,shtml-top-symbol "Gilbert & Sullivan"))
   (test/equal ""
               (html->shtml "Gilbert &amp Sullivan")
               `(,shtml-top-symbol "Gilbert & Sullivan"))
   (test/equal ""
               (html->shtml "Gilbert & Sullivan")
               `(,shtml-top-symbol "Gilbert & Sullivan"))

   (test/equal ""
               (html->shtml "Copyright &copy; Foo")
               `(,shtml-top-symbol "Copyright "
                                   (& ,(string->symbol "copy"))
                                   " Foo"))
   (test/equal ""
               (html->shtml "aaa&copy;bbb")
               `(,shtml-top-symbol
                 "aaa" (& ,(string->symbol "copy")) "bbb"))
   (test/equal ""
               (html->shtml "aaa&copy")
               `(,shtml-top-symbol
                 "aaa" (& ,(string->symbol "copy"))))

   (test/equal "" (html->shtml "&#42;")  `(,shtml-top-symbol "*"))
   (test/equal "" (html->shtml "&#42")   `(,shtml-top-symbol "*"))
   (test/equal "" (html->shtml "&#42x")  `(,shtml-top-symbol "*x"))
   (test/equal "" (html->shtml "&#151")  `(,shtml-top-symbol
                                           ,(string (%htmlprag:a2c 151))))
   (test/equal "" (html->shtml "&#1000") `(,shtml-top-symbol (& 1000)))
   (test/equal "" (html->shtml "&#x42")  `(,shtml-top-symbol "B"))
   (test/equal "" (html->shtml "&#xA2")  `(,shtml-top-symbol
                                           ,(string (%htmlprag:a2c 162))))
   (test/equal "" (html->shtml "&#xFF")  `(,shtml-top-symbol
                                           ,(string (%htmlprag:a2c 255))))
   (test/equal "" (html->shtml "&#x100") `(,shtml-top-symbol (& 256)))
   (test/equal "" (html->shtml "&#X42")  `(,shtml-top-symbol "B"))
   (test/equal "" (html->shtml "&42;")   `(,shtml-top-symbol "&42;"))

   (test/equal ""
               (html->shtml (string-append "aaa&copy;bbb&amp;ccc&lt;ddd&&gt;"
                                           "eee&#42;fff&#1000;ggg&#x5a;hhh"))
               `(,shtml-top-symbol
                 "aaa"
                 (& ,(string->symbol "copy"))
                 "bbb&ccc<ddd&>eee*fff"
                 (& 1000)
                 "gggZhhh"))

   (test/equal ""
               (html->shtml
                (string-append
                 "<IMG src=\"http://e.e/aw/pics/listings/"
                 "ebayLogo_38x16.gif\" border=0 width=\"38\" height=\"16\" "
                 "HSPACE=5 VSPACE=0\">2</FONT>"))
               `(,shtml-top-symbol
                 (img (@
                       (src
                        "http://e.e/aw/pics/listings/ebayLogo_38x16.gif")
                       (border "0") (width "38") (height "16")
                       (hspace "5") (vspace "0")))
                 "2"))

   (test/equal ""
               (html->shtml "<aaa bbb=ccc\"ddd>eee")
               `(,shtml-top-symbol (aaa (@ (bbb "ccc") (ddd)) "eee")))
   (test/equal ""
               (html->shtml "<aaa bbb=ccc \"ddd>eee")
               `(,shtml-top-symbol (aaa (@ (bbb "ccc") (ddd)) "eee")))

   (test/equal ""
               (html->shtml
                (string-append
                 "<HTML><Head><Title>My Title</Title></Head>"
                 "<Body BGColor=\"white\" Foo=42>"
                 "This is a <B><I>bold-italic</B></I> test of </Erk>"
                 "broken HTML.<br>Yes it is.</Body></HTML>"))
               `(,shtml-top-symbol
                 (html (head (title "My Title"))
                       (body (@ (bgcolor "white") (foo "42"))
                             "This is a "
                             (b (i "bold-italic"))
                             " test of "
                             "broken HTML."
                             (br)
                             "Yes it is."))))

   (test/equal ""
               (html->shtml
                (string-append
                 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
                 " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
               `(,shtml-top-symbol
                 (,shtml-decl-symbol
                  ,(string->symbol "DOCTYPE")
                  html
                  ,(string->symbol "PUBLIC")
                  "-//W3C//DTD XHTML 1.0 Strict//EN"
                  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")))

   (test/equal ""
               (html->shtml
                (string-append
                 "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
                 "xml:lang=\"en\" "
                 "lang=\"en\">"))
               `(,shtml-top-symbol
                 (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                          (xml:lang "en") (lang "en")))))

   (test/equal
    ""
    (html->shtml
     (string-append
      "<html:html xmlns:html=\"http://www.w3.org/TR/REC-html40\">"
      "<html:head><html:title>Frobnostication</html:title></html:head>"
      "<html:body><html:p>Moved to <html:a href=\"http://frob.com\">"
      "here.</html:a></html:p></html:body></html:html>"))
    `(,shtml-top-symbol
      (html (@ (xmlns:html "http://www.w3.org/TR/REC-html40"))
            (head (title "Frobnostication"))
            (body (p "Moved to "
                     (a (@ (href "http://frob.com"))
                        "here."))))))

   (test/equal ""
               (html->shtml
                (string-append
                 "<RESERVATION xmlns:HTML=\"http://www.w3.org/TR/REC-html40\">"
                 "<NAME HTML:CLASS=\"largeSansSerif\">Layman, A</NAME>"
                 "<SEAT CLASS=\"Y\" HTML:CLASS=\"largeMonotype\">33B</SEAT>"
                 "<HTML:A HREF=\"/cgi-bin/ResStatus\">Check Status</HTML:A>"
                 "<DEPARTURE>1997-05-24T07:55:00+1</DEPARTURE></RESERVATION>"))
               `(,shtml-top-symbol
                 (reservation (@ (,(string->symbol "xmlns:HTML")
                                  "http://www.w3.org/TR/REC-html40"))
                              (name (@ (class "largeSansSerif"))
                                    "Layman, A")
                              (seat (@ (class "Y") (class "largeMonotype"))
                                    "33B")
                              (a (@ (href "/cgi-bin/ResStatus"))
                                 "Check Status")
                              (departure "1997-05-24T07:55:00+1"))))

   (test/equal
    ""
    (html->shtml
     (string-append
      "<html><head><title></title><title>whatever</title></head><body>"
      "<a href=\"url\">link</a><p align=center><ul compact style=\"aa\">"
      "<p>BLah<!-- comment <comment> --> <i> italic <b> bold <tt> ened </i>"
      " still &lt; bold </b></body><P> But not done yet..."))
    `(,shtml-top-symbol
      (html (head (title) (title "whatever"))
            (body (a (@ (href "url")) "link")
                  (p (@ (align "center"))
                     (ul (@ (compact) (style "aa"))))
                  (p "BLah"
                     (,shtml-comment-symbol " comment <comment> ")
                     " "
                     (i " italic " (b " bold " (tt " ened ")))
                     " still < bold "))
            (p " But not done yet..."))))

   (test/equal ""
               (html->shtml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
               `(,shtml-top-symbol
                 (,shtml-pi-symbol xml "version=\"1.0\" encoding=\"UTF-8\"")))

   (test/equal ""
               (html->shtml "<?php php_info(); ?>")
               `(,shtml-top-symbol (,shtml-pi-symbol php "php_info(); ")))
   (test/equal ""
               (html->shtml "<?php php_info(); ?")
               `(,shtml-top-symbol (,shtml-pi-symbol php "php_info(); ?")))
   (test/equal ""
               (html->shtml "<?php php_info(); ")
               `(,shtml-top-symbol (,shtml-pi-symbol php "php_info(); ")))

   (test/equal ""
               (html->shtml "<?foo bar ? baz > blort ?>")
               `(,shtml-top-symbol
                 (,shtml-pi-symbol foo "bar ? baz > blort ")))

   (test/equal ""
               (html->shtml "<?foo b?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol foo "b") "x"))
   (test/equal ""
               (html->shtml "<?foo ?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol foo "")  "x"))
   (test/equal ""
               (html->shtml "<?foo ?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol foo "")  "x"))
   (test/equal ""
               (html->shtml "<?foo?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol foo "")  "x"))
   (test/equal ""
               (html->shtml "<?f?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol f   "")  "x"))
   (test/equal ""
               (html->shtml "<??>x")
               `(,shtml-top-symbol (,shtml-pi-symbol #f  "")  "x"))
   (test/equal ""
               (html->shtml "<?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol #f  ">x")))

   (test/equal ""
               (html->shtml "<foo bar=\"baz\">blort")
               `(,shtml-top-symbol (foo (@ (bar "baz")) "blort")))
   (test/equal ""
               (html->shtml "<foo bar='baz'>blort")
               `(,shtml-top-symbol (foo (@ (bar "baz")) "blort")))
   (test/equal ""
               (html->shtml "<foo bar=\"baz'>blort")
               `(,shtml-top-symbol (foo (@ (bar "baz'>blort")))))
   (test/equal ""
               (html->shtml "<foo bar='baz\">blort")
               `(,shtml-top-symbol (foo (@ (bar "baz\">blort")))))

   (test/equal ""
               (html->shtml (string-append "<p>A</p>"
                                           "<script>line0 <" lf
                                           "line1" lf
                                           "<line2></script>"
                                           "<p>B</p>"))
               `(,shtml-top-symbol (p "A")
                                   (script ,(string-append "line0 <" lf)
                                           ,(string-append "line1"   lf)
                                           "<line2>")
                                   (p "B")))

   (test/equal ""
               (html->shtml "<xmp>a<b>c</XMP>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))
   (test/equal ""
               (html->shtml "<XMP>a<b>c</xmp>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))
   (test/equal ""
               (html->shtml "<xmp>a<b>c</foo:xmp>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))
   (test/equal ""
               (html->shtml "<foo:xmp>a<b>c</xmp>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))
   (test/equal ""
               (html->shtml "<foo:xmp>a<b>c</foo:xmp>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))
   (test/equal ""
               (html->shtml "<foo:xmp>a<b>c</bar:xmp>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))

   (test/equal ""
               (html->shtml "<xmp>a</b>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b>c")     "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b >c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b >c")    "d"))
   (test/equal ""
               (html->shtml "<xmp>a</ b>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</ b>c")    "d"))
   (test/equal ""
               (html->shtml "<xmp>a</ b >c</xmp>d")
               `(,shtml-top-symbol (xmp "a</ b >c")   "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b:x>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b:x>c")   "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b::x>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b::x>c")  "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b:::x>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b:::x>c") "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b:>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b:>c")    "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b::>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b::>c")   "d"))
   (test/equal ""
               (html->shtml "<xmp>a</xmp:b>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</xmp:b>c") "d"))

   (test-define "expected output for next two tests"
                expected
                `(,shtml-top-symbol (p "real1")
                                    ,lf
                                    (xmp ,lf
                                         ,(string-append "alpha"       lf)
                                         ,(string-append "<P>fake</P>" lf)
                                         ,(string-append "bravo"       lf))
                                    (p "real2")))

   (test/equal ""
               (html->shtml (string-append "<P>real1</P>" lf
                                           "<XMP>"        lf
                                           "alpha"        lf
                                           "<P>fake</P>"  lf
                                           "bravo"        lf
                                           "</XMP "       lf
                                           "<P>real2</P>"))
               expected)

   (test/equal ""
               (html->shtml (string-append "<P>real1</P>" lf
                                           "<XMP>"        lf
                                           "alpha"        lf
                                           "<P>fake</P>"  lf
                                           "bravo"        lf
                                           "</XMP"        lf
                                           "<P>real2</P>"))
               expected)

   (test/equal ""
               (html->shtml "<xmp>a</xmp>x")
               `(,shtml-top-symbol (xmp "a")   "x"))
   (test/equal ""
               (html->shtml (string-append "<xmp>a" lf "</xmp>x"))
               `(,shtml-top-symbol (xmp ,(string-append "a" lf)) "x"))
   (test/equal ""
               (html->shtml "<xmp></xmp>x")
               `(,shtml-top-symbol (xmp)       "x"))

   (test/equal ""
               (html->shtml "<xmp>a</xmp") `(,shtml-top-symbol (xmp "a")))
   (test/equal ""
               (html->shtml "<xmp>a</xm")  `(,shtml-top-symbol (xmp "a</xm")))
   (test/equal ""
               (html->shtml "<xmp>a</x")   `(,shtml-top-symbol (xmp "a</x")))
   (test/equal ""
               (html->shtml "<xmp>a</")    `(,shtml-top-symbol (xmp "a</")))
   (test/equal ""
               (html->shtml "<xmp>a<")     `(,shtml-top-symbol (xmp "a<")))
   (test/equal ""
               (html->shtml "<xmp>a")      `(,shtml-top-symbol (xmp "a")))
   (test/equal ""
               (html->shtml "<xmp>")       `(,shtml-top-symbol (xmp)))
   (test/equal ""
               (html->shtml "<xmp")        `(,shtml-top-symbol (xmp)))

   (test/equal ""
               (html->shtml "<xmp x=42 ")
               `(,shtml-top-symbol (xmp (@ (x "42")))))
   (test/equal ""
               (html->shtml "<xmp x= ")   `(,shtml-top-symbol (xmp (@ (x)))))
   (test/equal ""
               (html->shtml "<xmp x ")    `(,shtml-top-symbol (xmp (@ (x)))))
   (test/equal ""
               (html->shtml "<xmp x")     `(,shtml-top-symbol (xmp (@ (x)))))

   (test/equal ""
               (html->shtml "<script>xxx")
               `(,shtml-top-symbol (script "xxx")))
   (test/equal ""
               (html->shtml "<script/>xxx")
               `(,shtml-top-symbol (script) "xxx"))

   (test/equal ""
               (html->shtml "<html xml:lang=\"en\" lang=\"en\">")
               `(,shtml-top-symbol (html (@ (xml:lang "en") (lang "en")))))

   (test/equal ""
               (html->shtml "<a href=/foo.html>")
               `(,shtml-top-symbol (a (@ (href "/foo.html")))))
   (test/equal ""
               (html->shtml "<a href=/>foo.html")
               `(,shtml-top-symbol (a (@ (href "/")) "foo.html")))

   ;; TODO: Add verbatim-pair cases with attributes in the end tag.

   (test/equal ""
               (shtml->html '(p))            "<p></p>")
   (test/equal ""
               (shtml->html '(p "CONTENT"))  "<p>CONTENT</p>")
   (test/equal ""
               (shtml->html '(br))           "<br />")
   (test/equal ""
               (shtml->html '(br "CONTENT")) "<br />")

   (test/equal ""
               (shtml->html `(hr (@ (clear "all"))))
               "<hr clear=\"all\" />")

   (test/equal ""
               (shtml->html `(hr (@ (noshade))))
               "<hr noshade />")
   (test/equal ""
               (shtml->html `(hr (@ (noshade #t))))
               "<hr noshade />") ;; TODO: Maybe lose this test.
   (test/equal ""
               (shtml->html `(hr (@ (noshade "noshade"))))
               "<hr noshade=\"noshade\" />")

   (test/equal ""
               (shtml->html `(hr (@ (aaa "bbbccc"))))
               "<hr aaa=\"bbbccc\" />")
   (test/equal ""
               (shtml->html `(hr (@ (aaa "bbb'ccc"))))
               "<hr aaa=\"bbb'ccc\" />")
   (test/equal ""
               (shtml->html `(hr (@ (aaa "bbb\"ccc"))))
               "<hr aaa='bbb\"ccc' />")
   (test/equal ""
               (shtml->html `(hr (@ (aaa "bbb\"ccc'ddd"))))
               "<hr aaa=\"bbb&quot;ccc'ddd\" />")

   (test/equal "" (shtml->html '(& "copy"))                   "&copy;")
   (test/equal "" (shtml->html '(& "rArr"))                   "&rArr;")
   (test/equal "" (shtml->html `(& ,(string->symbol "rArr"))) "&rArr;")
   (test/equal "" (shtml->html '(& 151))                      "&#151;")

   (test/equal ""
               (html->shtml "&copy;")
               `(,shtml-top-symbol (& ,(string->symbol "copy"))))
   (test/equal ""
               (html->shtml "&rArr;")
               `(,shtml-top-symbol (& ,(string->symbol "rArr"))))
   (test/equal ""
               (html->shtml "&#151;")
               `(,shtml-top-symbol ,(string (%htmlprag:a2c 151))))

   (test/equal ""
               (html->shtml "&#999;")
               `(,shtml-top-symbol (& 999)))

   (test/equal ""
               (shtml->html
                `(,shtml-pi-symbol xml "version=\"1.0\" encoding=\"UTF-8\""))
               "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")

   (test/equal ""
               (shtml->html
                `(,shtml-decl-symbol
                  ,(string->symbol "DOCTYPE")
                  html
                  ,(string->symbol "PUBLIC")
                  "-//W3C//DTD XHTML 1.0 Strict//EN"
                  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"))
               (string-append
                "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
                " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))

   (test/equal ""
               (shtml-entity-value '(*ENTITY* "shtml-named-char" "rArr"))
               (string->symbol "rArr"))

   (test/equal ""
               (shtml-entity-value '(& "rArr"))
               (string->symbol "rArr"))

   (test/equal ""
               (shtml-entity-value `(& ,(string->symbol "rArr")))
               (string->symbol "rArr"))

   ;; TODO: Write more test cases for HTML encoding.

   ;; TODO: Write test cases for foreign-filter of HTML writing.

   ;; TODO: Write test cases for attribute values that aren't simple strings.

   ;; TODO: Document this.
   ;;
   ;; (define html-1 "<myelem myattr=\"&\">")
   ;; (define shtml   (html->shtml html-1))
   ;; shtml
   ;; (define html-2 (shtml->html shtml))
   ;; html-2

   ))
