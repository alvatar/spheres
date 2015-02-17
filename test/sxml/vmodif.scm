
(include "../../sxml-tools/tests/xtest-harness.sch")

;; Validation tests for SXML modification tool: "modif.scm"
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

(define vmdf:tree1
  '(html
    (head (title "Slides"))
    (body
     (p
      (@ (align "center"))
      (table
       (@ (style "font-size: x-large"))
       (tr
        (td (@ (align "right")) "Talks ")
        (td (@ (align "center")) " = ")
        (td " slides + transition"))
       (tr (td) (td (@ (align "center")) " = ") (td " data + control"))
       (tr (td) (td (@ (align "center")) " = ") (td " programs"))))
     (ul
      (li (a (@ (href "slides/slide0001.gif")) "Introduction"))
      (li (a (@ (href "slides/slide0010.gif")) "Summary"))))))
(define vmdf:tree2
  '(poem
    (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
    (stanza
      (line "Let us go then, you and I,")
      (line "When the evening is spread out against the sky")
      (line "Like a patient etherized upon a table:"))
    (stanza
      (line "In the room the women come and go")
      (line "Talking of Michaelangelo."))))


;=========================================================================
; Basic functionality of a modification tool

; (sxml:modify (quasiquote (/stanza/line[1] (unquote modif:delete))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (stanza
   (line "When the evening is spread out against the sky")
   (line "Like a patient etherized upon a table:"))
 (stanza (line "Talking of Michaelangelo.")))
; <--- of:
(sxml:modify `("/stanza/line[1]" ,modif:delete))
vmdf:tree2
)

; (sxml:modify (quasiquote (/stanza (unquote modif:delete-undeep))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (line "Let us go then, you and I,")
 (line "When the evening is spread out against the sky")
 (line "Like a patient etherized upon a table:")
 (line "In the room the women come and go")
 (line "Talking of Michaelangelo."))
; <--- of:
(sxml:modify `("/stanza" ,modif:delete-undeep))
vmdf:tree2
)

; (sxml:modify (quasiquote (/stanza (unquote (modif:rename (quote para))))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (para
  (line "Let us go then, you and I,")
  (line "When the evening is spread out against the sky")
  (line "Like a patient etherized upon a table:"))
 (para
  (line "In the room the women come and go")
  (line "Talking of Michaelangelo.")))
; <--- of:
(sxml:modify `("/stanza" ,(modif:rename 'para)))
vmdf:tree2
)

; (sxml:modify (quasiquote (/stanza[2] (unquote (modif:insert-into (lambda (context base-node) (quote (line Whatever))))))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (stanza
   (line "Let us go then, you and I,")
   (line "When the evening is spread out against the sky")
   (line "Like a patient etherized upon a table:"))
 (stanza
   (line "In the room the women come and go")
   (line "Talking of Michaelangelo.")
   (line "Whatever")))
; <--- of:
(sxml:modify
  `("/stanza[2]"
    ,(modif:insert-into (lambda (context base-node) '(line "Whatever")))))
vmdf:tree2
)

; (sxml:modify (quasiquote (/stanza[2]/line[last()] (unquote (modif:insert-following (lambda (context base-node) (quote (line Whatever))))))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (stanza
   (line "Let us go then, you and I,")
   (line "When the evening is spread out against the sky")
   (line "Like a patient etherized upon a table:"))
 (stanza
   (line "In the room the women come and go")
   (line "Talking of Michaelangelo.")
   (line "Whatever")))
; <--- of:
(sxml:modify
  `("/stanza[2]/line[last()]"
    ,(modif:insert-following (lambda (context base-node) '(line "Whatever")))))
vmdf:tree2
)

; (sxml:modify (quasiquote (@poet (unquote modif:delete))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock"))
 (stanza
   (line "Let us go then, you and I,")
   (line "When the evening is spread out against the sky")
   (line "Like a patient etherized upon a table:"))
 (stanza
   (line "In the room the women come and go")
   (line "Talking of Michaelangelo.")))
; <--- of:
(sxml:modify `("@poet" ,modif:delete))
vmdf:tree2
)

; (sxml:modify (quasiquote (/stanza[2] (unquote modif:delete))) (quasiquote (preceding-sibling::*[last()] (unquote (lambda (node context base-node) (list base-node node))))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (stanza
   (line "In the room the women come and go")
   (line "Talking of Michaelangelo."))
 (stanza
   (line "Let us go then, you and I,")
   (line "When the evening is spread out against the sky")
   (line "Like a patient etherized upon a table:")))
; <--- of:
(sxml:modify
  `("/stanza[2]" ,modif:delete)
  `("preceding-sibling::*[last()]"
    ,(lambda (node context base-node) (list base-node node))))
vmdf:tree2
)

; (sxml:modify (quasiquote (/stanza/line[1] (unquote modif:delete))) (quasiquote (/stanza (unquote (modif:rename (quote para))))) (quasiquote (/stanza[2]/line[last()] (unquote (modif:insert-following (lambda (context base-node) (quote (line Whatever))))))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (para
  (line "When the evening is spread out against the sky")
  (line "Like a patient etherized upon a table:"))
 (para (line "Talking of Michaelangelo.") (line "Whatever")))
; <--- of:
(sxml:modify
  `("/stanza/line[1]" ,modif:delete)
  `("/stanza" ,(modif:rename 'para))
  `("/stanza[2]/line[last()]"
    ,(modif:insert-following (lambda (context base-node) '(line "Whatever")))))
vmdf:tree2
)


;=========================================================================
; Testing with tree1

; (sxml:modify (quasiquote (//@* (unquote modif:delete))))
(xtest-assert ; Expected result:
'(html
 (head (title "Slides"))
 (body
  (p
   (table
    (tr (td "Talks ") (td " = ") (td " slides + transition"))
    (tr (td) (td " = ") (td " data + control"))
    (tr (td) (td " = ") (td " programs"))))
  (ul (li (a "Introduction")) (li (a "Summary")))))
; <--- of:
(sxml:modify `("//@*" ,modif:delete))
vmdf:tree1
)

; (sxml:modify (quasiquote (/head/title/text() (unquote modif:delete))) (quasiquote (/head/title (unquote (modif:insert-into (lambda (context base-node) (quote Presentation)))))))
(xtest-assert ; Expected result:
'(html
 (head (title "Presentation"))
 (body
  (p
   (@ (align "center"))
   (table
    (@ (style "font-size: x-large"))
    (tr
     (td (@ (align "right")) "Talks ")
     (td (@ (align "center")) " = ")
     (td " slides + transition"))
    (tr (td) (td (@ (align "center")) " = ") (td " data + control"))
    (tr (td) (td (@ (align "center")) " = ") (td " programs"))))
  (ul
   (li (a (@ (href "slides/slide0001.gif")) "Introduction"))
   (li (a (@ (href "slides/slide0010.gif")) "Summary")))))
; <--- of:
(sxml:modify
  `("/head/title/text()" ,modif:delete)
  `("/head/title"
    ,(modif:insert-into (lambda (context base-node) '"Presentation"))))
vmdf:tree1
)

; (sxml:modify (quasiquote (//text() (unquote modif:delete-undeep))))
(xtest-assert ; Expected result:
'(html
 (head (title))
 (body
  (p
   (@ (align "center"))
   (table
    (@ (style "font-size: x-large"))
    (tr (td (@ (align "right"))) (td (@ (align "center"))) (td))
    (tr (td) (td (@ (align "center"))) (td))
    (tr (td) (td (@ (align "center"))) (td))))
  (ul
   (li (a (@ (href "slides/slide0001.gif"))))
   (li (a (@ (href "slides/slide0010.gif")))))))
; <--- of:
(sxml:modify `("//text()" ,modif:delete-undeep))
vmdf:tree1
)

; (sxml:modify (quasiquote (//* (unquote modif:delete-undeep))))
(xtest-assert ; Expected result:
'(html
 "Slides"
 "Talks "
 " = "
 " slides + transition"
 " = "
 " data + control"
 " = "
 " programs"
 "Introduction"
 "Summary")
; <--- of:
(sxml:modify `("//*" ,modif:delete-undeep))
((sxml:modify `("//@*" ,modif:delete) `("//*" ,(modif:rename 'common))) vmdf:tree1)
)


;=========================================================================
; Abbreviated syntax

; (sxml:modify (quote (/stanza/line[1] delete)))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (stanza
   (line "When the evening is spread out against the sky")
   (line "Like a patient etherized upon a table:"))
 (stanza (line "Talking of Michaelangelo.")))
; <--- of:
(sxml:modify '("/stanza/line[1]" delete))
vmdf:tree2
)

; (sxml:modify (quote (/stanza delete-undeep)))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (line "Let us go then, you and I,")
 (line "When the evening is spread out against the sky")
 (line "Like a patient etherized upon a table:")
 (line "In the room the women come and go")
 (line "Talking of Michaelangelo."))
; <--- of:
(sxml:modify '("/stanza" delete-undeep))
vmdf:tree2
)

; (sxml:modify (quote (/stanza rename para)))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (para
  (line "Let us go then, you and I,")
  (line "When the evening is spread out against the sky")
  (line "Like a patient etherized upon a table:"))
 (para
  (line "In the room the women come and go")
  (line "Talking of Michaelangelo.")))
; <--- of:
(sxml:modify '("/stanza" rename para))
vmdf:tree2
)

; (sxml:modify (quote (/stanza[2] insert-into (line Whatever))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (stanza
   (line "Let us go then, you and I,")
   (line "When the evening is spread out against the sky")
   (line "Like a patient etherized upon a table:"))
 (stanza
   (line "In the room the women come and go")
   (line "Talking of Michaelangelo.")
   (line "Whatever")))
; <--- of:
(sxml:modify '("/stanza[2]" insert-into (line "Whatever")))
vmdf:tree2
)

; (sxml:modify (quote (/stanza[2]/line[last()] insert-following (line Whatever))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (stanza
   (line "Let us go then, you and I,")
   (line "When the evening is spread out against the sky")
   (line "Like a patient etherized upon a table:"))
 (stanza
   (line "In the room the women come and go")
   (line "Talking of Michaelangelo.")
   (line "Whatever")))
; <--- of:
(sxml:modify '("/stanza[2]/line[last()]" insert-following (line "Whatever")))
vmdf:tree2
)

; (sxml:modify (quote (@poet delete)))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock"))
 (stanza
   (line "Let us go then, you and I,")
   (line "When the evening is spread out against the sky")
   (line "Like a patient etherized upon a table:"))
 (stanza
   (line "In the room the women come and go")
   (line "Talking of Michaelangelo.")))
; <--- of:
(sxml:modify '("@poet" delete))
vmdf:tree2
)

; (sxml:modify (quote (/stanza[2] move-preceding preceding-sibling::*[last()])))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (stanza
   (line "In the room the women come and go")
   (line "Talking of Michaelangelo."))
 (stanza
   (line "Let us go then, you and I,")
   (line "When the evening is spread out against the sky")
   (line "Like a patient etherized upon a table:")))
; <--- of:
(sxml:modify '("/stanza[2]" move-preceding "preceding-sibling::*[last()]"))
vmdf:tree2
)

; (sxml:modify (quote (/stanza/line[1] delete)) (quote (/stanza rename para)) (quote (/stanza[2]/line[last()] insert-following (line Whatever))))
(xtest-assert ; Expected result:
'(poem
 (@ (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot"))
 (para
  (line "When the evening is spread out against the sky")
  (line "Like a patient etherized upon a table:"))
 (para (line "Talking of Michaelangelo.") (line "Whatever")))
; <--- of:
(sxml:modify
  '("/stanza/line[1]" delete)
  '("/stanza" rename para)
  '("/stanza[2]/line[last()]" insert-following (line "Whatever")))
vmdf:tree2
)


;=========================================================================
; Testing with tree1

; (sxml:modify (quote (//@* delete)))
(xtest-assert ; Expected result:
'(html
 (head (title "Slides"))
 (body
  (p
   (table
    (tr (td "Talks ") (td " = ") (td " slides + transition"))
    (tr (td) (td " = ") (td " data + control"))
    (tr (td) (td " = ") (td " programs"))))
  (ul (li (a "Introduction")) (li (a "Summary")))))
; <--- of:
(sxml:modify '("//@*" delete))
vmdf:tree1
)

; (sxml:modify (quote (/head/title/text() delete)) (quote (/head/title insert-into Presentation)))
(xtest-assert ; Expected result:
'(html
 (head (title "Presentation"))
 (body
  (p
   (@ (align "center"))
   (table
    (@ (style "font-size: x-large"))
    (tr
     (td (@ (align "right")) "Talks ")
     (td (@ (align "center")) " = ")
     (td " slides + transition"))
    (tr (td) (td (@ (align "center")) " = ") (td " data + control"))
    (tr (td) (td (@ (align "center")) " = ") (td " programs"))))
  (ul
   (li (a (@ (href "slides/slide0001.gif")) "Introduction"))
   (li (a (@ (href "slides/slide0010.gif")) "Summary")))))
; <--- of:
(sxml:modify
  '("/head/title/text()" delete)
  '("/head/title" insert-into "Presentation"))
vmdf:tree1
)

; (sxml:modify (quote (//text() delete-undeep)))
(xtest-assert ; Expected result:
'(html
 (head (title))
 (body
  (p
   (@ (align "center"))
   (table
    (@ (style "font-size: x-large"))
    (tr (td (@ (align "right"))) (td (@ (align "center"))) (td))
    (tr (td) (td (@ (align "center"))) (td))
    (tr (td) (td (@ (align "center"))) (td))))
  (ul
   (li (a (@ (href "slides/slide0001.gif"))))
   (li (a (@ (href "slides/slide0010.gif")))))))
; <--- of:
(sxml:modify '("//text()" delete-undeep))
vmdf:tree1
)

; (sxml:modify (quote (//* delete-undeep)))
(xtest-assert ; Expected result:
'(html
 "Slides"
 "Talks "
 " = "
 " slides + transition"
 " = "
 " data + control"
 " = "
 " programs"
 "Introduction"
 "Summary")
; <--- of:
(sxml:modify '("//*" delete-undeep))
((sxml:modify '("//@*" delete) '("//*" rename common)) vmdf:tree1)
)

(cout nl "SXML modification tests passed successfully!" nl)
