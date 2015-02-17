;;!!! HtmlPrag provides permissive HTML parsing and emitting capability to Scheme

(define-library (spheres/markup html)
  (export make-html-tokenizer
          tokenize-html
          shtml-token-kind
          parse-html/tokenizer
          html->sxml-0nf
          html->sxml-1nf
          html->sxml-2nf
          html->sxml
          html->shtml
          write-shtml-as-html
          shtml->html
          sxml->html)

  (include "html.scm"))
