;;!!! SXML tools
;; SXML is XML Infoset represented as native Scheme data - S-expressions.
;; Any Scheme program can manipulate SXML data directly, and DOM-like API is not
;; necessary for SXML/Scheme applications.
;; SXML-tools (former DOMS) is just a set of handy functions which may be
;; convenient for some popular operations on SXML data.

(define-library (spheres/markup sxml-tools)
  (export sxml:attr-list-node
          sxml:attr-as-list
          sxml:aux-list-node
          sxml:aux-as-list
          sxml:error
          sxml:empty-element?
          sxml:shallow-normalized?
          sxml:normalized?
          sxml:shallow-minimized?
          sxml:minimized?
          sxml:name
          sxml:element-name
          sxml:node-name
          sxml:ncname
          sxml:name->ns-id
          sxml:content
          sxml:text
          sxml:content-raw
          sxml:attr-list-u
          sxml:aux-list
          sxml:aux-list-u
          sxml:aux-node
          sxml:aux-nodes
          sxml:attr
          sxml:attr-from-list
          sxml:num-attr
          sxml:attr-u
          sxml:ns-list
          sxml:ns-id->nodes
          sxml:ns-id->uri
          sxml:ns-uri->nodes
          sxml:ns-uri->id
          sxml:ns-id
          sxml:ns-uri
          sxml:ns-prefix
          sxml:change-content!
          sxml:change-content
          sxml:change-attrlist
          sxml:change-attrlist!
          sxml:change-name!
          sxml:change-name
          sxml:add-attr
          sxml:add-attr!
          sxml:change-attr
          sxml:change-attr!
          sxml:set-attr
          sxml:set-attr!
          sxml:add-aux
          sxml:add-aux!
          sxml:squeeze!
          sxml:squeeze
          sxml:clean
          sxml:select-first-kid
          sxml:node-parent
          sxml:add-parents
          sxml:lookup
          sxml:attr->xml
          sxml:string->xml
          sxml:sxml->xml
          sxml:attr->html
          sxml:string->html
          sxml:non-terminated-html-tag?
          sxml:sxml->html)

  (import (spheres/core base)
          (spheres/string string))

  (include "internal/sxpath-lib.scm")
  (include "internal/sxml-tools.scm"))
