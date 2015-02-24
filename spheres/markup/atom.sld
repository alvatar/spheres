;;!!! Atom Syndication Format
;; .author Jim Ursetto
;; .link http://wiki.call-cc.org/eggref/4/atom

(define-library (spheres/markup atom)

  (export atom-doc-encoding
          atom-doc-root
          atom-ns-prefixes
          author?
          author-email
          author-name
          author-uri
          category?
          category-contents
          category-label
          category-scheme
          category-term
          content?
          content-contents
          content-kind
          content-source
          content-text
          content-type
          content-xhtml
          content-xml
          contributor?
          contributor-email
          contributor-name
          contributor-uri
          entry?
          entry-author
          entry-authors
          entry-categories
          entry-content
          entry-contributors
          entry-id
          entry-links
          entry-published
          entry-rights
          entry-source
          entry-summary
          entry-title
          entry-updated
          entry=?
          feed?
          feed-author
          feed-authors
          feed-categories
          feed-contributors
          feed-entries
          feed-generator
          feed-icon
          feed-id
          feed-links
          feed-logo
          feed-rights
          feed-subtitle
          feed-title
          feed-updated
          generator?
          generator-agent
          generator-uri
          generator-version
          icon?
          icon-uri
          id=?
          link?
          link-contents
          link-length
          link-relation
          link-title
          link-type
          link-uri
          link-uri-language
          logo?
          logo-uri
          make-atom-doc
          make-author
          make-category
          make-content
          make-contributor
          make-entry
          make-feed
          make-generator
          make-icon
          make-link
          make-logo
          make-rights
          make-source
          make-subtitle
          make-summary
          make-title
          read-atom-doc
          read-atom-entry
          read-atom-feed
          rights?
          rights-text
          rights-type
          rights-xhtml
          source?
          source-author
          source-authors
          source-categories
          source-contributors
          source-generator
          source-icon
          source-id
          source-links
          source-logo
          source-rights
          source-subtitle
          source-title
          source-updated
          subtitle?
          subtitle-text
          subtitle-type
          subtitle-xhtml
          summary?
          summary-text
          summary-type
          summary-xhtml
          title?
          title-text
          title-type
          title-xhtml
          write-atom-doc)

  (import (spheres/core base)
          (spheres/core match)
          (spheres/algorithm list)
          (spheres/string string)
          (spheres/string regexp)
          (spheres/markup sxpath-context-xlink)
          (spheres/markup sxml-serializer))

  (include "atom.scm"))
