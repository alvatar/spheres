;;!!! URI handling
;; .author Marc Feeley, 2005-2008
;; .author Marco Benelli, 2010-2011
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/net uri)
  (export make-uri
          uri?
          uri-authority
          uri-authority-set!
          uri-fragment
          uri-fragment-set!
          uri-path
          uri-path-set!
          uri-query
          uri-query-set!
          uri-scheme
          uri-scheme-set!

          string->uri
          string->uri-query
          encode-for-uri

          encode-x-www-form-urlencoded
          decode-x-www-form-urlencoded)

  (include "uri.scm"))
