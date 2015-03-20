;;!!! URI parsing
;; .author Marc Feeley, 2005-2007
;; .author PerEckerdal, 2008-2009
;; .author Mikael More, 2012-2013
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/net/sack uri)
  (export make-uri
          clone-uri
          uri?
          uri-scheme
          uri-scheme-set
          uri-userinfo
          uri-userinfo-set
          uri-host
          uri-host-set
          uri-port
          uri-port-set
          uri-path
          uri-path-set
          uri-query
          uri-query-set
          uri-fragment
          uri-fragment-set
          uri-authority
          uri-authority-set
          uri-query-string:use-char-encoding-in-urlencoding
          uri:use-char-encoding-in-urldecoding
          uri-query-string
          uri-path&query->string
          parse-uri
          parse-uri-query
          string->uri
          uri->string
          string->uri-query
          encode-for-uri
          remove-dot-segments
          uri-join
          uri-join-strings
          urlencode-uripath)
  (import (spheres/string string) ;; string-index-right string-downcase
          (spheres/string u8vector) ;; utf8-u8vector->string string->utf8-u8vector
          (spheres/net/sack x-www-form-urlencoded))

  (include "uri.scm"))
