;;!!! Cookie handling module for the Sack web server middleware
;; .author Per Eckerdal, 2008-2009
;; .author Mikael More, 2012-2013
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/net/sack cookie)
  (export
   ;; # Cookie Sack app adapters
   ;; Generally recommended form
   cookies->
   ;; Form recommended to use if every HTTP request both gets and sets cookies.
   cookies->/notlazy
   ;; Cookie object constructor, typechecker, and accessors
   make-cookie
   cookie?
   cookie-name
   cookie-value
   cookie-value-set!
   cookie-expires
   cookie-expires-set!
   cookie-domain
   cookie-domain-set!
   cookie-path
   cookie-path-set!
   cookie-port
   cookie-port-set!
   cookie-secure
   cookie-secure-set!
   cookie-http-only
   cookie-http-only-set!
   ;; For testing, cookie object <-> HTTP header
   ;; Deserializes string in the raw form provided in the Cookie: header in HTTP requests to a list of cookie objects.
   cookie-parse-to-list
   ;; Takes a cookie object and serializes it to the format output in Set-Cookie: HTTP response headers.
   cookie-to-http
   date-in-the-past)

  (import (spheres/os date-time)
          (spheres/string string)
          (spheres/string string-extra)
          (spheres/net/sack http-util) ;; string-strip string-split-char
          (spheres/net/sack x-www-form-urlencoded))

  (include "cookie.scm"))
