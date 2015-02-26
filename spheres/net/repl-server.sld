;;!!! REPL server which can be contacted via telnet on port 7000.
;; .author Marc Feeley, 2011
;;
;; Copyright (c) 2011 by Marc Feeley, All Rights Reserved.

(define-library (spheres/net repl-server)
  (export repl-server-start)

  (include "repl-server.scm"))
