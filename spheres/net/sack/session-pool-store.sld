;;!!! Sack session store that stores the session data in memory
;; .author Per Eckerdal, 2008-2009
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/net/sack session-pool-store)
  (export make-session-pool
          pool-session-store)

  (import (spheres/net/sack session-cookie-id-based-store))

  (include "session-pool-store.scm"))
