(define-library (spheres/net/sack session-cookie-id-based-store)
  (export default-session-generate-id
          default-session-cookie-name
          cookie-id-based-session-store)

  (import (spheres/net uuid)
          (spheres/net/sack cookie)
          ;; (spheres/net/sack session)
          )

  (include "session-cookie-id-based-store.scm"))
