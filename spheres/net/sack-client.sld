(define-library (spheres/net sack-client)
  (export http-connect
          simple-raw-http-request
          simple-raw-http-request-resp
          http-client-response-if-successful)
  (import (spheres/algorithm list)
          (spheres/structure a-list)
          (spheres/structure token-table)
          (spheres/string string)
          (spheres/string u8vector)
          (spheres/net/sack io-primitives)
          (spheres/net/sack http-util)
          (spheres/net/sack uri)
          (spheres/net/sack x-www-form-urlencoded))

  (include "sack-client.scm"))
