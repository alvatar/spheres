;;!!! LRU Cache
;; .author Jim Ursetto, 2009
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure lru-cache)
  (export make-lru-cache
          lru-cache-ref
          lru-cache-set!
          lru-cache-walk
          lru-cache-fold
          lru-cache-delete!
          lru-cache-flush!
          lru-cache-size
          lru-cache-capacity)
  (import (spheres/core base)
          (spheres/structure hash-table))

  (include "lru-cache.scm"))
