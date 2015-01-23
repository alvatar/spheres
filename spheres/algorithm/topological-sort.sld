;;! Topological sort
;; .author Mikael Djurfeldt, Copyright (C) 1995
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/algorithm topological-sort)
  (export topological-sort)
  (import (spheres/math arithmetic))

  (include "topological-sort.scm"))
