;;!!! SRFI-95
;; .author Richard A. O'Keefe (based on Prolog code by D.H.D.Warren) (2006)
;; .author Alvaro Castro-Castilla, 2014.

(define-library (spheres/algorithm sort-merge)
  (export sorted?
          merge
          merge!
          sort
          sort!)
  (include "sort-merge.scm"))
