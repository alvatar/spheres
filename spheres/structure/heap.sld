;;!!! Mutable heap with priority-queue operations and O(1) membership-testing
;; .author Peter Danenberg, 2015
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/structure heap)
  (export initial-heap-size
          ;; build-heap!
          ;; heap-data
          make-heap
          make-max-heap
          make-min-heap
          heap->alist
          heap->list
          heap-change-key!
          heap-delete!
          heap-empty?
          heap-extract-extremum!
          heap-extremum
          heap-insert!
          heap-key
          heap-member?
          ;; heap-membership
          ;; heapify!
          heap-size)

  (import (spheres/core base)
          (spheres/structure hash-table))

  (include "heap.scm"))
