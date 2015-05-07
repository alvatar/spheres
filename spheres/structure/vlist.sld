;;!!! This module provides an implementations of vlists, a functional list-like
;; data structure described by Phil Bagwell in "Fast Functional Lists,
;; Hash-Lists, Dequeues and Variable-Length Arrays", EPFL Technical Report,
;; 2002.
;; .author Ludovic Courtès, 2009-2012
;; .author Ivan Raikov, 2012
;; .author Alvaro Castro-Castilla, 2015
;; .license LGPL

(define-libary (spheres/structure vlist)
  (export vlist?
          vlist-cons
          vlist-head
          vlist-tail
          vlist-null?
          vlist-null
          list->vlist
          vlist-ref
          vlist-drop
          vlist-take
          vlist-length
          vlist-fold
          vlist-fold-right
          vlist-map
          vlist-unfold
          vlist-unfold-right
          vlist-append
          vlist-reverse
          vlist-filter
          vlist-delete
          vlist->list
          vlist-for-each
          block-growth-factor
          vhash?
          vhash-cons
          vhash-consq
          vhash-consv
          vhash-assoc
          vhash-assq
          vhash-assv
          vhash-delete
          vhash-delq
          vhash-delv
          vhash-fold
          vhash-fold-right
          vhash-fold*
          vhash-foldq*
          vhash-foldv*
          alist->vhash)

  (import (spheres/core functional)
          (spheres/algorithm list)
          (spheres/structure hash-table))

  (include "vlist.scm"))
