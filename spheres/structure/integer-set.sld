;;!!! Integer Sets
;; .author Alex Shinn, 2004-2006
;; .author Alvaro Castro-Castilla
;; Copyright (c) 2004-2006 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; An integer set is a set of exact integers optimized for minimal space
;; usage and fast membership lookup.  General set operations are
;; provided based on the character set operations found in SRFI-14.
;;
;; Creating isets:
;;
;; (make-iset)     ; an empty integer set
;; (make-iset n)   ; a set of the single integer N
;; (make-iset n m) ; a set of the range of all integers from N-M inclusive
;;
;; The following procedures are provided as direct analogs of the
;; SRFI-14 procedures, accepting and returning isets and integers in
;; place of char-sets and characters:
;;
;; Creating isets:
;;
;; (iset-copy is)            ; a new copy of IS
;; (iset n ...)              ; an iset containing the elements N...
;; (list->iset ls [base-is]) ; an iset containing all the integers in
;;                           ; list LS, union BASE-IS if provided
;; (list->iset! ls base-is)  ; same as above, allowed but not required to
;;                           ; modify base-is
;;
;; Querying isets:
;;
;; (iset-size is)          ; return the # of elements in IS
;; (iset-contains? is n)   ; test N for membership in IS
;; (iset->list is)         ; returns a list of all integers in IS
;; (iset-any pred is)      ; apply PRED to every element of IS, returning
;;                         ; the first element it finds (order unspecified)
;; (iset-every pred is)    ; returns #t if every element of IS satisfies
;;                         ; the predicate PRED (order unspecified)
;;
;; Predicates:
;;
;; (iset? obj)     ; #t iff obj is an integer set
;; (iset= is ...)  ; #t iff all arguments are equivalent integer sets
;; (iset<= is ...) ; #t iff the arguments are monotonically increasing sets
;; (iset>= is ...) ; #t iff the arguments are monotonically decreasing sets
;;
;; Iteration:
;;
;; (iset-fold kons knil is)       ; char-set-fold
;; (iset-unfold f p g [base-is])  ; char-set-unfold
;; (iset-unfold! f p g base-is)   ; char-set-unfold!
;; (iset-for-each proc is)        ; char-set-for-each
;; (iset-map proc is)             ; char-set-for-each
;; (iset-filter pred is [bas-is]) ; char-set-filter
;; (iset-filter! pred is base-is) ; char-set-filter!
;;
;; Cursors:
;;
;; (iset-cursor iset)
;; (iset-ref iset cursor)
;; (iset-cursor-next iset cursor)
;; (end-of-iset? iset)
;;
;; Set operations:
;;
;; (iset-adjoin is n ...)         ; char-set-adjoin
;; (iset-delete is n ...)         ; char-set-delete
;;
;; (iset-adjoin! is n ...)        ; char-set-adjoin!
;; (iset-delete! is n ...)        ; char-set-delete!
;;
;; (iset-union is1 ...)                  ; char-set-union
;; (iset-intersection is1 ...)           ; char-set-intersection
;; (iset-difference is1 is2 ...)         ; char-set-difference
;; (iset-xor is1 ...)                    ; char-set-xor
;; (iset-diff+intersection is1 is2 ...)  ; char-set-diff+intersection
;;
;; (iset-union! is1 ...)                 ; char-set-union!
;; (iset-intersection! is1 ...)          ; char-set-intersection!
;; (iset-difference! is1 is2 ...)        ; char-set-difference!
;; (iset-xor! is1 ...)                   ; char-set-xor!
;; (iset-diff+intersection! is1 is2 ...) ; char-set-diff+intersection!

(define-library (spheres/structure integer-set)
  (export make-iset
          iset
          iset?
          iset-copy
          list->iset
          list->iset!
          iset->list
          iset=
          iset<=
          iset>=
          iset-start
          iset-end
          iset-bits
          iset-left
          iset-right
          set-iset-start!
          set-iset-end!
          set-iset-bits!
          set-iset-left!
          set-iset-right!
          iset-empty?
          iset-contains?
          iset-adjoin
          iset-adjoin!
          iset-delete
          iset-delete!
          iset-cursor
          iset-ref
          iset-cursor-next
          end-of-iset?
          iset-fold
          iset-unfold
          iset-unfold!
          iset-for-each
          iset-map
          iset-filter
          iset-filter!
          iset-every
          iset-any
          iset-size
          iset-union!
          iset-union
          iset-intersection!
          iset-intersection
          iset-difference!
          iset-difference
          iset-xor!
          iset-xor
          iset-diff+intersection!
          iset-diff+intersection
          ;; low-level utilities
          %make-iset
          iset-dump
          iset-write-code
          iset-balance
          iset-balance!
          iset-optimize
          iset-optimize!)

  (import (spheres/core base)
          (spheres/algorithm u8vector)
          (spheres/structure bit-vector))

  (include "integer-set.scm"))
