;;!!! A functional balanced tree data structure, with a rather low level
;; interface. It is intended to be used as a base to implement data
;; structures like maps, sets and priority queues. It can obviously
;; also be used to implement sorting, removal of duplicate elements in
;; lists and things like that. The implementation is based on the
;; algorithms described in
;; http://groups.csail.mit.edu/mac/users/adams/BB/
;;
;; .author Per Eckerdal, 2010
;; .author Alvaro Castro-Castilla, 2015

;; A function whose name begins with %% is unsafe; it doesn't check
;; its arguments. It might or might not segfault, but other functions
;; might do it later on since the data structure can become bogus
;; unless you give it proper arguments. They are not exported, so
;; those issues are taken care of internally.
;;
;; Copyright (c) 2010 Per Eckerdal
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


(define-library (spheres/structure bb-tree)
  (export bb-tree?
          empty-bb-tree
          bb-tree-size
          bb-tree-min
          bb-tree-max
          bb-tree-fold
          bb-tree-fold-from
          bb-tree-backwards-fold-from

          bb-tree-search
          bb-tree-member?

          bb-tree-add
          bb-tree-delete
          bb-tree-delete-min
          bb-tree-delete-max

          bb-tree-split<
          bb-tree-split>
          bb-tree-union
          bb-tree-difference
          bb-tree-intersection

          bb-tree-rank
          bb-tree-index

          list->bb-tree
          bb-tree->list)

  (include "bb-tree.scm"))
