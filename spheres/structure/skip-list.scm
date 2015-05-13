;;!!! Skip Lists
;; .author Taylor R. Campbell, 2007-2009
;;
;; Copyright (c) 2007-2009, Taylor R. Campbell
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; * Neither the names of the authors nor the names of contributors
;;   may be used to endorse or promote products derived from this
;;   software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; The skip list is a probabilistic data structure comparable to
;; balanced binary trees.  Skip lists are introduced in
;;
;;   William Pugh, `Skip Lists: A Probabilistic Alternative to
;;   Balanced Trees', Algorithms and Data Structures: Workshop WADS,
;;   Ottawa, Springer-Verlag Lecture Notes in Computer Science, 1990,
;;
;; if you read past the scorn Pugh heaps upon balanced binary trees,
;; which are perfectly respectable data structures: red/black trees
;; and bounded-balance (weight-balanced) trees tend to perform better
;; than skip lists, and require less storage.  Skip lists tend to use
;; fewer comparisons than red/black trees, can peek at the minimum
;; element in constant time, can delete the minimum element in
;; logarithmic time but with good constant factors, and can be merged
;; in slightly better time than bounded-balance trees; see
;;
;;   William Pugh, `A Skip List Cookbook', TR 2286, Department of
;;   Computer Science, University of Maryland, 1989.
;;
;; This implementation does not admit concurrent update as in
;;
;;   William Pugh, `Concurrent Maintenance of Skip Lists', TR 2222,
;;   Department of Computer Science, University of Maryland, 1989.
;;
;; This code depends on SRFIs 9 (DEFINE-RECORD-TYPE), 23 (ERROR), and
;; 27 (random number generation).  The dependency on SRFI 27 could be
;; elided with a suitable replacement for the definition of FLIP-COIN,
;; at the bottom of the file.
;;
;; This code is permanently stored at
;;
;;   <http://mumble.net/~campbell/scheme/skip-list.scm>.

;; Documentation

;; A skip list is a collection of associations between keys and data:
;; with each key in the skip list, there is one associated datum.
;; Associated with each skip list is a skip list type, representing a
;; total order on keys of skip lists of that type.  Total orders can
;; be expressed using key order predicates or key comparators.  An
;; order predicate is a procedure of two arguments that returns true
;; if the first is less than the second, and false if not.  A
;; comparator is a procedure of two arguments that returns -1 if the
;; first is less than the second, 0 if they are equal, and +1 if the
;; first is greater than the second.  Two keys are equivalent if they
;; are equivalent under the total order on them, i.e. neither is less
;; than the other.
;;
;; Except where otherwise specified, every procedure here runs in
;; constant time, ignoring the time taken by garbage collection.
;; Where running time is specified, n is the context-dictated size of
;; the input; e.g., if the input is a list, n is its length; if the
;; input is a skip list, n is its count, i.e. its number of
;; associations.  Some operations are expected to run faster than they
;; run in the worst case, in which case their running time is
;; specified as expected running time; otherwise, the running time is
;; specified as running time.  For higher-order procedures, such as
;; SKIP-LIST/INTERN!, the running time of the procedures supplied as
;; arguments is ignored.
;;
;;   (MAKE-SKIP-LIST-TYPE/COMPARATOR <key-comparator>) -> skip-list-type
;;   (MAKE-SKIP-LIST-TYPE/ORDER-PREDICATE <key-order-predicate>)
;;       -> skip-list-type
;;     Constructors for skip list types.  <Key-comparator> must be a
;;     comparator as described above; <key-order-predicate>, an order
;;     predicate.
;;
;;   (SKIP-LIST-TYPE? <object>) -> boolean
;;     Disjoint type predicate for skip list types.
;;
;;   (SKIP-LIST-TYPE/KEY-COMPARATOR <skip-list-type>) -> key-comparator
;;   (SKIP-LIST-TYPE/KEY-ORDER-PREDICATE <skip-list-type>)
;;       -> key-order-predicate
;;     Accessors for the key comparator and key order predicate
;;     procedures of skip list types.

;; Skip List Operations

;;   (MAKE-SKIP-LIST <skip-list-type> [<max-level>]) -> skip-list
;;     Skip list constructor.  <Skip-list-type> must be a skip list
;;     type, and <max-level> must be a non-negative integer, which
;;     indicates to what depth the skip list is allowed to expand.
;;     The default is 32, which suffices for four billion keys to be
;;     accessible on average with fewer than 32 key comparisons.
;;
;;   (SKIP-LIST? <object>) -> boolean
;;     Disjoint type predicate for skip lists.
;;
;;   (SKIP-LIST/TYPE <skip-list>) -> skip-list-type
;;     Returns <skip-list>'s type.
;;
;;   (SKIP-LIST/COUNT <skip-list>) -> exact, non-negative integer
;;     Returns the number of associations in <skip-list>.
;;
;;   (SKIP-LIST/EMPTY? <skip-list>) -> boolean
;;     Returns true if <skip-list> has no associations, or false if it
;;     has any.
;;
;;   (ALIST->SKIP-LIST <alist> <skip-list-type>) -> skip-list
;;   (SKIP-LIST->ALIST <skip-list>) -> alist
;;     <Alist> must be a list (not necessarily sorted) of pairs each
;;     representing an association of a datum in its cdr with a key,
;;     appropriate for <skip-list-type>, in its car.  ALIST->SKIP-LIST
;;     returns a skip list with the associations in <alist>.
;;     SKIP-LIST->ALIST returns a sorted alist, in ascending order of
;;     keys, of the associations in <skip-list>.
;;
;;     Expected running time of ALIST->SKIP-LIST: O(n log n).  Running
;;     time of SKIP-LIST->ALIST: Theta(n).
;;
;;   (SKIP-LIST/KEY-LIST <skip-list>) -> key list
;;     Returns a sorted list of the keys in <skip-list>, in ascending
;;     order.
;;
;;     Running time: Theta(n).
;;
;;   (SKIP-LIST/DATUM-LIST <skip-list>) -> datum list
;;     Returns a sorted list of the data in <skip-list>, in ascending
;;     order of the keys with which they were associated.
;;
;;     Running time: Theta(n).

;; Set Operations

;;   (SKIP-LIST/LOOKUP <skip-list> <key> <default>) -> datum
;;     Returns the datum in <skip-list> associated with <key>, or
;;     <default> if <skip-list> has no association for <key>.
;;
;;     Unlike some lookup operations, <default> must be provided;
;;     there is no default <default> of #F or anything.  This avoids
;;     the common error of accidentally preventing sets from
;;     containing #F, or another sentinel value, by relying on a
;;     default <default>.  Requiring programmers to supply <default>
;;     forces them to acknowledge what their out-of-band sentinels
;;     are, if <default> is being used as such.  To take different
;;     actions depending on whether an association is found without
;;     the use of sentinels, use SKIP-LIST/SEARCH below.
;;
;;     Expected running time: O(log n).
;;
;;   (SKIP-LIST/MEMBER? <skip-list> <key>) -> boolean
;;     Returns true if <skip-list> has an association for <key>, and
;;     false if not.  (This is a terrible name.  Formerly this was
;;     called SKIP-LIST/CONTAINS-KEY?, which is not much better.  How
;;     about SKIP-LIST/KEY? instead?)
;;
;;     Expected running time: O(log n).
;;
;;   (SKIP-LIST/INSERT! <skip-list> <key> <datum>)
;;     Inserts an association of <datum> with <key> in <skip-list>.
;;     If <skip-list> already contains an association for <key>,
;;     SKIP-LIST/INSERT! substitutes <datum> for its datum.
;;
;;     Expected running time: O(log n).
;;
;;   (SKIP-LIST/MODIFY! <skip-list> <key> <default> <modifier>) -> datum
;;     If <skip-list> has an association for <key> whose datum is
;;     <datum>, SKIP-LIST/MODIFY! replaces <datum> by (<modifier>
;;     <datum>); otherwise, SKIP-LIST/MODIFY! inserts an association
;;     for <key> with (<modifier> <default>).  In either case,
;;     SKIP-LIST/MODIFY! returns the datum returned by <modifier>.
;;
;;     Expected running time: O(log n).
;;
;;   (SKIP-LIST/INTERN! <skip-list> <key> <generator>) -> datum
;;     If <skip-list> has an association for <key>, returns the datum
;;     associated with it.  Otherwise, calls (<generator> <key>) to
;;     obtain a datum, inserts an association for <key> with the datum
;;     into <skip-list>, and returns the datum.
;;
;;     Expected running time: O(log n).
;;
;;   (SKIP-LIST/DELETE! <skip-list> <key>)
;;     Deletes the association for <key> in <skip-list>, if there is
;;     one; does nothing if there is no such association.
;;
;;     Expected running time: O(log n).

;; Ordered Set Operations

;;   (SKIP-LIST/MIN <skip-list> <default>) -> key or <default>
;;     Returns the least key in <skip-list>, or <default> if
;;     <skip-list> is empty.
;;
;;     Running time: O(1).
;;
;;   (SKIP-LIST/MIN-DATUM <skip-list> <default>) -> datum or <default>
;;     Returns the datum associated with the least key in <skip-list>,
;;     or <default> if <skip-list> is empty.
;;
;;     Running time: O(1).
;;
;;   (SKIP-LIST/MIN-PAIR <skip-list>) -> pair or #F
;;     Returns a pair whose car is the least key in <skip-list> and
;;     whose cdr is its associated datum, or #F if <skip-list> is
;;     empty.
;;
;;     Running time: O(1).
;;
;;   (SKIP-LIST/DELETE-MIN! <skip-list> <default>) -> key or <default>
;;   (SKIP-LIST/DELETE-MIN-DATUM! <skip-list> <default>) -> datum or <default>
;;   (SKIP-LIST/DELETE-MIN-PAIR! <skip-list>) -> pair or #F
;;     Like SKIP-LIST/MIN, SKIP-LIST/MIN-DATUM, & SKIP-LIST/MIN-PAIR,
;;     respectively, but also delete the association for the least key
;;     in <skip-list>.
;;
;;     Expected running time: O(log n).
;;
;;   (SKIP-LIST/UPDATE-MIN! <skip-list> <if-found> <if-empty>)
;;     If <skip-list> is non-empty, this is like SKIP-LIST/UPDATE!
;;     (see below), but with the association for the least key in
;;     <skip-list>, rather than the association for with a given key.
;;     If <skip-list> is empty, this calls <if-empty> in a tail
;;     position with zero arguments.  Example:
;;
;;       (define (skip-list/delete-min! skip-list default)
;;         (skip-list/update-min! skip-list
;;           (lambda (key datum replace delete)
;;             replace datum           ;ignore
;;             (delete)
;;             key)
;;           (lambda () default)))
;;
;;     Running time: O(1) initially, O(1) to replace, and O(log n)
;;     expected time to delete.  Only one of the procedures REPLACE
;;     and DELETE may be used, and only once, however.

;;   (SKIP-LIST/MAX <skip-list> <default>) -> key or <default>
;;   (SKIP-LIST/MAX-DATUM <skip-list> <default>) -> datum or <default>
;;   (SKIP-LIST/MAX-PAIR <skip-list>) -> pair or #F
;;     Like SKIP-LIST/MIN, SKIP-LIST/MIN-DATUM, & SKIP-LIST/MIN-PAIR,
;;     respectively, but with the association for the greatest key,
;;     rather than the association for the least key.  Note that,
;;     unlike the minimum access procedures, these run in logarithmic
;;     expected time, not in constant time in the worst case.
;;
;;     Expected running time: O(log n).
;;
;;   (SKIP-LIST/DELETE-MAX! <skip-list> <default>) -> key or <default>
;;   (SKIP-LIST/DELETE-MAX-DATUM! <skip-list> <default>) -> datum or <default>
;;   (SKIP-LIST/DELETE-MAX-PAIR! <skip-list>) -> pair or #F
;;     Like the minimum deletion procedures, but for the maximum.
;;
;;     Expected running time: O(log n).
;;
;;   (SKIP-LIST/UPDATE-MAX! <skip-list> <if-found> <if-empty>)
;;     Like SKIP-LIST/UPDATE-MIN!, but for the maximum.
;;
;;     Running time: O(log n) expected time initially, O(1) time to
;;     replace, O(log n) expected time to delete.  As with
;;     SKIP-LIST/UPDATE-MIN!, at most one of the replacement and
;;     deletion procedure may be called, at most once.
;;
;;   (SKIP-LIST/SPLIT<! <skip-list> <key>) -> [skip-list skip-list]
;;   (SKIP-LIST/SPLIT>=! <skip-list> <key>) -> [skip-list skip-list]
;;   (SKIP-LIST/SPLIT>! <skip-list> <key>) -> [skip-list skip-list]
;;   (SKIP-LIST/SPLIT<=! <skip-list> <key>) -> [skip-list skip-list]
;;     These operations split <skip-list> into two skip lists,
;;     destroying the original one.  The keys of the first skip list
;;     are respectively all less than, not less than, greater than, or
;;     not greater than <key>.
;;
;;     Running time: Theta(n).  However, the expected number of key
;;     comparisons is O(log n).  (Why?  These recompute the count, so
;;     that SKIP-LIST/COUNT runs in constant time.  Don't like it?
;;     Use bounded-balance (weight-balanced) binary trees instead.)
;;
;;   (SKIP-LIST/UNION! <skip-list-a> <skip-list-b>) -> skip-list
;;   (SKIP-LIST/LEFT-UNION! <skip-list-a> <skip-list-b>) -> skip-list
;;   (SKIP-LIST/RIGHT-UNION! <skip-list-a> <skip-list-b>) -> skip-list
;;   (SKIP-LIST/UNION-MERGE! <skip-list-a> <skip-list-b> <merger>)
;;       -> skip-list
;;     Each of these operations returns a skip list whose set of keys
;;     is the union of the set of keys in <skip-list-a> and the set of
;;     keys in <skip-list-b>.  The operations differ on what datum is
;;     associated with each key in the resulting skip list for keys
;;     for which both input skip lists have associations:
;;
;;     . UNION and LEFT-UNION choose the datum in <skip-list-a>.
;;     . RIGHT-UNION chooses the datum in <skip-list-b>>
;;     . UNION-MERGE! uses the value of (<merger> <key> <datum-a>
;;       <datum-b>), where <datum-a> is <key>'s associated datum in
;;       <skip-list-a>, and <datum-b> its datum in <skip-list-b>.
;;
;;     Example:
;;
;;     (define (skip-list/left-union! skip-list-a skip-list-b)
;;       (skip-list/union-merge! skip-list-a skip-list-b
;;         (lambda (key datum-a datum-b) datum-a)))
;;
;;     Expected running time: O(m + m log (n/m)).

;; Primitive Operations

;; The above operations on skip list are all implemented in terms of
;; the following general skip list operations:
;;
;;   (SKIP-LIST/SEARCH <skip-list> <key> <if-found> <if-not-found>)
;;     Searches in <skip-list> for an association for <key>.  If
;;     <skip-list> has such an association, SKIP-LIST/SEARCH calls the
;;     procedure <if-found> in a tail position with the associated
;;     datum as its sole argument; otherwise, it calls the procedure
;;     <if-not-found> in a tail position with zero arguments.  For
;;     example, SKIP-LIST/LOOKUP could have been defined in terms of
;;     SKIP-LIST/SEARCH like so:
;;
;;       (define (skip-list/lookup skip-list key default)
;;         (skip-list/search skip-list key
;;           (lambda (datum) datum)
;;           (lambda () default)))
;;
;;     Expected running time: O(log n).
;;
;;   (SKIP-LIST/UPDATE! <skip-list> <key> <if-found> <if-not-found>)
;;     Searches in <skip-list> for an association for <key>, building
;;     up the necessary internal data structures for updating the
;;     structure of <skip-list>.  If <skip-list> has such an
;;     association, SKIP-LIST/UPDATE! calls the procedure <if-found>
;;     in a tail position with three arguments:
;;
;;        1. The datum associated with <key>.
;;        2. A unary procedure to replace the datum with another one:
;;           (REPLACE <datum>).
;;        3. A nullary procedure to delete the association: (DELETE).
;;
;;     If <skip-list> has no such association, SKIP-LIST/UPDATE! calls
;;     <if-not-found> in a tail position with one argument, a unary
;;     procedure to insert an association for the key in the skip
;;     list: (INSERT <datum>).
;;
;;     For example, SKIP-LIST/INTERN! could have been defined like so:
;;
;;       (define (skip-list/intern! skip-list key generator)
;;         (skip-list/update! skip-list key
;;           (lambda (datum replace delete)
;;             replace delete          ;ignore
;;             datum)
;;           (lambda (insert)
;;             (let ((datum (generator key)))
;;               (insert datum)
;;               datum))))
;;
;;     Expected running time: O(log n).  REPLACE runs in O(1) time,
;;     and DELETE and INSERT in O(log n) expected time, but in a
;;     single use of SKIP-LIST/UPDATE!, only one of the three
;;     procedures can be used, and only once.

;; Built-In Skip List Types

;;   SKIP-LIST-TYPE:REAL-NUMBER
;;     Skip list type for keys of arbitrary real numbers -- any
;;     object on which the `<' procedure is defined, which by the
;;     R5RS is any object satisfying the REAL? predicate.
;;
;;   SKIP-LIST-TYPE:EXACT-INTEGER
;;     Like above, but restricted to exact integers, for efficiency.
;;     These exact integers are not limited to a particular range.
;;
;;   SKIP-LIST-TYPE:SYMBOL
;;     Skip list type for keys of symbols, ordered by STRING<? on
;;     their names, as obtained using SYMBOL->STRING.
;;
;;   SKIP-LIST-TYPE:STRING
;;   SKIP-LIST-TYPE:STRING-CI
;;     Skip list type for keys of strings, ordered by STRING<? or
;;     STRING-CI<?, respectively.

;; Skip List Structure

(define-record-type <skip-list-type>
  (%make-skip-list-type key-comparator
                        key-order-predicate
                        operation/search
                        operation/lookup
                        operation/member?
                        operation/update!
                        operation/insert!
                        operation/modify!
                        operation/intern!
                        operation/delete!
                        operation/split<!
                        operation/split>!
                        operation/left-union!
                        operation/right-union!
                        operation/union-merge!)
  skip-list-type?
  (key-comparator skip-list-type.key-comparator)
  (key-order-predicate skip-list-type.key-order-predicate)
  (operation/search skip-list-type.operation/search)
  (operation/lookup skip-list-type.operation/lookup)
  (operation/member? skip-list-type.operation/member?)
  (operation/update! skip-list-type.operation/update!)
  (operation/insert! skip-list-type.operation/insert!)
  (operation/modify! skip-list-type.operation/modify!)
  (operation/intern! skip-list-type.operation/intern!)
  (operation/delete! skip-list-type.operation/delete!)
  (operation/split<! skip-list-type.operation/split<!)
  (operation/split>! skip-list-type.operation/split>!)
  (operation/left-union! skip-list-type.operation/left-union!)
  (operation/right-union! skip-list-type.operation/right-union!)
  (operation/union-merge! skip-list-type.operation/union-merge!))

(define-record-type <skip-list>
  (%make-skip-list type header level count)
  skip-list?
  (type skip-list.type)
  (header skip-list.header set-skip-list.header!)
  (level skip-list.level set-skip-list.level!)
  (count skip-list.count set-skip-list.count!))

(define (error:not-comparison object)
  (error "Not a comparison:" object))

(define (error:mismatched-skip-list-types skip-list-a skip-list-b)
  (error "Skip lists have mismatched types:" skip-list-a skip-list-b))

(define (skip-list-type/key-comparator skip-list-type)
  (skip-list-type.key-comparator skip-list-type))

(define (skip-list-type/key-order-predicate skip-list-type)
  (skip-list-type.key-order-predicate skip-list-type))

(define (make-skip-list type . arguments)
  (%make-skip-list
   type
   (make-header-node
    (+ (bottom-level)
       (if (pair? arguments)
           (car arguments)
           (default-max-level))))
   (bottom-level)
   0))

(define (skip-list/type skip-list)
  (skip-list.type skip-list))

(define (skip-list/count skip-list)
  (skip-list.count skip-list))

(define (skip-list/empty? skip-list)
  (bottom-level? (skip-list.level skip-list)))

(define (skip-list.max-level skip-list)
  (node-level (skip-list.header skip-list)))

(define (default-max-level) 32)

(define (random-level skip-list)
  (let ((max-level (skip-list.max-level skip-list)))
    (let loop ((level (bottom-level)))
      (let ((level (+ level 1)))
        (if (and (< level max-level) (zero? (random-integer 2)))
            (loop level)
            level)))))

;;!! Skip List Operations

(define (skip-list/search skip-list key if-found if-not-found)
  ((skip-list-type.operation/search (skip-list.type skip-list))
   skip-list key if-found if-not-found))

(define (skip-list/lookup skip-list key default)
  ((skip-list-type.operation/lookup (skip-list.type skip-list))
   skip-list key default))

(define (skip-list/member? skip-list key)
  ((skip-list-type.operation/member? (skip-list.type skip-list))
   skip-list key))

(define (skip-list/update! skip-list key if-found if-not-found)
  ((skip-list-type.operation/update! (skip-list.type skip-list))
   skip-list key if-found if-not-found))

(define (skip-list/insert! skip-list key datum)
  ((skip-list-type.operation/insert! (skip-list.type skip-list))
   skip-list key datum))

(define (skip-list/modify! skip-list key default modifier)
  ((skip-list-type.operation/modify! (skip-list.type skip-list))
   skip-list key default modifier))

(define (skip-list/intern! skip-list key generator)
  ((skip-list-type.operation/intern! (skip-list.type skip-list))
   skip-list key generator))

(define (skip-list/delete! skip-list key)
  ((skip-list-type.operation/delete! (skip-list.type skip-list))
   skip-list key))

;;!! Skip List <-> List Operations

(define (skip-list/fold skip-list initial-value combinator)
  (let loop ((node (skip-list.header skip-list)) (value initial-value))
    (let ((node* (node-next node)))
      (if (node? node*)
          (loop node* (combinator (node-key node*) (node-datum node*) value))
          value))))

(define (skip-list->list skip-list selector)
  (reverse
   (skip-list/fold skip-list '()
                   (lambda (key datum list)
                     (cons (selector key datum) list)))))

(define (skip-list->alist skip-list)
  (skip-list->list skip-list cons))

(define (skip-list/key-list skip-list)
  (skip-list->list skip-list (lambda (key datum) datum key)))

(define (skip-list/datum-list skip-list)
  (skip-list->list skip-list (lambda (key datum) key datum)))

(define (alist->skip-list alist skip-list-type)
                                        ;++ We can probably do better here.
  (let ((skip-list (make-skip-list skip-list-type)))
    (for-each (lambda (association)
                (skip-list/insert! skip-list
                                   (car association)
                                   (cdr association)))
              alist)
    skip-list))

;;!! Ordered Set Operations

(define (skip-list/min skip-list default)
  (skip-list/min-node skip-list node-key default))

(define (skip-list/min-datum skip-list default)
  (skip-list/min-node skip-list node-datum default))

(define (skip-list/min-pair skip-list)
  (skip-list/min-node skip-list node-pair #f))

(define (skip-list/delete-min! skip-list default)
  (skip-list/delete-min-node! skip-list node-key default))

(define (skip-list/delete-min-datum! skip-list default)
  (skip-list/delete-min-node! skip-list node-datum default))

(define (skip-list/delete-min-pair! skip-list)
  (skip-list/delete-min-node! skip-list node-pair #f))

(define (skip-list/max skip-list default)
  (skip-list/max-node skip-list node-key default))

(define (skip-list/max-datum skip-list default)
  (skip-list/max-node skip-list node-datum default))

(define (skip-list/max-pair skip-list)
  (skip-list/max-node skip-list node-pair #f))

(define (skip-list/delete-max! skip-list default)
  (skip-list/delete-max-node! skip-list node-key default))

(define (skip-list/delete-max-datum! skip-list default)
  (skip-list/delete-max-node! skip-list node-datum default))

(define (skip-list/delete-max-pair! skip-list)
  (skip-list/delete-max-node! skip-list node-pair #f))

(define (node-pair node)
  (cons (node-key node) (node-datum node)))

(define (skip-list/min-node skip-list receiver default)
  (let ((node (node-next (skip-list.header skip-list))))
    (if (node? node)
        (receiver node)
        default)))

(define (skip-list/delete-min-node! skip-list receiver default)
  (let* ((header (skip-list.header skip-list))
         (node (node-next header)))
    (if (node? node)
        (begin (skip-list/%delete-min-node! skip-list node)
               (receiver node))
        default)))

(define (skip-list/update-min! skip-list if-found if-empty)
  (let* ((header (skip-list.header skip-list))
         (node (node-next header)))
    (if (node? node)
        (if-found (node-key node)
                  (node-datum node)
                  (lambda (datum) (set-node-datum! node datum))
                  (lambda () (skip-list/%delete-min-node! skip-list node)))
        (if-empty))))

(define (skip-list/%delete-min-node! skip-list node)
  (let ((header (skip-list.header skip-list)))
    (let loop ((level (node-level node)))
      (let ((level (- level 1)))
        (set-node-forward! header level (node-forward node level))
        (if (not (bottom-level? level))
            (loop level)))))
  (skip-list/maybe-shrink! skip-list))

(define (skip-list/max-node skip-list receiver default)
  (skip-list/update-max-node! skip-list
                              (lambda (previous-node node level)
                                previous-node level               ;ignore
                                (receiver node))
                              (lambda () default)))

(define (skip-list/delete-max-node! skip-list receiver default)
  (skip-list/update-max-node! skip-list
                              (lambda (previous-node node level)
                                (skip-list/%delete-max-node! skip-list previous-node node level)
                                (receiver node))
                              (lambda () default)))

(define (skip-list/update-max! skip-list if-found if-empty)
  (skip-list/update-max-node! skip-list
                              (lambda (previous-node node level)
                                (if-found
                                 (node-key node)
                                 (node-datum node)
                                 (lambda (datum) (set-node-datum! node datum))
                                 (lambda ()
                                   (skip-list/%delete-max-node! skip-list previous-node node level))))
                              if-empty))

(define (skip-list/update-max-node! skip-list if-found if-empty)
  (let ((level (skip-list.level skip-list)))
    (if (bottom-level? level)
        (if-empty)
        (let ()
          (define (scan-down node level)
            ;; Assumption: NODE is not the last node.
            (let ((level* (- level 1)))
              (if (bottom-level? level*)
                  (scan-bottom node (node-next node))
                  (scan-across node level*))))
          (define (scan-across node level)
            (let ((node* (node-forward node level)))
              (if (node? node*)
                  (if (node? (node-next node*))
                      (scan-across node* level)
                      (if-found node node* level))
                  (scan-down node level))))
          (define (scan-bottom previous-node node)
            (if (not (node? node)) (error "Internal error in UPDATE-MAX!"))
            (let ((next-node (node-next node)))
              (if (node? next-node)
                  (scan-bottom node next-node)
                  (if-found previous-node node (bottom-level)))))
          (scan-down (skip-list.header skip-list) level)))))

(define (skip-list/%delete-max-node! skip-list previous-node node level)
  (define (delete-down node delete-node level)
    (set-node-forward! node level (non-node))
    (if (not (bottom-level? level))
        (delete-across node delete-node (- level 1))))
  (define (delete-across node delete-node level)
    (let ((node* (node-forward node level)))
      (if (not (node? node*)) (error "Internal error in DELETE-MAX!"))
      (if (eq? node* delete-node)
          (delete-down node delete-node level)
          (delete-across node* delete-node level))))
  (delete-down previous-node node level)
  (skip-list/maybe-shrink! skip-list))

(define (skip-list/split<! skip-list key)
  ((skip-list-type.operation/split<! (skip-list.type skip-list))
   skip-list key))

(define (skip-list/split>! skip-list key)
  ((skip-list-type.operation/split>! (skip-list.type skip-list))
   skip-list key))

(define (skip-list/split>=! skip-list key)
  (receive (less greater-or-equal)
           ((skip-list-type.operation/split<! (skip-list.type skip-list))
            skip-list key)
           (values greater-or-equal less)))

(define (skip-list/split<=! skip-list key)
  (receive (greater less-or-equal)
           ((skip-list-type.operation/split>! (skip-list.type skip-list))
            skip-list key)
           (values less-or-equal greater)))

(define (skip-list/union! skip-list-a skip-list-b)
  (skip-list/left-union! skip-list-a skip-list-b))

(define (skip-list/left-union! skip-list-a skip-list-b)
  ((skip-list-type.operation/left-union!
    (check-skip-list-types skip-list-a skip-list-b))
   skip-list-a skip-list-b))

(define (skip-list/right-union! skip-list-a skip-list-b)
  ((skip-list-type.operation/right-union!
    (check-skip-list-types skip-list-a skip-list-b))
   skip-list-a skip-list-b))

(define (skip-list/union-merge! skip-list-a skip-list-b merger)
  ((skip-list-type.operation/union-merge!
    (check-skip-list-types skip-list-a skip-list-b))
   skip-list-a skip-list-b merger))

(define (check-skip-list-types skip-list-a skip-list-b)
  (let ((type (skip-list.type skip-list-a)))
    (if (not (eq? type (skip-list.type skip-list-b)))
        (error:mismatched-skip-list-types skip-list-a skip-list-b))
    type))

;; Node Abstraction

;; Note: the NON-NODE procedure must return the same value each time,
;; in the sense of EQ?.

(define (make-header-node level)
  (make-node level 'HEADER-DUMMY-KEY 'HEADER-DUMMY-DATUM))

;; This implementation of nodes uses two cons cells and a vector, in
;; order to require no frobnication of level indices.

;; (define (bottom-level) 0)
;; (define (bottom-level? level) (zero? level))
;;
;; (define (make-node level key datum)
;;   (cons (cons key datum) (make-vector level (non-node))))
;;
;; (define (non-node) #f)
;; (define (node? object) (pair? object))
;; (define (node-level node) (vector-length (cdr node)))
;; (define (node-key node) (caar node))
;; (define (node-datum node) (cdar node))
;; (define (set-node-datum! node datum) (set-cdr! (car node) datum))
;; (define (node-next node) (node-forward node 0))
;; (define (node-forward node level) (vector-ref (cdr node) level))
;; (define (set-node-forward! node level forward)
;;   (vector-set! (cdr node) level forward))

;; This implementation of nodes uses two-element longer vectors, and
;; zero-based level indices, but must adjust the level indices for any
;; access to the vector.

;; (define (bottom-level) 0)
;; (define (bottom-level? level) (zero? level))
;;
;; (define (make-node level key datum)
;;   (let ((node (make-vector (+ level 2) (non-node))))
;;     (vector-set! node 0 key)
;;     (vector-set! node 1 datum)
;;     node))
;;
;; (define (non-node) #f)
;; (define (node? object) (vector? object))
;; (define (node-level node) (- (vector-length node) 2))
;; (define (node-key node) (vector-ref node 0))
;; (define (node-datum node) (vector-ref node 1))
;; (define (set-node-datum! node datum) (vector-set! node 1 datum))
;; (define (node-next node) (vector-ref node 2))
;; (define (node-forward node level) (vector-ref node (+ level 2)))
;; (define (set-node-forward! node level forward)
;;   (vector-set! node (+ level 2) forward))

;; This implementation uses a two-element longer vector, and
;; represents levels by one-based indices.  In MIT Scheme this is
;; about 5% faster than either of the above two.  In most systems this
;; will use much a little bit less storage than the pair-based one,
;; proportional to the number of associations in the skip list.

(define (bottom-level) 2)
(define (bottom-level? level) (= level 2))

(define (make-node level key datum)
  (let ((node (make-vector level (non-node))))
    (vector-set! node 0 key)
    (vector-set! node 1 datum)
    node))

(define (non-node) #f)
(define (node? object) (vector? object))
(define (node-level node) (vector-length node))
(define (node-key node) (vector-ref node 0))
(define (node-datum node) (vector-ref node 1))
(define (set-node-datum! node datum) (vector-set! node 1 datum))
(define (node-next node) (vector-ref node 2))
(define (node-forward node level) (vector-ref node level))
(define (set-node-forward! node level forward)
  (vector-set! node level forward))

;; Making Skip List Types

(define (make-skip-list-type/order-predicate key<?)
  (make-skip-list-type (lambda (key-a key-b)
                         (cond ((key<? key-a key-b) -1)
                               ((key<? key-b key-a) +1)
                               (else 0)))
                       key<?))

(define (make-skip-list-type/comparator compare-keys)
  (make-skip-list-type compare-keys
                       (lambda (key-a key-b)
                         (let ((comparison (compare-keys key-a key-b)))
                           (case comparison
                             ((-1) #t)
                             ((0 +1) #f)
                             (else (error:not-comparison comparison)))))))

;; Originally this was all one big procedure so that it could be
;; specialized for values of COMPARE-KEYS that reduce nicely, but it
;; has now gotten out of hand.  At least, SEARCH and UPDATE! should be
;; integrated at their call sites within this giant definition.

(define (make-skip-list-type compare-keys key<?)

  ;; Please ignore this bogosity.  Yes, the indentation is wrong.
  (let ((the-skip-list-type #f))

    (define (search skip-list key if-found if-not-found)

      ;; There is a moderately clever optimization going on in this
      ;; algorithm to avoid repeated comparisons to the same keys.  If
      ;; the only forward pointer from NODE at LEVEL is NEXT, then we
      ;; have already compared KEY to NEXT's key, so we can continue
      ;; searching down, confident that the desired association will lie
      ;; between NODE and NEXT.  NEXT may also be the non-node.  There is
      ;; no extra cost to this because we have to check for non-nodes
      ;; anyway (a notable omission from the algorithms in the papers).
      (define (search-across level node next)
        (let ((node* (node-forward node level)))
          (if (eq? node* next)
              (search-down level node next)
              (let* ((key* (node-key node*))
                     (comparison (compare-keys key key*)))
                (case comparison
                  ((-1) (search-down level node node*))
                  ((+1) (search-across level node* (non-node)))
                  ((0) (if-found (node-datum node*)))
                  (else (error:not-comparison comparison)))))))

      (define (search-down level node next)
        (if (bottom-level? level)
            (if-not-found)
            (search-across (- level 1) node next)))

      (search-down (skip-list.level skip-list)
                   (skip-list.header skip-list)
                   (non-node)))

    (define (lookup skip-list key default)
      (search skip-list key (lambda (datum) datum) (lambda () default)))

    (define (member? skip-list key)
      (search skip-list key (lambda (datum) datum #t) (lambda () #f)))

    ;; MAKE-SKIP-LIST-TYPE, continued: update

    (define (update! skip-list key if-found if-not-found)
      (let ((header (skip-list.header skip-list))
            (level (skip-list.level skip-list)))
        (let ((update (make-vector level header)))

          (define (search-across level node next)
            (let ((node* (node-forward node level)))
              (if (eq? node* next)
                  (search-down/update level node next)
                  (let ((comparison (compare-keys key (node-key node*))))
                    (case comparison
                      ((-1) (search-down/update level node node*))
                      ((+1) (search-across level node* (non-node)))
                      ((0)
                       (if-found
                        (node-datum node*)
                        (lambda (datum)
                          (set-node-datum! node* datum))
                        (lambda ()
                          (do-delete skip-list node level node* update))))
                      (else (error:not-comparison comparison)))))))

          (define (search-down/update level node next)
            (vector-set! update level node)
            (search-down level node next))

          (define (search-down level node next)
            (if (bottom-level? level)
                (if-not-found (lambda (datum)
                                (do-insert skip-list key datum update)))
                (search-across (- level 1) node next)))

          (search-down level header (non-node)))))

    ;; MAKE-SKIP-LIST-TYPE, continued: update utilities

    (define (insert! skip-list key datum)
      (update! skip-list key
               (lambda (datum* replace delete)
                 datum* delete          ;ignore
                 (replace datum))
               (lambda (insert)
                 (insert datum))))

    (define (modify! skip-list key default modifier)
      (update! skip-list key
               (lambda (datum replace delete)
                 delete                 ;ignore
                 (replace (modifier datum)))
               (lambda (insert)
                 (let ((datum (modifier default)))
                   (insert datum)
                   datum))))

    (define (intern! skip-list key generator)
      (update! skip-list key
               (lambda (datum replace delete)
                 replace delete         ;ignore
                 datum)
               (lambda (insert)
                 (let ((datum (generator key)))
                   (insert datum)
                   datum))))

    (define (delete! skip-list key)
      (update! skip-list key
               (lambda (datum replace delete)
                 datum replace          ;ignore
                 (delete))
               (lambda (insert)
                 insert                 ;ignore
                 (values))))

    ;; MAKE-SKIP-LIST-TYPE, continued: split

    (define (split! skip-list key frob)
      (let ((header* (make-header-node (skip-list.max-level skip-list)))
            (level (skip-list.level skip-list)))
        (define (finish level count)
          (values skip-list
                  (%make-skip-list the-skip-list-type header* level count)))
        (if (bottom-level? level)
            (finish level 0)
            (let ()
              (define (search-across node level next)
                (let ((node* (node-forward node level)))
                  (if (eq? node* next)
                      (search-down node level next)
                      (let ((comparison
                             (* frob (compare-keys key (node-key node*)))))
                        (case comparison
                          ((-1) (search-down node level node*))
                          ((+1) (search-across node* level (non-node)))
                          ((0) (finish-down node level node*))
                          (else (error:not-comparison comparison)))))))
              (define (search-down node level next)
                (snip node level next)
                (if (bottom-level? level)
                    (shrink-and-finish)
                    (search-across node (- level 1) next)))
              (define (finish-across node level next)
                (let ((node* (node-forward node level)))
                  (if (eq? node* next)
                      (finish-down node level next)
                      (finish-across node* level next))))
              (define (finish-down node level next)
                (snip node level next)
                (if (bottom-level? level)
                    (shrink-and-finish)
                    (finish-across node (- level 1) next)))
              (define (shrink-and-finish)
                (skip-list/maybe-shrink! skip-list)
                (finish (find-level header* level) (count-em)))
              (define (count-em) ;Argh!  Turns O(log n) into Theta(n).
                (let loop ((node (skip-list.header skip-list))
                           (node* header*)
                           (count 0))
                  (let ((next (node-next node)) (next* (node-next node*)))
                    (cond ((not (node? next))
                           (let ((old-count (skip-list.count skip-list)))
                             (set-skip-list.count! skip-list count)
                             (- old-count count)))
                          ((not (node? next*))
                           (set-skip-list.count!
                            skip-list
                            (- (skip-list.count skip-list) count))
                           count)
                          (else
                           (loop next next* (+ count 1)))))))
              (define (snip node level next)
                (set-node-forward! node level (non-node))
                (set-node-forward! header* level next))
              (search-across (skip-list.header skip-list)
                             (- level 1)
                             (non-node))))))

    (define (split<! skip-list key) (split! skip-list key +1))
    (define (split>! skip-list key) (split! skip-list key -1))

    ;; MAKE-SKIP-LIST-TYPE, continued: union

    ;; Mega-hairy!  Sorry.  This is really just the obvious merge of two
    ;; lists, with extra bookkeeping and skipping.

    (define (%union-merge! skip-list-a skip-list-b merger!)
      (let ((level-a (skip-list.level skip-list-a))
            (level-b (skip-list.level skip-list-b)))
        (cond ((bottom-level? level-a) skip-list-b)
              ((bottom-level? level-b) skip-list-a)
              (else
               (let ((level* (max level-a level-b))
                     (max-level*
                      (max (skip-list.max-level skip-list-a)
                           (skip-list.max-level skip-list-b))))
                 (let* ((header (make-header-node max-level*))
                        (update (make-vector level* header)))

                   (define (finish left-over left-over-level duplicates)
                     (let loop ((level (bottom-level)))
                       (set-node-forward! (vector-ref update level)
                                          level
                                          (node-forward left-over level))
                       (let ((level (+ level 1)))
                         (if (< level left-over-level)
                             (loop level))))
                     (%make-skip-list the-skip-list-type
                                      header
                                      (find-level header level*)
                                      (- (+ (skip-list.count skip-list-a)
                                            (skip-list.count skip-list-b))
                                         duplicates)))

                   (define (scan-forward header-a level-a key-b header-b level-b
                                         flipped? duplicates)
                     (define (scan-up node level)
                       (set-node-forward! (vector-ref update level) level node)
                       (let ((next-level (+ level 1)))
                         (if (>= next-level level-a)
                             (scan-down/across node level)
                             (let ((next-node
                                    (node-forward header-a next-level)))
                               (if (key<? key-b (node-key next-node))
                                   (scan-down/across node level)
                                   (scan-up next-node next-level))))))
                     (define (scan-down/across node level)
                       (let ((node* (node-forward node level)))
                         (if (or (not (node? node*))
                                 (key<? key-b (node-key node*)))
                             (begin
                               (vector-set! update level node)
                               (set-node-forward! header-a level node*)
                               (if (bottom-level? level)
                                   (loop header-a level-a header-b level-b
                                         flipped?
                                         (if (maybe-merge key-b node header-b
                                                          flipped?)
                                             (+ duplicates 1)
                                             duplicates))
                                   (scan-down/across node (- level 1))))
                             (scan-down/across node* level))))
                     (scan-up (node-next header-a) (bottom-level)))

                   ;; MAKE-SKIP-LIST-TYPE, continued: union, continued

                   (define (maybe-merge key node header flipped?)
                     (let ((comparison (compare-keys key (node-key node))))
                       (case comparison
                         ((0) (let ((datum (node-datum (node-next header))))
                                (merger! key node datum flipped?)
                                #t))
                         ((-1 +1) #f)
                         (else (error:not-comparison comparison)))))

                   (define (loop header-a level-a header-b level-b flipped?
                                 duplicates)
                     (let ((node-a (node-next header-a))
                           (node-b (node-next header-b)))
                       (cond ((not (node? node-a))
                              (finish header-b level-b duplicates))
                             ((not (node? node-b))
                              (finish header-a level-a duplicates))
                             (else
                              (let ((key-a (node-key node-a))
                                    (key-b (node-key node-b)))
                                (if (key<? key-b key-a)
                                    (scan-forward header-b level-b
                                                  key-a header-a level-a
                                                  (not flipped?) duplicates)
                                    (scan-forward header-a level-a
                                                  key-b header-b level-b
                                                  flipped? duplicates)))))))

                   (loop (skip-list.header skip-list-a) level-a
                         (skip-list.header skip-list-b) level-b
                         #f 0)))))))

    (define (union-merge! skip-list-a skip-list-b merger)
      (%union-merge! skip-list-a skip-list-b
                     (lambda (key target-node source-datum flipped?)
                       (set-node-datum! target-node
                                        (let ((target-datum (node-datum target-node)))
                                          (if flipped?
                                              (merger key source-datum target-datum)
                                              (merger key target-datum source-datum)))))))

    (define (left-union! skip-list-a skip-list-b)
      (%union-merge! skip-list-a skip-list-b
                     (lambda (key target-node source-datum flipped?)
                       key              ;ignore
                       (if flipped? (set-node-datum! target-node source-datum)))))

    (define (right-union! skip-list-a skip-list-b)
      (%union-merge! skip-list-a skip-list-b
                     (lambda (key target-node source-datum flipped?)
                       key              ;ignore
                       (if (not flipped?) (set-node-datum! target-node source-datum)))))

    ;; Urk.  Silly.
    (set! the-skip-list-type
          (%make-skip-list-type
           compare-keys key<? search lookup member?
           update! insert! modify! intern! delete!
           split<! split>! left-union! right-union! union-merge!))

    the-skip-list-type))

;;!! Insertion and Deletion

(define (do-insert skip-list key datum update)
  (let ((new-level (random-level skip-list))
        (old-level (skip-list.level skip-list)))
    (let ((new-node (make-node new-level key datum)))
      (define (do-update top-level)
        (do ((level (bottom-level) (+ level 1)))
            ((= level top-level))
          (let ((old-node (vector-ref update level)))
            (set-node-forward! new-node level (node-forward old-node level))
            (set-node-forward! old-node level new-node))))
      (if (< new-level old-level)
          (do-update new-level)
          (begin
            (do-update old-level)
            (let ((header (skip-list.header skip-list)))
              (do ((level old-level (+ level 1)))
                  ((= level new-level))
                (set-node-forward! header level new-node)))
            (set-skip-list.level! skip-list new-level)))))
  (set-skip-list.count! skip-list (+ 1 (skip-list.count skip-list))))

(define (do-delete skip-list current-node current-level delete-node update)
  (define (unsplice node level)       ;++ Should this also destroy DELETE-NODE?
    (set-node-forward! node level (node-forward delete-node level)))
  (let ((limit (vector-length update)))
    (let do-update ((level current-level))
      (let ((level (+ level 1)))
        (if (< level limit)
            (let ((node (vector-ref update level)))
              (if (eq? delete-node (node-forward node level))
                  (begin (unsplice node level) (do-update level))))))))
  ;; Why this extra loop, which is not in the algorithm in the paper?
  ;; The algorithm in the paper searches all the way to the bottom
  ;; level, performing needless key comparisons.  The search algorithm
  ;; here avoids needless key comparisons by stopping once it has found
  ;; the desired node, but that leaves the update vector only partially
  ;; filled.  (Filling it may not be needed, if we decide that we don't
  ;; want to delete after all.)  This loop compensates for that.
  (let loop ((node current-node) (level current-level))
    (unsplice node level)
    (let loop-down ((level level))
      (if (not (bottom-level? level))
          (let ((level (- level 1)))
            (let loop-across ((node node))
              (let ((node* (node-forward node level)))
                (cond ((eq? node* delete-node) (loop node level))
                      ((node? node*) (loop-across node*))
                      (else (loop-down level)))))))))
  (skip-list/maybe-shrink! skip-list)
  (set-skip-list.count! skip-list (- (skip-list.count skip-list) 1)))

(define (skip-list/maybe-shrink! skip-list)
  (let ((header (skip-list.header skip-list))
        (level (- (skip-list.level skip-list) 1)))
    (if (not (node? (node-forward header level)))
        (set-skip-list.level! skip-list (find-level header level)))))

(define (find-level header level)
  (let loop ((level level))
    (if (or (bottom-level? level)
            (node? (node-forward header (- level 1))))
        level
        (loop (- level 1)))))

;;!! Comparison Operators

(define (compare-exact-integers a b)
  ;; Why not just (SIGNUM (- A B))?  Aside from the lack of a standard
  ;; SIGNUM procedure, that may overflow into bignum territory.
  (cond ((< a b) -1)
        ((< b a) +1)
        (else 0)))

(define (compare-reals a b)
  (cond ((< a b) -1)
        ((< b a) +1)
        (else 0)))

;; These are pretty silly.

(define (compare-symbols a b)
  (compare-strings (symbol->string a) (symbol->string b)))

(define (compare-strings a b)
  (let ((length-a (string-length a))
        (length-b (string-length b)))
    (let ((length (min length-a length-b)))
      (let loop ((i 0))
        (if (>= i length)
            (compare-exact-integers length-a length-b)
            (let ((char-a (string-ref a i))
                  (char-b (string-ref b i)))
              (cond ((char<? char-a char-b) -1)
                    ((char<? char-b char-a) +1)
                    (else (loop (+ i 1))))))))))

(define (compare-strings-ci a b)
  (let ((length-a (string-length a))
        (length-b (string-length b)))
    (let ((length (min length-a length-b)))
      (let loop ((i 0))
        (if (>= i length)
            (compare-exact-integers length-a length-b)
            (let ((char-a (string-ref a i))
                  (char-b (string-ref b i)))
              (cond ((char-ci<? char-a char-b) -1)
                    ((char-ci<? char-b char-a) +1)
                    (else (loop (+ i 1))))))))))

;;!! Miscellaneous Skip List Types

(define skip-list-type:real-number
  (make-skip-list-type/comparator compare-reals))

(define skip-list-type:exact-integer
  (make-skip-list-type/comparator compare-exact-integers))

(define skip-list-type:symbol
  (make-skip-list-type/comparator compare-symbols))

(define skip-list-type:string
  (make-skip-list-type/comparator compare-strings))

(define skip-list-type:string-ci
  (make-skip-list-type/comparator compare-strings-ci))

;;; Iterator for foof-loop; see
;;; <http://mumble.net/~campbell/scheme/foof-loop.txt>.

;; (define-syntax in-skip-list
;;   (syntax-rules ()
;;     ((IN-SKIP-LIST (key-variable datum-variable)
;;                    (skip-list-expression)
;;                    next . rest)
;;      (next (((SKIP-LIST) skip-list-expression))       ;Outer bindings
;;            ((NODE (SKIP-LIST/INITIAL-NODE SKIP-LIST)  ;Loop variables
;;                   NODE*))
;;            ()                                         ;Entry bindings
;;            ((NOT (NODE? NODE)))                       ;Termination conditions
;;            (((key-variable) (NODE-KEY NODE))          ;Body bindings
;;             ((datum-variable) (NODE-DATUM NODE))
;;             ((NODE*) (NODE-NEXT NODE)))
;;            ()                                         ;Final bindings
;;            . rest))))

;; (define (skip-list/initial-node skip-list)
;;   (node-next (skip-list.header skip-list)))
