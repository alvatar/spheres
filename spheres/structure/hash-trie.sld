;;!!! Hash Tries: Persistent Trie-Structured Hash Tables
;; .author Taylor R. Campbell, 2009
;; .author Alvaro Castro-Castilla, 2015

;; This implements what are called Hash Array Mapped Tries (HAMT) in
;;
;;   Phil Bagwell, `Ideal Hash Trees', Technical Report, 2001.
;;
;; Provided that the hash function have no collisions and yield hash
;; values in a bounded interval, hash tries admit search and update in
;; constant worst-case time (!), bounded by a somewhat larger constant
;; than what one usually finds for the worst-case time of search or
;; replacement, and the amortized time of insertion or deletion, in
;; hash tables.  There is no complicated incremental rehashing going
;; on like in some real-time hash tables to attain these constant time
;; bounds; in fact, there is never any rehashing.  A little more
;; precisely, `constant' means logarithmically proportional to the
;; length of the interval of the hash values, or, practically,
;; linearly proportional to the number of bits in the hash, with small
;; constant factors.
;;
;; Yes, this is the same data structure as Clojure uses to implement
;; its hash maps and hash sets.  Similarly to Clojure, this code uses
;; hash collision buckets rather than the paper's suggestion of
;; repeating hashes salted by the trie depth.
;;
;; Although the pronunciation is identical, and despite the title of
;; Bagwell's paper, a hash trie is not a hash tree.  Sorry.  Nor do
;; these hash tries have any relation to what Knuth calls hash tries.
;;
;; This code depends on SRFIs 8 (RECEIVE), 9 (DEFINE-RECORD-TYPE), 23
;; (ERROR), and 33 (Integer Bitwise Operations)

(define-library (spheres/structure hash-trie)
  (export make-hash-trie
          hash-trie-type
          hash-trie/count
          hash-trie/empty?
          hash-trie-type/key-equality-predicate
          hash-trie-type/key-hash-function
          hash-trie/search
          hash-trie/lookup
          hash-trie/member?
          hash-trie-update
          hash-trie/insert
          hash-trie/modify
          hash-trie/intern
          hash-trie/delete
          hash-trie->alist
          hash-trie/key-list
          hash-trie/datum-list
          alist->hash-trie
          string-hash
          symbol-hash
          exact-integer-hash
          real-number-hash
          complex-number-hash
          hash-trie-type:string
          hash-trie-type:string-ci
          hash-trie-type:symbol
          hash-trie-type:exact-integer
          hash-trie-type:real-number
          hash-trie-type:complex-number)

  (include "hash-trie.scm"))
