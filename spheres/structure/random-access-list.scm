;;!!! Random Access List
;; .author Juergen Lorenz, 2011-2013
;;
;;
;;  Copyright (c) 2011-2013, Juergen Lorenz
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are
;;  met:
;;
;;  Redistributions of source code must retain the above copyright
;;  notice, this list of conditions and the following disclaimer.
;;
;;  Redistributions in binary form must reproduce the above copyright
;;  notice, this list of conditions and the following disclaimer in the
;;  documentation and/or other materials provided with the distribution.
;;
;;  Neither the name of the author nor the names of its contributors may be
;;  used to endorse or promote products derived from this software without
;;  specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;;  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;;  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;;  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;  HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;  Author: Juergen Lorenz
;;          ju (at) jugilo (dot) de
;;
;;  Last update: Apr 16, 2013
;;
;; Rationale
;; =========
;;
;; Random-access-lists combine the advantages of vectors (fast access) and
;; linked-lists (fast insertions). They can be implemented on the basis of
;; skiplists.
;;
;; Whereas an ordinary skiplist-node consists of an item and a vector of
;; next nodes, whose length is computed randomly in such a way, that the
;; number of nodes with length n are in the average one half of the number
;; of nodes with length (- n 1), a random-access-list-node must have an
;; additional vector of same length containing the jumps, i.e. numbers
;; indicating how far the node is moved when following the next nodes at a
;; given level. Following a node at level n describes a fast lane across
;; the random-access-list, where the jumps at level n are in the average
;; twice as long as the jumps at the level below.
;;
;; In our implementation of random-access-lists we store a vector of nodes,
;; called cursors, and a vector of positions, called places, which are
;; updated along the movement accross the list. Moving cursors and places
;; to a given position, pos, works as follows: One starts at the highest
;; level and follows the next link at that level adding the jump at that
;; level to the place at that level until the ladder is less than pos but
;; a further movement at that level would be greater or equal pos. Then
;; one saves cursor and place and restarts the same movement process at
;; the level below, starting with the values saved in the level above.
;; Eventually one reaches level 0 and stops at a place one less than pos.
;; The stored cursors can than be used to insert or remove an item, as
;; well as getting or setting pos' item. Note, that in the latter case we
;; only need to step down until a level where the next place is equal to
;; pos. Since this cursor and place movement is O(log n), so are all the
;; fundamental random-access-list operations, insert!, remove!, ref and set!
;;
;; The other supplied operators like map, filter split and join work only
;; at a fixed level, whence are ordinary linked list operators, which
;; perform as O(n).
;;
;; Some additional remarks are in order.
;;
;; We described the process with a width of two, i.e. increasing the level
;; of movement doubles the jumps of next nodes in the average.  A higher
;; value than two for the width is possible as well, trading performance
;; against space.
;;
;; We said nothing about the maximal length of the nodes, i.e. of the
;; maximal height of the random-access-list. Our default is 10, but this
;; can be changed in the constructor. This should be appropriate in most
;; cases. But note, that the highest actual, i.e. computed, node height
;; might be smaller, so it must be updated in the list, so that the cursor
;; knows where to start.

;;; helpers

;; From Chicken's data-structures module
(define (o . fns)
  (if (null? fns)
      (lambda (x) x)
      (let loop ((fns fns))
	(let ((h (car fns))
	      (t (cdr fns)) )
	  (if (null? t)
	      h
	      (lambda (x) (h ((loop t) x))))))))

(define-syntax do-while
  (syntax-rules ()
    ((_ test? xpr xpr1 ...)
     (let loop ()
       (if test?
           (begin
             xpr xpr1 ...
             (loop)))))))

;;; stop-node-nexts is a vector of '() instead of nodes
(define (nexts-map fn vec)
  (let* ((len (vector-length vec)) (result (make-vector len)))
    (do ((n 0 (++ n)))
        ((fx= n len) result)
      (vector-set! result n (if (null? vec)
                                gstop
                                (fn (vector-ref vec n)))))))

(define (repeat-string str k)
  (let loop ((k k) (result ""))
    (if (zero? k)
        result
        (loop (-- k) (string-append str result)))))

;; this is where randomness comes into the play
(define (choose-height width)
  (let loop ((choice (random-integer width)) (k 1))
    (if (fx>= choice 1)
        k
        (loop (random-integer width) (++ k)))))

(define gstart (gensym 'start))
(define gstop (gensym 'stop))

;;;; setters will in general not be exported,
;;;; whence they can be defined inline

;;; state of a random-access-list (hidden)
;;; i.e. item of head node
(define-record-type state
  (make-state item? width max-height height count level start stop places cursors)
  state?
  ;; constant
  (item? state-item?)
  (width state-width)
  (max-height state-max-height)
  ;; mutable
  (height state-height state-height-set!) ; actual height
  (count state-count state-count-set!) ; number of nodes
  (level state-level state-level-set!) ; level at which go! stops
  (start state-start state-start-set!) ; start node
  (stop state-stop state-stop-set!) ; stop node
  (places state-places state-places-set!) ; vector of positions
  (cursors state-cursors state-cursors-set!)) ; vector of nodes

;; (define-record-printer (state st out)
;;   (format out
;;           "[height: ~S count: ~S level: ~S places: ~S cursor-items: ~S]"
;;           (state-height st)
;;           (state-count st)
;;           (state-level st)
;;           (state-places st)
;;           (nexts-map ral-node-item (state-cursors st))))

;; unary composed procedures can be defined with o

;;; node of a random-access-list
(define-record-type ral-node
  (make-ral-node item jumps nexts)
  ral-node?
  (item ral-node-item ral-node-item-set!)
  (jumps ral-node-jumps ral-node-jumps-set!)
  (nexts ral-node-nexts ral-node-nexts-set!))

(define (ral? node)
  (and (ral-node? node)
       (state? (ral-node-item node))))
(define (ral-node-start? node)
  (and (ral-node? node)
       (eq? (ral-node-item node) gstart)))
(define (ral-node-stop? node)
  (and (ral-node? node)
       (eq? (ral-node-item node) gstop)))


;;; You should not print the whole list of items. If you do that, you
;;; traverse the list linearly, i.e. you loose the peformance of
;;; skiplists. So only the items of nodes with highest level are printed
;; (define-record-printer (ral-node node out)
;;   (if (ral? node)
;;     (let ((lst (ral->pairs node (-- (ral-height node)))))
;;       (format out
;;               "#,(ral [width: ~S height: ~S level: ~S count: ~S]~? ...)"
;;               (ral-width node)
;;               (ral-height node)
;;               (ral-level node)
;;               (ral-count node)
;;               (repeat-string " ... ~S@~S" (fxshr (length lst) 1))
;;               lst))
;;     ;; for debugging
;;     (format out
;;             "#,(ral-node item: ~S jumps: ~S  next-items: ~S)"
;;             (ral-node-item node)
;;             (ral-node-jumps node)
;;             (nexts-map ral-node-item (ral-node-nexts node)))))

;; print the linked lists at each level including their places
;; (define* (ral-print ls (k 0) (out (current-output-port)))
;;   (let ((lst (ral->pairs ls k)))
;;     (format out
;;             "#,(ral [width: ~S height: ~S level: ~S count: ~S]~?)~%"
;;             (ral-width ls)
;;             (ral-height ls)
;;             (ral-level ls)
;;             (ral-count ls)
;;             (repeat-string " ~S@~S" (fxshr (length lst) 1))
;;             lst)))

(define (ral-node-height node)
  (vector-length (ral-node-jumps node)))

(define (make-stop-node height)
  (make-ral-node gstop
                 (make-vector height 0)
                 (make-vector height '())))

;; node constructor (to be re-initialized later)
(define (ral-node item height)
  (make-ral-node item
                 (make-vector height 0)
                 (make-vector height
                              (make-stop-node height))))

;; exported head-node constructor
(define* (make-ral item? (width 2) (max-height 10))
  (let* ((stop (make-stop-node max-height))
         (start (make-ral-node gstart
                               (make-vector max-height 1) ;jumps
                               (make-vector max-height stop)))) ;nexts
    (make-ral-node
     ;;(make-state item? width max-height height count level start stop places cursors)
     (make-state item?
                 width
                 max-height
                 1 ; height
                 0 ; count
                 0 ; level
                 start
                 stop
                 (vector -1) ; places
                 (vector start)) ; cursors
     (make-vector max-height 1) ; jumps
     (make-vector max-height stop)))) ; nexts


(define (ral-node-jump node k)
  (vector-ref (ral-node-jumps node) k))
(define (ral-node-jump-set! node k n)
  (vector-set! (ral-node-jumps node) k n))

(define (ral-node-next node k)
  (vector-ref (ral-node-nexts node) k))
(define (ral-node-next-set! node k new)
  (vector-set! (ral-node-nexts node) k new))

(define ral-state ral-node-item)
(define ral-max-height (o state-max-height ral-state))
(define ral-width (o state-width ral-state))
(define ral-item? (o state-item? ral-state))
(define ral-height (o state-height ral-state))
(define  (ral-height-set! ls n)
  (state-height-set! (ral-state ls) n));(compose state-height-set! ral-state))
(define ral-count (o state-count ral-state))
(define  (ral-count-set! ls n)
  (state-count-set! (ral-state ls) n));(compose state-count-set! ral-state))
(define ral-start (o state-start ral-state))
(define (ral-stop ls); (o state-stop ral-state))
  (state-stop (ral-state ls)))
(define (ral-places ls) ;(o state-places ral-state))
  (state-places (ral-state ls)))
(define (ral-places-set! ls places)
  (state-places-set! (ral-state ls) places))
(define (ral-cursors ls) ;(o state-cursors ral-state))
  (state-cursors (ral-state ls)))
(define (ral-cursors-set! ls cursors)
  (state-cursors-set! (ral-state ls) cursors))
(define ral-null? (o zero? ral-count))
(define ral-level (o state-level ral-state))
(define (ral-level-set! ls k)
  (state-level-set! (ral-state ls) k))

(define (ral-place ls k)
  (vector-ref (ral-places ls) k))
(define (ral-place-set! ls k n)
  (vector-set! (ral-places ls) k n))
(define (ral-place-next ls k)
  (fx+ (ral-place ls k) (ral-cursor-jump ls k)))

(define (ral-cursor ls k)
  (vector-ref (ral-cursors ls) k))
(define (ral-cursor-set! ls k new)
  (vector-set! (ral-cursors ls) k new))

(define (ral-cursor-jump ls k)
  (ral-node-jump (ral-cursor ls k) k))

(define (ral-cursor-next ls k)
  (ral-node-next (ral-cursor ls k) k))

(define (ral-forth! ls k)
  (ral-place-set! ls k (ral-place-next ls k))
  (ral-cursor-set! ls k (ral-cursor-next ls k)))

(define (ral-top ls)
  (-- (ral-height ls)))

(define (ral-start! ls k place)
  (if (fx= k (ral-top ls))
                                        ;(begin
      ;; only move to start node, if ral-place >= place
      (when (fx>= (ral-place ls k) place)
            (ral-place-set! ls k -1)
            (ral-cursor-set! ls k (ral-start ls)))
      ;; start at level above
      (let ((k+ (++ k)))
        (ral-place-set! ls k (ral-place ls k+))
        (ral-cursor-set! ls k (ral-cursor ls k+)))))

(define (ral-moveto! ls k place)
  (do-while (fx< (ral-place-next ls k) place)
            (ral-forth! ls k)))

;;; The following two internal commands do the actual movement of
;;; cursors and places
;;;
;;; move cursor at each level upto place
;;; bail out if next cursor's place hits place
;;; in any case save level
(define (ral-go! ls place)
  (call-with-current-continuation
   (lambda (out)
     (do ((k (ral-top ls) (-- k)))
         ((fx< k 0) (ral-level-set! ls 0))
       (ral-start! ls k place)
       (ral-moveto! ls k place)
       (when (fx= (ral-place-next ls k) place)
             (ral-level-set! ls k)
             (out (void)))))))

;;; continue cursor movements below saved level
;;; but don't touch level
(define (ral-go-on! ls)
  (let* ((level (ral-level ls))
         ;; the place, ral-go! moved to
         (place (ral-place-next ls level)))
    (do ((k (-- level) (-- k)))
        ((fx< k 0))
      (ral-start! ls k place)
      (ral-moveto! ls k place))))

(define (ral-ref ls place)
  (ral-go! ls place)
  (ral-node-item (ral-cursor-next ls (ral-level ls))))

(define (ral-set! ls place item)
  (ral-go! ls place)
  (ral-node-item-set! (ral-cursor-next ls (ral-level ls)) item))

;; internal
(define (ral-state-adjust! ls height)
  ;; vector-resize is a Chicken Scheme library procedure
  ;; XXX optimize
  (define (vector-resize vec n init)
    (let ((new-vec (make-vector n init)))
      (let recur ((k (vector-length vec)))
        (if (zero? k)
            new-vec
            (begin (vector-set! new-vec k (vector-ref vec k))
                   (recur (-- k)))))))
  (let ((count (ral-count ls))
        (start (ral-start ls)))
    ;; adjust jumps of start above old height
    (do ((k (ral-height ls) (++ k)))
        ((fx= k height))
      (ral-node-jump-set! start k (++ count)))
    ;; adjust places
    (ral-places-set! ls
                     (vector-resize (ral-places ls)
                                    height
                                    -1))
    ;; adjust cursors
    (ral-cursors-set! ls
                      (vector-resize (ral-cursors ls)
                                     height
                                     start))
    ;; adjust height
    (ral-height-set! ls height)))

;; insert item before place
(define (ral-insert! ls place item)
  ;; prepare cursor for insertion
  (ral-go! ls place)
  (ral-go-on! ls)
  ;; now cursor at each level is before place but next-place is after
  ;; i.e. cursor is prepared for insertion
  (let* ((height (fxmin (ral-max-height ls)
                        (choose-height (ral-width ls))))
         (new (ral-node item height)))
    ;; restructure if necessary (doesn't happen often)
    (if (fx> height (ral-height ls))
        (ral-state-adjust! ls height))
    (do ((k 0 (++ k)))
        ((fx= k (ral-height ls)))
      ;; the node and place after which to insert
      (let ((node-before (ral-cursor ls k))
            (place-before (ral-place ls k)))
        ;; add 1 to each cursor's jump
        (ral-node-jump-set! node-before
                            k
                            (++ (ral-node-jump node-before k)))
        (if (fx< k height)
            (let ((jump (ral-node-jump node-before k)))
              ;; populate jumps and nexts of new
              (ral-node-jump-set! new k (fx- jump (fx- place place-before)))
              (ral-node-next-set! new k (ral-cursor-next ls k))
              ;; adjust jumps and nexts of node-before
              (ral-node-jump-set! node-before k (fx- place place-before))
              (ral-node-next-set! node-before k new))))))
  (ral-count-set! ls (++ (ral-count ls))))

(define (ral-remove! ls place)
  ;; prepare cursor for deletion
  (ral-go! ls place) ; sets ral-level
  (ral-go-on! ls) ; doesn't touch level
  (let* ((level (ral-level ls))
         (node (ral-node-next (ral-cursor ls level) level)))
    (do ((k 0 (++ k)))
        ((fx= k (ral-height ls)))
      (let ((node-before (ral-cursor ls k)))
        (if (fx<= k level)
            ;; adjust jumps and nexts
            (begin
              (ral-node-jump-set! node-before
                                  k
                                  (-- (fx+ (ral-node-jump node-before k)
                                           (ral-node-jump node k))))
              (ral-node-next-set! node-before k (ral-node-next node k)))
            ;; adjust only jumps
            (ral-node-jump-set! node-before
                                k
                                (-- (ral-node-jump node-before k)))))))
  (ral-count-set! ls (-- (ral-count ls))))

;; insert at right end
(define (ral-add! ls item . items)
  (ral-insert! ls (ral-count ls) item)
  (do ((items items (cdr items)))
      ((null? items))
    (ral-insert! ls (ral-count ls) (car items))))

;; insert at left end
(define (ral-add-left! ls item . items)
  (ral-insert! ls 0 item)
  (do ((items items (cdr items)))
      ((null? items))
    (ral-insert! ls 0 (car items))))

(define (ral-clear! ls)
  (ral-height-set! ls 1)
  (ral-count-set! ls 0)
  (ral-level-set! ls 0)
  (let ((start (ral-start ls)))
    (ral-places-set! ls (vector -1))
    (ral-cursors-set! ls (vector start))
    (do ((k 0 (++ k)))
        ((fx= k (ral-max-height ls)))
      (ral-node-jump-set! start k 1)
      (ral-node-next-set! start k (ral-stop ls)))))

;; the following procedures work only as ordinary linked lists,
;; without taking advantage of the skiplist properties

;; this is needed to print a ral with items and places
(define* (ral->pairs ls (k 0))
  (ral-go! ls 0)
  (ral-go-on! ls)
  ;;(ral-forth! ls k)
  (let loop ((cursor                    ;(ral-cursor ls k))
              (ral-cursor-next ls k))
             (place                     ;(ral-place ls k))
              (ral-place-next ls k))
             (result '()))
    (if (ral-node-stop? cursor)
        (reverse result)
        (loop (ral-node-next cursor k)
              (fx+ place (ral-node-jump cursor k))
              (cons place (cons (ral-node-item cursor) result))))))

(define* (ral->list ls (k 0))
  (let loop ((node (ral-start ls)) (result '()))
    (if (ral-node-stop? node)
        (cdr (reverse result))          ; remove gstart
        (loop (ral-node-next node k)
              (cons (ral-node-item node) result)))))

(define (ral-for-each ls proc)
  (do ((node (ral-node-next (ral-start ls) 0) (ral-node-next node 0)))
      ((ral-node-stop? node)) ; way out
    (proc (ral-node-item node))))

(define* (ral-map ls proc (item? (ral-item? ls))
                  (width (ral-width ls))
                  (max-height (ral-max-height ls)))
  (let ((result (make-ral item? width max-height)))
    (do ((node (ral-node-next (ral-start ls) 0)
               (ral-node-next node 0)))
        ((ral-node-stop? node))         ; way out
      (ral-add! result (proc (ral-node-item node))))
    result))

(define* (ral-restructure ls (width (ral-width ls))
                          (max-height (ral-max-height ls)))
  (let ((result (make-ral (ral-item? ls) width max-height)))
    (do ((node (ral-node-next (ral-start ls) 0) (ral-node-next node 0)))
        ((ral-node-stop? node))
      (ral-add! result (ral-node-item node)))
    result))

(define (ral-filter ls ok?)
  (let ((result (make-ral (ral-item? ls)
                          (ral-width ls)
                          (ral-max-height ls))))
    (do ((node (ral-node-next (ral-start ls) 0)
               (ral-node-next node 0)))
        ((ral-node-stop? node)) ; way out
      (let ((item (ral-node-item node)))
        (if (ok? item) (ral-add! result item))))
    result))

(define (ral-split ls place)
  (let ((tail (make-ral (ral-item? ls) (ral-width ls) (ral-max-height ls)))
        (head (make-ral (ral-item? ls) (ral-width ls) (ral-max-height ls))))
    (do ((k 0 (++ k)))
        ((fx= k place))
      (ral-add! head (ral-ref ls k)))
    (do ((k place (++ k)))
        ((fx= k (ral-count ls)))
      (ral-add! tail (ral-ref ls k)))
    (values head tail)))

(define (ral-join head tail)
  (let ((result (make-ral (ral-item? head)
                          (fxmin (ral-width head) (ral-width tail))
                          (fxmax (ral-max-height head) (ral-max-height tail)))))
    (do ((k 0 (++ k)))
        ((fx= k (ral-count head)))
      (ral-add! result (ral-ref head k)))
    (do ((k 0 (++ k)))
        ((fx= k (ral-count tail)) result)
      (ral-add! result (ral-ref tail k)))))

(define (ral-from-upto ls from upto)
  (let ((result (make-ral (ral-item? ls) (ral-width ls) (ral-max-height ls))))
    (do ((k from (++ k)))
        ((fx= k upto) result)
      (ral-add! result (ral-ref ls k)))))

;; helper
(define (list-eql? eql? ls0 ls1)
  (and (fx= (length ls0) (length ls1))
       (let loop ((ls0 ls0) (ls1 ls1))
         (cond
          ((null? ls0) #t)
          ((not (eql? (car ls0) (car ls1))) #f)
          (else (loop (cdr ls0) (cdr ls1)))))))

(define (ral-eql? eql? ls0 ls1)
  (and (eq? (ral-item? ls0) (ral-item? ls1))
       (fx= (ral-count ls0) (ral-count ls1))
       (list-eql? eql? (ral->list ls0) (ral->list ls1))))

(define (ral-equal? ls0 ls1)
  (ral-eql? equal? ls0 ls1))

(define (ral-eqv? ls0 ls1)
  (ral-eql? eqv? ls0 ls1))

(define (ral-eq? ls0 ls1)
  (ral-eql? eq? ls0 ls1))


;;-------------------------------------------------------------------------------
;; Contract-based API


;; ;;;;; constructor

;; (define-with-contract make-ral
;;   (contract (result)
;;             ((_ item? . args)
;;              (and ((list-of? (lambda (arg) (and (fixnum? arg) (fx> arg 1))))
;;                    args)
;;                   (procedure? item?) "(item? item)")
;;              (%ral? result)))
;;   %make-ral)

;; ;;;;; predicates

;; ;; node predicate
;; (define-with-contract ral-node?
;;   (contract (result)
;;             ((_ xpr)
;;              #t
;;              (boolean? result)))
;;   %ral-node?)

;; ;; list predicate
;; (define-with-contract ral?
;;   (contract (result)
;;             ((_ xpr)
;;              #t
;;              (boolean? result)))
;;   %ral?)

;; ;; is ral empty?
;; (define-with-contract ral-null?
;;   (contract (result)
;;             ((_ ls)
;;              (%ral? ls)
;;              (boolean? result)))
;;   %ral-null?)

;; ;; are two rals elementwise eql?
;; (define-with-contract ral-eql?
;;   (contract (result)
;;             ((_ eql? ls0 ls1)
;;              (and (procedure? eql?) "(eql? item0 item1)"
;;                   (%ral? ls0) (%ral? ls1))
;;              (boolean? result)))
;;   %ral-eql?)

;; ;; are two rals elementwise equal?
;; (define-with-contract ral-equal?
;;   (contract (result)
;;             ((_ ls0 ls1)
;;              (and (%ral? ls0) (%ral? ls1))
;;              (boolean? result)))
;;   %ral-equal?)

;; ;; are two rals elementwise eqv?
;; (define-with-contract ral-eqv?
;;   (contract (result)
;;             ((_ ls0 ls1)
;;              (and (%ral? ls0) (%ral? ls1))
;;              (boolean? result)))
;;   %ral-eqv?)

;; ;; are two rals elementwise eq?
;; (define-with-contract ral-eq?
;;   (contract (result)
;;             ((_ ls0 ls1)
;;              (and (%ral? ls0) (%ral? ls1))
;;              (boolean? result)))
;;   %ral-eq?)

;; ;;;; queries

;; ;; item type predicate
;; (define-with-contract ral-item?
;;   (contract (result)
;;             ((_ ls)
;;              (%ral? ls)
;;              (procedure? result)))
;;   %ral-item?)

;; ;; maximal possible height of nodes in ral
;; (define-with-contract ral-max-height
;;   (contract (result)
;;             ((_ ls)
;;              (%ral? ls)
;;              (and (fixnum? result) (fx> result 0))))
;;   %ral-max-height)

;; ;; maximal actual height of nodes in ral
;; (define-with-contract ral-height
;;   (contract (result)
;;             ((_ ls)
;;              (%ral? ls)
;;              (and (fixnum? result) (fx> result 0))))
;;   %ral-height)

;; ;; stop level, where next place hits target place
;; (define-with-contract ral-level
;;   (contract (result)
;;             ((_ ls)
;;              (%ral? ls)
;;              (and (fixnum? result)
;;                   (fx> result 0)
;;                   (fx< result (%ral-height ls)))))
;;   %ral-level)

;; ;; width skipped on average at each search level supplied by constructor
;; (define-with-contract ral-width
;;   (contract (result)
;;             ((_ ls)
;;              (%ral? ls)
;;              (and (fixnum? result) (fx> result 1))))
;;   %ral-width)

;; ;; number of items stored in ral
;; (define-with-contract ral-count
;;   (contract (result)
;;             ((_ ls)
;;              (%ral? ls)
;;              (and (fixnum? result) (fx>= result 0))))
;;   %ral-count)

;; (define-with-contract ral-start
;;   (contract (result)
;;             ((_ ls)
;;              (%ral? ls)
;;              (%ral-node? result)))
;;   %ral-start)

;; (define-with-contract ral-place
;;   (contract (result)
;;             ((_ ls k)
;;              (and (%ral? ls) (fixnum? k) (fx>= k 0) (fx< k (%ral-height ls)))
;;              (and (fixnum? result) (fx>= result -1) (fx< result (%ral-count ls)))))
;;   %ral-place)

;; (define-with-contract ral-place-next
;;   (contract (result)
;;             ((_ ls k)
;;              (and (%ral? ls) (fixnum? k) (fx>= k 0) (fx< k (%ral-height ls)))
;;              (and (fixnum? result) (fx>= result 0) (fx<= result (%ral-count ls)))))
;;   %ral-place-next)

;; (define-with-contract ral-cursor-jump
;;   (contract (result)
;;             ((_ ls k)
;;              (and (%ral? ls) (fixnum? k) (fx>= k 0) (fx< k (%ral-height ls)))
;;              (and (fixnum? result) (fx> result 0) (fx<= result (%ral-count ls)))))
;;   %ral-cursor-jump)

;; (define-with-contract ral-cursor-next
;;   (contract (result)
;;             ((_ ls k)
;;              (and (%ral? ls) (fixnum? k) (fx>= k 0) (fx< k (%ral-height ls)))
;;              (or (null? result) (%ral-node? result))))
;;   %ral-cursor-next)

;; (define-with-contract ral-ref
;;   (contract (result)
;;             ((_ ls place)
;;              (and (%ral? ls)
;;                   (fixnum? place) (fx>= place 0) (fx< place (%ral-count ls)))
;;              ((%ral-item? ls) result)))
;;   %ral-ref)

;; ;;; the list of items stored in each level
;; (define-with-contract ral->list
;;   (contract (result)
;;             ((_ ls)
;;              (%ral? ls)
;;              ((list-of? (%ral-item? ls)) result))
;;             ((_ ls level)
;;              (and (%ral? ls) (fixnum? level)
;;                   (fx<= 0 level) (fx< level (%ral-height ls)))
;;              ((list-of? (%ral-item? ls)) result)))
;;   %ral->list)

;; (define-with-contract ral-filter
;;   (contract (result)
;;             ((_ ls ok?)
;;              (and (%ral? ls) (procedure? ok?) "(ok? item)")
;;              (and (%ral? result) (fx<= (%ral-count result) (%ral-count ls)))))
;;   %ral-filter)

;; ;; depending on the mapping procedure a differetn item? for the result
;; ;; might be necessary
;; (define-with-contract ral-map
;;   (contract (result)
;;             ((_ ls fn)
;;              (and (%ral? ls) (procedure? fn) "(fn item)")
;;              (and (%ral? result) (fx= (%ral-count result) (%ral-count ls))))
;;             ((_ ls fn item?)
;;              (and (%ral? ls) (procedure? fn) "(fn item)"
;;                   (procedure? item?) "(item? item)")
;;              (and (%ral? result) (fx= (%ral-count result) (%ral-count ls))
;;                   (eq? item? (%ral-item? result)))))
;;                                         ;    ((_ ls fn item? width)
;;                                         ;     (and (%ral? ls) (procedure? fn) "(fn item)" (procedure? item?)
;;                                         ;         (fixnum? width) (fx> width 1))
;;                                         ;     (and (%ral? result) (fx= (%ral-count result) (%ral-count ls))
;;                                         ;          (eq? item? (%ral-item? result)) (fx= (%ral-width result) width)))
;;                                         ;    ((_ ls fn item? width max-height)
;;                                         ;     (and (%ral? ls) (procedure? fn) "(fn item)" (procedure? item?)
;;                                         ;         (fixnum? width) (fx> width 1)
;;                                         ;         (fixnum? max-heigth) (fx> max-heigth 1))
;;                                         ;     (and (%ral? result) (fx= (%ral-count result) (%ral-count ls))
;;                                         ;          (eq? item? (%ral-item? result)) (fx= width (%ral-width result))
;;                                         ;          (fx= max-height (%ral-max-height result))))
;;   %ral-map)

;; (define-with-contract ral-from-upto
;;   (contract (result)
;;             ((_ ls from upto)
;;              (and (%ral? ls) (fixnum? from) (fixnum? upto) (fx>= from 0)
;;                   (fx>= upto from) (fx<= upto (%ral-count ls)))
;;              (and (%ral? result) (fx= (%ral-count result) (fx- upto from)))))
;;   %ral-from-upto)

;; (define-with-contract ral-split
;;   (contract (head tail)
;;             ((_ ls place)
;;              (and (%ral? ls) (fixnum? place) (fx>= place 0)
;;                   (fx< place (%ral-count ls)))
;;              (and (%ral? head) (%ral? tail) (fx= (%ral-count head) place)
;;                   (fx= (%ral-count tail) (fx- (%ral-count ls) place)))))
;;   %ral-split)

;; (define-with-contract ral-join
;;   (contract (result)
;;             ((_ head tail)
;;              (and (%ral? head) (%ral? tail)
;;                   (eq? (%ral-item? head) (%ral-item? tail)))
;;              (and (%ral? result)
;;                   (fx= (%ral-count result)
;;                        (fx+ (%ral-count head) (%ral-count tail))))))
;;   %ral-join)

;; (define-with-contract ral-restructure
;;   (contract (result)
;;             ((_ ls width)
;;              (and (%ral? ls) (fixnum? width) (fx> width 1))
;;              (and (%ral? result) (fx= (%ral-count ls) (%ral-count result))
;;                   (fx= (%ral-width result) width)))
;;             ((_ ls width max-height)
;;              (and (%ral? ls) (fixnum? width) (fx> width 1)
;;                   (fixnum? max-height) (fx> max-height 1))
;;              (and (%ral? result) (fx= (%ral-count ls) (%ral-count result))
;;                   (fx= (%ral-width result) width)
;;                   (fx= (%ral-max-height result) max-height))))
;;   %ral-restructure)


;; ;;;; commands

;; (define-with-contract ral-print
;;   (command-contract ((old new (constantly #t)))
;;                     ((_ ls)
;;                      (%ral? ls)
;;                      new))
;;   %ral-print)

;; (define-with-contract ral-for-each
;;   (command-contract ((old new (constantly #t)))
;;                     ((_ ls proc)
;;                      (and (%ral? ls) (procedure? proc) "(proc item)")
;;                      new))
;;   %ral-for-each)

;; (define-with-contract ral-set!
;;   (command-contract ((old new (lambda (ls place item) (%ral-ref ls place))))
;;                     ((_ ls place item)
;;                      (and (%ral? ls) ((%ral-item? ls) item)
;;                           (fixnum? place) (fx>= place 0) (fx< place (%ral-count ls)))
;;                      (equal? new item)))
;;   %ral-set!)

;; (define-with-contract ral-insert!
;;   (command-contract (
;;                      (oldcount newcount (lambda (ls place item) (%ral-count ls)))
;;                      (olditem newitem (lambda (ls place item) (%ral-ref ls place)))
;;                      )
;;                     ((_ ls place item)
;;                      (and (%ral? ls) ((%ral-item? ls) item)
;;                           (fixnum? place) (fx>= place 0) (fx<= place (%ral-count ls)))
;;                      (and (fx= newcount (fx+ 1 oldcount))
;;                           (equal? newitem item))))
;;   %ral-insert!)

;; (define-with-contract ral-add!
;;   (command-contract (
;;                      (oldcount newcount (lambda (ls item . items) (%ral-count ls)))
;;                      )
;;                     ((_ ls item . items)
;;                      (and (%ral? ls) ((%ral-item? ls) item)
;;                           ((list-of? (%ral-item? ls)) items))
;;                      (fx= newcount (fx+ (length (cons item items)) oldcount))))
;;   %ral-add!)

;; (define-with-contract ral-add-left!
;;   (command-contract (
;;                      (oldcount newcount (lambda (ls item . items) (%ral-count ls)))
;;                      )
;;                     ((_ ls item . items)
;;                      (and (%ral? ls) ((%ral-item? ls) item)
;;                           ((list-of? (%ral-item? ls)) items))
;;                      (fx= newcount (fx+ (length (cons item items)) oldcount))))
;;   %ral-add-left!)

;; (define-with-contract ral-remove!
;;   (command-contract (
;;                      (oldcount newcount (lambda (ls place) (%ral-count ls)))
;;                      )
;;                     ((_ ls place)
;;                      (%ral? ls)
;;                      (and (fx= newcount (fx- oldcount 1)))))
;;   %ral-remove!)

;; ;;;; commands
;;                                         ;
;; ;;; reset ral
;; (define-with-contract ral-clear!
;;   (command-contract (
;;                      (oldcount newcount %ral-count)
;;                      (oldheight newheight %ral-height)
;;                      )
;;                     ((_ ls)
;;                      (%ral? ls)
;;                      (and (fx= 0 newcount) (fx= 1 newheight))))
;;   %ral-clear!)
