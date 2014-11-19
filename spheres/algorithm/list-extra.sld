;;!!! Additional list procedures
;; Alvaro Castro-Castilla, 2012-2014, All rights reserved.

(define-library (spheres/algorithm list-extra)
  (export atom?
          xor
          snoc
          uncons
          uncons-2
          uncons-3
          uncons-4
          uncons-cons
          unlist
          unvector
          cars+cdrs
          cars
          cdrs
          list-ref-right
          rotate-left
          rotate-right
          list-swap!
          map*
          map**
          map/values
          fold/values
          demux
          pair-map
          map+fold
          map-fold
          pair-fold-x
          pair-fold-2
          insert-at
          insert-left
          insert-left*
          insert-right
          insert-right*
          intersperse
          remove-at
          remove-first
          remove-any
          remove-every
          find-remove
          find+remove
          find-rotate
          most
          most+remove
          most/generator
          max/generator
          min/generator
          x-substitute
          substitute-first
          substitute
          x-subst*
          substitute-first*
          substitute*
          flatten
          flatten-unordered
          flatten-tag
          list->skeleton
          expand-skeleton
          apply-skeleton
          slice
          slice!
          split-in-halves
          split-in-halves!
          classify
          case-classify
          replicate
          pack
          n-groups
          group
          ticker!)
  (import (spheres/core base)
          (spheres/algorithm list))
  
  ;;! Map applying the function only to the elements satisfying predicate
  (define-syntax map-if
    (syntax-rules ()
      ((_ p f l)
       (map (lambda (e) (if (p e) (f e) e)) l))
      ((_ p ft ff l)
       (map (lambda (e) (if (p e) (ft e) (ff e))) l))))

  ;;! Map and cond combined: maps applying a function to the elements that
  ;; satisfy each predicate. It can contain an else clause
  (define-syntax map-cond
    (syntax-rules (else)
      ;; Implicit with selector
      ((_ ((?letb <- ?s) ...) ((?p ?f) ...) ?l . ?lt) ; entry for implicit vars with selector
       (map-cond "expl/sel" ((?letb ?s) ...) () ((?p ?f) ...) () (?l . ?lt)))
      ((_ "expl/sel" ?let-block (?vars ...) ((?p ?f) ...) (?l ...) (?lh . ?lls)) ; recur
       (map-cond "expl/sel" ?let-block (?vars ... x) ((?p ?f) ...) (?l ... ?lh) ?lls))
      ((_ "expl/sel" ?let-block (?vars ...) ((?p ?f) ...) (?l ...) ()) ; finalize
       (map-cond "expl/sel-init-let" (?vars ...) ?let-block ((?p ?f) ...) (?l ...)))

      ((_ "expl/sel-init-let" ?vars ((?bind ?s) . ?ct) ?cond-block (?l ...)) ; init let
       (map-cond "expl/sel-let" ?vars ?ct ((?bind (?s . ?vars))) ?cond-block (?l ...)))
      ((_ "expl/sel-let" ?vars ((?bind ?s) . ?ct) (?bind-list ...) ?cond-block (?l ...)) ; recur let
       (map-cond "expl/sel-let" ?vars ?ct (?bind-list ... (?bind (?s . ?vars))) ?cond-block (?l ...)))
      ((_ "expl/sel-let" ?vars () (?bind-list ...) ?cond-block (?l ...)) ; finalize let
       (map-cond "expl/sel-init-cond" ?cond-block ?vars (?bind-list ...) (?l ...)))

      ((_ "expl/sel-init-cond" ((else ?f) . ?ct) ?vars ?let-block  (?l ...)) ; error: else is first
       (error "Syntax error: else clause can't be first"))
      ((_ "expl/sel-init-cond" ((?p ?f) . ?ct) ?vars ?let-block (?l ...)) ; init
       (map-cond "expl/sel-cond" ?ct ((?p ?f)) ?vars ?let-block (?l ...)))
      ((_ "expl/sel-cond" ((else ?f)) (?conds ...) ?vars ?let-block (?l ...)) ; catch given 'else'
       (map (lambda ?vars (let ?let-block (cond ?conds ... (else ?f)))) ?l ...))
      ((_ "expl/sel-cond" ((else ?f) . ?ct) (?conds ...) ?vars ?let-block (?l ...)) ; error: else is not last
       (error "Syntax error: else clause must be last"))
      ((_ "expl/sel-cond" ((?p ?f) . ?ct) (?conds ...) ?vars ?let-block (?l ...)) ; recur
       (map-cond "expl/sel-cond" ?ct (?conds ... (?p ?f)) ?vars ?let-block (?l ...)))
      ((_ "expl/sel-cond" () (?conds ...) ?vars ?let-block (?l ...)) ; finalize cond
       (map (lambda ?vars (let ?let-block (cond ?conds ... (else #f)))) ?l ...))

      ;; Explicit
      ((_ (?vars ...) ((?p ?f ...) ...) ?l ...) ; entry for explicit vars case
       (map-cond "expl-init" (?vars ...) ((?p ?f ...) ...) ?l ...))

      ((_ "expl-init" (?vars ...) ((else ?f ...) . ?ct) ?l ...) ; error: else is first
       (error "Syntax error: else clause can't be first"))
      ((_ "expl-init" (?vars ...) (((?p ...) ?f ...) . ?ct) ?l ...) ; init explicit-vars
       (map-cond "expl-cond" (?vars ...) ?ct (((?p ...) ?f ...)) (?l ...)))

      ((_ "expl-cond" (?vars ...) ((else ?f . ?ft)) (?conds ...) (?l ...)) ; catch given 'else'
       (map (lambda (?vars ...) (cond ?conds ... (else ?f . ?ft))) ?l ...))
      ((_ "expl-cond" (?vars ...) ((else ?f . ?ft) . ?ct) (?conds ...) (?l ...)) ; error: else is not last
       (error "Syntax error: else clause must be last"))
      ((_ "expl-cond" (?vars ...) (((?p ...) ?f ...) . ?ct) (?conds ...) (?l ...)) ; recur explicit-vars
       (map-cond "expl-cond" (?vars ...) ?ct (?conds ... ((?p ...) ?f ...)) (?l ...)))
      ((_ "expl-cond" (?vars ...) () (?conds ...) (?l ...)) ; finalize with default 'else'
       (map (lambda (?vars ...) (cond ?conds ... (else #f))) ?l ...))

      ;; Implicit
      ((_ ((?p ?f) ...) ?l . ?lt)       ; entry for given vars case
       (map-cond "impl" () ((?p ?f) ...) () (?l . ?lt)))
      ((_ "impl" (?vars ...) ((?p ?f) ...) (?l ...) (?lh . ?lls)) ; recur vars
       (map-cond "impl" (?vars ... x) ((?p ?f) ...) (?l ... ?lh) ?lls))
      ((_ "impl" (?vars ...) ((?p ?f) ...) (?l ...) ()) ; finalize vars
       (map-cond "impl-cond-init" (?vars ...) ((?p ?f) ...) (?l ...)))

      ((_ "impl-cond-init" ?vars ((else ?f) . ?ct) (?l ...)) ; error: else is first
       (error "Syntax error: else clause can't be first"))
      ((_ "impl-cond-init" ?vars ((?p ?f) . ?ct) (?l ...)) ; init cond
       (map-cond "impl-cond" ?vars ?ct (((?p . ?vars) (?f . ?vars))) (?l ...)))
      ((_ "impl-cond" ?vars ((else ?f)) (?conds ...) (?l ...)) ; catch given 'else' in cond
       (map (lambda ?vars (cond ?conds ... (else (?f . ?vars)))) ?l ...))
      ((_ "impl-cond" ?vars ((else ?f) . ?ct) (?conds ...) (?l ...)) ; error: else is not last
       (error "Syntax error: else clause must be last"))
      ((_ "impl-cond" ?vars ((?p ?f) . ?ct) (?conds ...) (?l ...)) ; recur cond
       (map-cond "impl-cond" ?vars ?ct (?conds ... ((?p . ?vars) (?f . ?vars))) (?l ...)))
      ((_ "impl-cond" ?vars () (?conds ...) (?l ...)) ; finalize cond with default 'else'
       (map (lambda ?vars (cond ?conds ... (else #f))) ?l ...))

      ;; Global wrong syntax cases
      ((_ ((any ...) ...) thing ...)    ; detect wrong syntax
       (error "Syntax error: wrong number of arguments in condition"))))

  ;;! Apply a function to values, interleaving multiple sources
  ;; (apply/values (lambda (x y) (cons x y)) (values 'a 'b) (values 1 2))
  ;; => (a . 1)
  ;;    (b . 2)
  ;;
  ;;              g1 -------+
  ;;             /          |
  ;;            /           |
  ;;           /            |
  ;;          /             +--------> (f g1 h1) --> val1
  ;; g* -->  o--- g2 ----------+
  ;;          \             |  |
  ;;   |       \            |  |
  ;;   |        \           |  |
  ;;   |         \          |  |
  ;;   |          g3 ----+  |  |
  ;; values              |  |  +-----> (f g2 h2) --> val2
  ;;   |          h1 -------+  |
  ;;   |         /       |     |
  ;;   |        /        |     |
  ;;   |       /         |     |
  ;;          /          |     |
  ;; h* -->  o--- h2 ----------+
  ;;          \          +-----------> (f g3 h3) --> val3
  ;;           \         |
  ;;            \        |                            ...
  ;;             \       |
  ;;              h3 ----+
  (define-syntax apply/values
    (syntax-rules ()
      ((_ "init-transformation" ?l . ?ls)
       (apply/values "transformation" ((values->list ?l)) . ?ls))
      ((_ "transformation" (?tr ...))
       (list ?tr ...))
      ((_ "transformation" (?tr ...) ?l . ?ls)
       (apply/values "transformation" (?tr ... (values->list ?l)) . ?ls))
      ((_ ?f ?ls ...)
       (list->values
        (map (lambda (e) (apply ?f e)) (apply zip (apply/values "init-transformation" ?ls ...)))))))

  (include "list-extra.scm"))
