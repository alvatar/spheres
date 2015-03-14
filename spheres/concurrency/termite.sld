;;!!! Termite (Erlang-like concurrency system)
;; .author Guillaume Germain, Copyright (C) 2005-2009, All Rights Reserved.
;; .author Alvaro Castro-Castilla, 2015.

;; This is the declaration file for the Termite system
(define-library (spheres/concurrency termite)
  (export self ;; Termite "primordials"
          !
          ?
          ??
          !?
          on
          make-node
          spawn pid?
          spawn-link
          remote-spawn
          remote-spawn-link
          ;; Useful
          make-tag
          current-node
          ;; Process linking for error propagation
          inbound-link
          outbound-link
          full-link
          ;; Wrap Gambit's I/O
          spawn-output-port
          spawn-input-port
          ;; Exceptions...
          termite-exception? ;; actually that has to be exported for RECV
          ;; Migration
          migrate-task
          migrate/proxy
          ;; Useful condition reporting/logging procedures
          termite-log
          termite-warning
          termite-debug
          termite-info
          ;; Node stuff
          node-init
          node?
          node-host
          node-port
          ;; Nameserver mechanism
          make-nameserver-node
          ;; OTP-style stuff (genserver)
          make-server-plugin
          server:start
          server:start-link
          server:call
          server:cast
          server:stop
          ;; Distributed data structures
          make-dict
          dict?
          dict->list
          dict-for-each
          dict-search
          dict-set!
          dict-ref
          dict-length
          ;; Publishing and resolving names for services
          publish-service
          unpublish-service
          resolve-service
          remote-service
          ;; default init and node names for convenience
          init
          node1
          node2
          *termite-nameserver-port*
          *termite-cookie*
          ;; Useful
          ping
          ;; Macros
          termite-match
          termite-match/action
          recv)

  (import (spheres/net uuid)
          (spheres/os date-format))

  ;; Clause manipulation
  ;;
  ;;! 2 possible clause expression:
  ;;
  ;; (termite-match data
  ;;   (clause (where guard) . code)
  ;;   (clause               . code))
  (define-macro (termite-match/action on-success on-fail datum . clauses)
    ;; (define-record-type clause/rt
    ;;   (make-clause pattern guard code)
    ;;   clause?
    ;;   (pattern clause-pattern)
    ;;   (guard   clause-guard)
    ;;   (code    clause-code))
    (define (make-clause pattern guard code)
      (list 'clause-rt pattern guard code))
    (define (clause? c)
      (and (pair? c) (eq? (car c) 'clause-rt)))
    (define (clause-pattern c)
      (if (not (clause? c)) (error "clause expected"))
      (cadr c))
    (define (clause-guard c)
      (if (not (clause? c)) (error "clause expected"))
      (caddr c))
    (define (clause-code c)
      (if (not (clause? c)) (error "clause expected"))
      (cadddr c))
    ;; compile-pattern-match: generate the code for the pattern matching
    ;;
    ;; on-success: code to insert when a clause matches
    ;; on-fail: code to execute when the whole pattern fail
    ;; clause-list: list of all the pattern clauses
    ;; args: the name of the variable holding the value we're matching
    ;;       against (bad name...)
    (define (compile-pattern-match on-success on-fail clause-list args)
      (define (filter pred? lst)
        (cond
         ((null? lst) '())
         ((pred? (car lst))
          (cons (car lst)
                (filter pred? (cdr lst))))
         (else
          (filter pred? (cdr lst)))))
      (define (remove pred? lst)
        (filter (lambda (x) (not (pred? x))) lst))
      (define (quoted-symbol? datum)
        (and
         (pair? datum)
         (eq? (car datum) 'quote)
         (pair? (cdr datum))
         (symbol? (cadr datum))))
      (define (unquoted-symbol? datum)
        (and
         (pair? datum)
         (eq? (car datum) 'unquote)
         (pair? (cdr datum))
         (symbol? (cadr datum))))
      ;; accumulate every part of the tree which satisfies PRED? and only go
      ;; down the child satisfying GO-DOWN?
      (define (tree-filter pred? go-down? tree)
        (cond
         ((pred? tree)
          (list tree))
         ((and (pair? tree) (go-down? tree))
          (append (tree-filter pred? go-down? (car tree))
                  (tree-filter pred? go-down? (cdr tree))))
         (else '())))
      ;; remove duplicates (bad name...)
      (define (delete-duplicates lst)
        (cond
         ((null? lst) '())
         ((member (car lst)
                  (cdr lst))
          (delete-duplicates (cdr lst)))
         (else
          (cons (car lst)
                (delete-duplicates (cdr lst))))))
      ;; call the translation on each clause sequentially
      (define (translate clauses made uenv ienv)
        (if (null? clauses)
            on-fail
            (let ((clause (car clauses))
                  (f-k (lambda (made uenv ienv)
                         (translate
                          (cdr clauses)
                          made
                          '()
                          ienv))))
              (pattern-walk (clause-pattern clause)
                            made
                            uenv
                            ienv
                            args
                            (lambda (made uenv ienv)
                              ;; TODO: remove this ugly test (and the
                              ;; on-success test...)
                              (if (eq? #t (clause-guard clause))
                                  (if on-success
                                      `(begin
                                         ,on-success
                                         ,((clause-code clause) uenv))
                                      ((clause-code clause) uenv))
                                  `(if (let ,uenv ,(clause-guard clause)) ;; ~if ?
                                       ,(if on-success
                                            `(begin
                                               ,on-success
                                               ,((clause-code clause) uenv))
                                            ((clause-code clause) uenv))
                                       ,(f-k made uenv ienv))))
                            f-k))))
      ;; is that variable already bound?
      (define (bound? var env) (assoc var env))
      ;; what code is it bound to
      (define (lookup var env) (cadr (assoc var env)))
      (define (rlookup val env)
        (let ((v (assoc val (map (lambda (p)
                                   (cons (cadr p)
                                         (car p)))
                                 env))))
          (if (not v)
              (error "can't find it:" (list val: val env: env))
              (cdr v))))
      ;; extend the env. with a new var
      (define (extend var val env) (cons (list var val) env))
      ;; this is the compilation function that goes 'down' a clause
      ;; patt: the pattern to match
      ;; made: the tests made, an a-list of (test-code . result)
      ;; uenv: the user env., ex. the x in (match 123 (x x))
      ;; ienv: the 'internal' env, bindings introduced by the macro
      ;; acc: the current 'accessor', that is the way to get to the current patt
      ;; s-k: success continuation, if the match succeeds
      ;; f-k: failure continuation, if the match fails
      (define (pattern-walk patt made uenv ienv acc s-k f-k)
        ;; has that test already been made
        (define (test-made? test) (assoc test made))
        ;; did it fail or succeed
        (define (test-result test) (cdr (assoc test made)))
        ;; build one of those test
        (define (make-test test var . val)
          (if (pair? val)
              `(,test ,var ,(car val))
              `(,test ,var)))
        ;; add a test that succeeded
        (define (add-t-test test) (cons (cons test #t) made))
        ;; add a test that failed
        (define (add-f-test test) (cons (cons test #f) made))
        ;; check to see if a test has already succeded in another context,
        ;; that would mean trying the current test would fail (I take this
        ;; assumes every test is mutually exclusive...)
        (define (test-would-fail? test)
          (let ((acc (cadr test))
                (successful-tests (filter cdr made)))
            (member acc (map cadar successful-tests))))
        ;; generate an IF if both branches are different
        (define (~if test succ fail)
          (if (equal? succ fail)
              succ
              `(if ,test
                   ,succ
                   ,fail)))
        ;; main body of pattern-walk
        (cond
         ;; if the pattern is null
         ((null? patt)
          (let ((test (make-test 'null? acc)))
            (cond
             ((test-made? test)
              (if (test-result test)
                  (s-k made uenv ienv)
                  (f-k made uenv ienv)))
             ((test-would-fail? test)
              (f-k made uenv ienv))
             (else
              (~if test
                   (s-k (add-t-test test) uenv ienv)
                   (f-k (add-f-test test) uenv ienv))))))
         ;; is the pattern some constant value?
         ((or (quoted-symbol? patt)
              (number? patt) ;; fixme: bignums wont work (because of eq?)
              (eq? #t patt)
              (eq? #f patt)
              (char? patt)
              (keyword? patt))
          (let ((test (make-test
                       'eq?
                       acc
                       (if (quoted-symbol? patt)
                           `',(cadr patt)
                           patt))))
            (cond
             ((test-made? test)
              (if (test-result test)
                  (s-k made uenv ienv)
                  (f-k made uenv ienv)))
             ((test-would-fail? test)
              (f-k made uenv ienv))
             (else
              (~if test
                   (s-k (add-t-test test) uenv ienv)
                   (f-k (add-f-test test) uenv ienv))))))
         ;; is the pattern an unquoted symbol (reference to the user env)?
         ((unquoted-symbol? patt)
          (let ((test (make-test 'eq? acc (cadr patt))))
            (~if test
                 (s-k made uenv ienv)
                 (f-k made uenv ienv))))
         ;; is the pattern a pair?
         ((pair? patt)
          (let ((test (make-test 'pair? acc)))
            (cond
             ;; test done already
             ((test-made? test)
              (if (test-result test)
                  (pattern-walk (car patt)
                                made
                                uenv
                                ienv
                                (rlookup `(car ,acc) ienv)
                                (lambda (made uenv ienv)
                                  (pattern-walk (cdr patt)
                                                made
                                                uenv
                                                ienv
                                                (rlookup `(cdr ,acc) ienv)
                                                s-k
                                                f-k))
                                f-k)
                  (f-k made uenv ienv)))
             ;; another test succeded meaning this one would fail
             ((test-would-fail? test)
              (f-k made uenv ienv))
             ;; do the test
             (else
              (~if test
                   (let ((?car (gensym))
                         (?cdr (gensym)))
                     (let ((ienv (extend ?car
                                         `(car ,acc)
                                         (extend ?cdr
                                                 `(cdr ,acc)
                                                 ienv))))
                       `(let ((,?car (car ,acc))
                              (,?cdr (cdr ,acc)))
                          ,(pattern-walk (car patt)
                                         (add-t-test test)
                                         uenv
                                         ienv
                                         ?car
                                         (lambda (made uenv ienv)
                                           (pattern-walk (cdr patt)
                                                         made
                                                         uenv
                                                         ienv
                                                         ?cdr
                                                         s-k
                                                         f-k))
                                         f-k))))
                   (f-k (add-f-test test) uenv ienv))))))
         ;; is it a 'free' symbol, to be bound to a new value or compared
         ;; to a previous value which it was bound to during the pattern
         ;; matching?
         ((symbol? patt)
          (if (bound? patt uenv)
              (let ((test (make-test 'eq? acc (lookup patt uenv))))
                (if (test-made? test)
                    (if (test-result test)
                        (s-k made uenv ienv)
                        (f-k made uenv ienv))
                    (~if test
                         (s-k (add-t-test test) uenv ienv)
                         (f-k (add-f-test test) uenv ienv))))
              (if (eq? patt '_)
                  (s-k made uenv ienv)
                  (s-k made (extend patt acc uenv) ienv))))
         ;; maybe it's something we don't handle in here
         (else
          (error "unknown pattern" patt))))
      ;; compile-pattern-match main body
      ;; this code build clauses, then extract the [non-trivial]
      ;; CLAUSE-CODEs and lift them outside WARNING: hairy code, fixme
      (let* ((transform
              (map
               (lambda (clause)
                 (let ((pattern (car clause))
                       (guard
                        (let ((g (map
                                  cadr
                                  (filter
                                   (lambda (x)
                                     (and (pair? x)
                                          (eq? (car x)
                                               'where)))
                                   (cdr clause)))))
                          (case (length g)
                            ((0) #t)
                            ((1) (car g))
                            (else `(and ,@g)))))
                       (code
                        (let ((c (remove
                                  (lambda (x)
                                    (and (pair? x)
                                         (eq? (car x)
                                              'where)))
                                  (cdr clause))))
                          (case (length c)
                            ((0) #t) ;; ???
                            ((1) (car c))
                            (else `(begin ,@c))))))
                   (make-clause pattern guard code)))
               clause-list))
             (data (map (lambda (clause)
                          (let ((code-label (gensym))
                                (var-list
                                 (delete-duplicates
                                  (tree-filter
                                   (lambda (t)
                                     (and (symbol? t)
                                          (not (eq? t '_))))
                                   (lambda (t)
                                     (not
                                      (or
                                       (unquoted-symbol? t)
                                       (quoted-symbol? t))))
                                   (clause-pattern clause))))
                                (code (clause-code clause)))
                            (let ((lifted `(,code-label
                                            (lambda ,var-list
                                              ,code)))
                                  ;; trivial non-triviality test
                                  (not-trivial? (and (pair? code)
                                                     (not (quoted-symbol? code)))))
                              (cons (and not-trivial? lifted)
                                    (make-clause
                                     (clause-pattern clause)
                                     (clause-guard clause)
                                     (lambda (env)
                                       (if not-trivial?
                                           (cons code-label
                                                 (map
                                                  (lambda (var)
                                                    (cadr (assoc var env)))
                                                  var-list))
                                           (if (symbol? code)
                                               (cond
                                                ((assoc code env) => cadr)
                                                (else code))
                                               code))))))))
                        transform)))
        `(let ,(map car (filter car data))
           ,(translate (map cdr data) '() '() '()))))
    (let ((tmp (gensym))
          (succ (gensym))
          (fail (gensym)))
      `(let ((,tmp ,datum)
             (,succ (lambda () ,on-success)) ;; the thunk for success is lifted
             (,fail (lambda () ,on-fail))) ;; the thunk for failure is lifted
         ,(compile-pattern-match `(,succ) `(,fail) clauses tmp))))

  (define-macro (termite-match datum . clauses)
    `(termite-match/action
      #f
      (raise (list bad-match: ',clauses))
      ,datum
      ,@clauses))

  (define-macro (recv . clauses)
    (let ((msg  (gensym 'msg)) ;; the current mailbox message
          (loop (gensym 'loop))) ;; the mailbox seeking loop
      ;; check the last clause to see if it's a timeout
      (let ((sesualc (reverse clauses)))
        (if (and (pair? (car sesualc))
                 (eq? (caar sesualc) 'after))
            (let ((clauses (reverse (cdr sesualc)))
                  ;; the code to compute the timeout
                  (init (cadar sesualc))
                  ;; the variable holding the timeout
                  (timeout (gensym 'timeout))
                  ;; the code to be executed on a timeout
                  (on-timeout (cddar sesualc))
                  ;; the timeout exception-handler to the whole match
                  (e (gensym 'e)))
              ;; RECV code when there is a timeout
              `(let ((,timeout ,init))
                 (with-exception-catcher
                  (lambda (,e)
                    (if (mailbox-receive-timeout-exception? ,e)
                        (begin
                          (thread-mailbox-rewind)
                          ,@on-timeout)
                        (raise ,e)))
                  (lambda ()
                    (let ,loop ((,msg (thread-mailbox-next ,timeout)))
                         (termite-match/action
                          (thread-mailbox-extract-and-rewind)
                          (,loop
                           (thread-mailbox-next ,timeout))
                          ,msg
                          ;; extra clause to handle system events
                          (event
                           (where (termite-exception? event))
                           (handle-exception-message event))
                          ;; the user clauses
                          ,@clauses))))))
            ;; RECV code when there is no timeout
            `(let ,loop ((,msg (thread-mailbox-next)))
                  (termite-match/action
                   (thread-mailbox-extract-and-rewind)
                   (,loop
                    (thread-mailbox-next))
                   ,msg
                   ;; extra clause to handle system events
                   (event
                    (where (termite-exception? event))
                    (handle-exception-message event))
                   ;; the user clauses
                   ,@clauses))))))

  (include "termite.scm"))
