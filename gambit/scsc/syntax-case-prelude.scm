;;;============================================================================

;;; File: "syntax-case.scm", Time-stamp: <2014-03-21 22:26:28 matthastie>

;;; Copyright (c) 1998-2008 by Marc Feeley, All Rights Reserved.

;;; This is version 3.2 .

;; This version includes a patch which avoids quoting self-evaluating
;; constants.  This makes it possible to use some Gambit specific forms
;; such as declare, namespace and define-macro.

;; This is an implementation of "syntax-case" for the Gambit-C 4.0
;; system based on the portable implementation "psyntax.ss".  At the
;; top of the file "psyntax.ss" can be found this information:
;;
;;      Portable implementation of syntax-case
;;      Extracted from Chez Scheme Version 7.3 (Feb 26, 2007)
;;      Authors: R. Kent Dybvig, Oscar Waddell, Bob Hieb, Carl Bruggeman

;; This file can be used to replace the builtin macro expander of the
;; interpreter and compiler.  Source code correlation information
;; (filename and position in file) is preserved by the expander.  The
;; expander mangles non-global variable names and this complicates
;; debugging somewhat.  Note that Gambit's normal parser processes the
;; input after expansion by the syntax-case expander.  Since the
;; syntax-case expander does not know about Gambit's syntactic
;; extensions (like DSSSL parameters) some of the syntactic
;; extensions cannot be used.  On the other hand, the syntax-case
;; expander defines some new special forms, such as "module",
;; "alias", and "eval-when".

;; You can simply load this file at the REPL with:
;;
;;   (load "syntax-case")
;;
;; For faster macro processing it is worthwhile to compile the file
;; with the compiler.  You can also rename this file to "gambcext.scm"
;; and put it in the Gambit "lib" installation directory so that it is
;; loaded every time the interpreter and compiler are started.
;;
;; Alternatively, the expander can be loaded from the command line
;; like this:
;;
;;   % gsi ~~lib/syntax-case -
;;   > (pp (lambda (x y) (if (< x y) (let ((z (* x x))) z))))
;;   (lambda (%%x0 %%y1)
;;     (if (< %%x0 %%y1) ((lambda (%%z2) %%z2) (* %%x0 %%x0)) (void)))

;;;============================================================================

(##declare
 (standard-bindings)
 (extended-bindings)
 (inlining-limit 100)
 (block)
)

(##namespace ("sc#"))

(##include "~~lib/gambit#.scm")

(##namespace (""

$make-environment
$sc-put-cte
$syntax-dispatch
bound-identifier=?
datum->syntax
environment?
free-identifier=?
generate-temporaries
identifier?
interaction-environment
literal-identifier=?
syntax-error
syntax->datum
syntax->list
syntax->vector
$load-module
$update-module
$include-file-hook
$generate-id
syntax-case-debug
))

(##namespace ("sc#"

interaction-environment
eval
gensym

))

;;;============================================================================

;; The following procedures are needed by the syntax-case system.

(define andmap
  (lambda (f first . rest)
    (or (null? first)
        (if (null? rest)
            (let andmap ((first first))
              (let ((x (car first)) (first (cdr first)))
                (if (null? first)
                    (f x)
                    (and (f x) (andmap first)))))
            (let andmap ((first first) (rest rest))
              (let ((x (car first))
                    (xr (map car rest))
                    (first (cdr first))
                    (rest (map cdr rest)))
                (if (null? first)
                    (apply f (cons x xr))
                    (and (apply f (cons x xr)) (andmap first rest)))))))))

(define ormap
  (lambda (proc list1)
    (and (not (null? list1))
         (or (proc (car list1)) (ormap proc (cdr list1))))))

(define eval
  (lambda (expr)
    (cond ((and (##pair? expr)
                (##equal? (##car expr) "noexpand")
                (##pair? (##cdr expr))
                (##null? (##cddr expr)))
           (##eval (##cadr expr)))
          ((and (##source? expr)
                (##pair? (##source-code expr))
                (##source? (##car (##source-code expr)))
                (##equal? (##source-code (##car (##source-code expr))) "noexpand")
                (##pair? (##cdr (##source-code expr)))
                (##null? (##cddr (##source-code expr))))
           (##eval (##cadr (##source-code expr))))
          (else
           (##raise-error-exception
            "eval expects an expression of the form (\"noexpand\" <expr>)"
            (##list expr))))))

(define gensym-count 0)

(define gensym
  (lambda id
    (let ((n gensym-count))
      (set! gensym-count (+ n 1))
      (string->symbol
       (string-append "%%"
                      (if (null? id) "" (symbol->string (car id)))
                      (number->string n))))))

(define gensym?
  (lambda (obj)
    (and (symbol? obj)
         (let ((str (symbol->string obj)))
           (and (> (string-length str) 2)
                (string=? (substring str 0 2) "%%"))))))

(define prop-table (##make-table))

(define remprop
  (lambda (sym key)
    (let ((sym-key (cons sym key)))
      (##table-set! prop-table sym-key))))

(define putprop
  (lambda (sym key val)
    (let ((sym-key (cons sym key)))
      (##table-set! prop-table sym-key val))))

(define getprop
  (lambda (sym key)
    (let ((sym-key (cons sym key)))
      (##table-ref prop-table sym-key #f))))

(define list*
  (lambda (arg1 . other-args)

    (define (fix lst)
      (if (null? (cdr lst))
          (car lst)
          (cons (car lst) (fix (cdr lst)))))

    (fix (cons arg1 other-args))))

(define remq
  (lambda (obj lst)
    (cond ((null? lst)
           '())
          ((eq? (car lst) obj)
           (remq obj (cdr lst)))
          (else
           (cons (car lst) (remq obj (cdr lst)))))))

;;;----------------------------------------------------------------------------

;; These initial definitions are needed because these variables are
;; mutated with a "set!" without a prior definition.

(define $sc-put-cte (make-parameter #f))
(define sc-expand (lambda (src) src)) ; temporary definition
(define $make-environment #f)
(define make-expander #f)
(define environment? #f)
(define interaction-environment #f)
(define identifier? #f)
(define syntax->list #f)
(define syntax->vector #f)
(define syntax->datum #f)
(define datum->syntax #f)
(define generate-temporaries #f)
(define free-identifier=? #f)
(define bound-identifier=? #f)
(define literal-identifier=? #f)
(define syntax-error #f)
(define $syntax-dispatch #f)
(define visit-marker '#(visit))
(define $load-module #f)
(define $update-module #f)
(define $include-file-hook #f)
(define syntax-case-debug (make-parameter #f))

;;; generate-id ideally produces globally unique symbols, i.e., symbols
;;; unique across system runs, to support separate compilation/expansion.
;;; Use gensyms if you do not need to support separate compilation/
;;; expansion or if your system's gensym creates globally unique
;;; symbols (as in Chez Scheme).  Otherwise, use the following code
;;; as a starting point.  session-key should be a unique string for each
;;; system run to support separate compilation; the default value given
;;; is satisfactory during initial development only.

(define $generate-id
  (make-parameter
   (let ((digits "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!$%&*/:<=>?~_^.+-"))
     (let ((base (string-length digits)) (session-key "_"))
       (define make-digit (lambda (x) (string-ref digits x)))
       (define fmt
         (lambda (n)
           (let fmt ((n n) (a '()))
             (if (< n base)
                 (list->string (cons (make-digit n) a))
                 (let ((r (modulo n base)) (rest (quotient n base)))
                   (fmt rest (cons (make-digit r) a)))))))
       (let ((n -1))
         (lambda (name) ;; name is #f or a symbol
           (set! n (+ n 1))
           (string->symbol 
            (string-append session-key
                           (fmt n)
                           (if name
                               (string-append "." (symbol->string name))
                               "")))))))))

;;;----------------------------------------------------------------------------

;;; Interface to Gambit's source code annotations.

(define annotation?
  (lambda (x)
;;    (pp `(annotation? ,x))
    (##source? x)))

(define annotation-expression
  (lambda (x)
;;    (pp `(annotation-expression ,x))
    (##source-code x)))

(define annotation-stripped
  (lambda (x)
;;    (pp `(annotation-stripped ,x))
    (##desourcify x)))

(define build-source
  (lambda (ae x)
;;    (pp `(build-source ,ae ,x))
    (if (##source? ae)
        (##make-source x (##source-locat ae))
        (##make-source x #f))))

(define attach-source
  (lambda (ae datum)
;;    (pp `(attach-source ,ae ,datum))
    (let ((src
           (if (##source? ae)
               ae
               (##make-source ae #f))))

      (define (datum->source x)
        (##make-source (cond ((pair? x)
                              (list-convert x))
                             ((box? x)
                              (box (datum->source (unbox x))))
                             ((vector? x)
                              (vector-convert x))
                             (else
                              x))
                       (##source-locat src)))

      (define (list-convert lst)
        (cons (datum->source (car lst))
              (list-tail-convert (cdr lst))))

      (define (list-tail-convert lst)
        (cond ((pair? lst)
               (if (quoting-form? lst)
                   (datum->source lst)
                   (cons (datum->source (car lst))
                         (list-tail-convert (cdr lst)))))
              ((null? lst)
               '())
              (else
               (datum->source lst))))

      (define (quoting-form? x)
        (let ((first (car x))
              (rest (cdr x)))
          (and (pair? rest)
               (null? (cdr rest))
               (or (eq? first 'quote)
                   (eq? first 'quasiquote)
                   (eq? first 'unquote)
                   (eq? first 'unquote-splicing)))))

      (define (vector-convert vect)
        (let* ((len (vector-length vect))
               (v (make-vector len)))
          (let loop ((i (- len 1)))
            (if (>= i 0)
              (begin
                (vector-set! v i (datum->source (vector-ref vect i)))
                (loop (- i 1)))))
          v))

      (datum->source datum))))

;;;----------------------------------------------------------------------------

(define self-eval?
  (lambda (x)
    (or (number? x)
        (string? x)
        (char? x)
        (keyword? x)
        (memq x
              '(#f
                #t
                #!eof
                #!void
                #!unbound
                #!unbound2
                #!optional
                #!rest
                #!key)))))

;;;----------------------------------------------------------------------------

(define (unmark! src)

  (define (unmark-list! lst)
    (cond ((pair? lst)
           (unmark! (car lst))
           (unmark-list! (cdr lst)))))

  (define (unmark-vector! vect)
    (let ((len (vector-length vect)))
      (do ((i 0 (fx+ i 1)))
          ((fx= i len))
        (unmark! (vector-ref vect i)))))
    
  (cond ((and (vector? src)
              (positive? (vector-length src))
              (equal? (vector-ref src 0) visit-marker))
         (vector-set! src 0 ##source1-marker)
         (unmark! (##source-code src)))
        ((pair? src)
         (unmark-list! src))
        ((vector? src)
         (unmark-vector! src))
        ((box? src)
         (unmark! (unbox src)))
        (else src)))

;;;============================================================================

(set! ##expression-parsing-exception-names
      (append '((psyntax-error . "Syntax expansion failure:")
                (invalid-argument . "Invalid argument:"))
            ##expression-parsing-exception-names))

;;;============================================================================

