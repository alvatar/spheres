;; Gambit Unicode library 1.0
;; New BSD license
;
;; Enables Gambit Scheme with Unicode handling features.
;; Runs out of the box on vanilla Gambit and on Black Hole.
;
;; The Gambit-specific code and distribution:
;; Copyright (C) 2010-2012 Mikael More
;
;; The unicode-data.scm generator and Bigloo unicode library this library was inspired by,
;; found in the bigloo-unicode directory:
;; Copyright (C) 2007-2012 Florian Loitsch
;
;; The source data for the unicode-data.scm generator, found in the bigloo-unicode/data directory:
;; Copyright (C) 1991-2011 Unicode, Inc.
;
;; ## Exports
;; unicode-char-upper
;; unicode-char-lower
;; unicode-string-upper unicode-string-upper! ; "aBc" => "ABC"
;; unicode-string-lower unicode-string-lower! ; "aBc" => "abc"
;; unicode-string-capitalize-strict! ; "aBc" => "Abc"
;; unicode-string-capitalize-strict  ;
;; string-unicode-ci<?
;; string-unicode-ci<=?
;; string-unicode-ci=?
;; string-unicode-ci>?
;; string-unicode-ci>=?
;
;; ## Relation with Bigloo Unicode library. Intended scope of use of this library.
;; This library is based on the Unicode library for Bigloo Scheme by Florian Loitsch, as bundled
;; with the js2scheme distribution as found on http://code.google.com/p/js2scheme/source/browse/ .
;;      A copy of this library is provided in this distribution in the bigloo-unicode directory,
;; to enable you to easily generate Unicode constants based on any new Unicode Character Database
;; versions.
;;      The vesion of the Bigloo Unicode library bundled here is was pulled from the abovementioned
;; repo the 13:t of May 2012. Florian's last changes were the 12:th of March 2011. Florian's code is
;; to be viewed as stable since at least 2010, to the best of my knowledge the only changes since
;; then have been minor restructurings.
;
;; Please note that the scope of this library and the Bigloo unicode library are different, in that
;; Gambit has complete native Unicode character handling and charset encoding/decoding features
;; built-in and thus none of this needs to be provided in a separate library, whereas the
;; Bigloo Unicode library provides such functionality to Bigloo.
;
;; ## Distribution overview
;; unicode.scm           - The Gambit Unicode handling library
;; unicode-data.scm      - The constants required to make unicode.scm spin.
;;                         Generated out of the Unicode Character Database.
;; bigloo-unicode/*      - Florian Loitsch's unicode-data.scm generator, and Bigloo Unicode library
;;                         that was used as inspiration for making this library.
;;                         Use this to generate a new unicode-data.scm out of a Unicode Character
;;                         Database update, if that would ever be released and relevant.
;;                         Latest version as of the 13:th of May 2012, see more above.
;; bigloo-unicode/data/* - The required files of the Unicode Character Database, see below.
;
;; ## How to update unicode-data.scm with a new version of the Unicode Character Database
;; In case by any reason this would ever be relevant, here is how to proceed:
;
;; Update the bigloo-unicode/data/ files (namely SpecialCasing.txt and UnicodeData.txt) with
;; the new Unicode Character Database files.
;
;; As of the date of writing, these are released on
;; http://www.unicode.org/Public/UNIDATA/SpecialCasing.txt and
;; http://www.unicode.org/Public/UNIDATA/UnicodeData.txt ,
;; and you can see the release date of the latest update of them on
;; http://www.unicode.org/Public/UNIDATA/ .
;
;; Go to the bigloo-unicode directory, remove utf-data.sch if it exists, and run make utf-data.sch .

(declare (standard-bindings) (extended-bindings) (mostly-fixnum) (block))

(cond-expand
  (black-hole
    ; Black Hole specific code:
    (import unicode-data) ; (Note that unicode-data.scm is actually a symlink to bigloo-data/utf-data.sch)

    (export unicode-char-upper
	    unicode-char-lower
    	    unicode-string-upper unicode-string-upper! ; "aBc" => "ABC"
    	    unicode-string-lower unicode-string-lower! ; "aBc" => "abc"
	    unicode-string-capitalize-strict! ; "aBc" => "Abc"
	    unicode-string-capitalize-strict  ;
	    string-unicode-ci<?
	    string-unicode-ci<=?
	    string-unicode-ci=?
	    string-unicode-ci>?
	    string-unicode-ci>=?))
    (else
      ; Vanilla Gambit-specific code:
      (include "unicode-data.scm") ; (Note that unicode-data.scm is actually a symlink to bigloo-data/utf-data.sch)
      ))

(define (unicode-char-upper c)
  (let* ((c (char->integer c))
         (i1 (bitwise-and #xFF c))
         (i2 (bitwise-and #xFF (arithmetic-shift c 8)))
         (i3 (bitwise-and #xFF (arithmetic-shift c 16)))
         (tmp (##vector-ref (##vector-ref (##vector-ref *upper-casing* i3) i2) i1)))
    (integer->char (if (zero? tmp)
                       c
                       tmp))))

(define (unicode-char-lower c)
  (let* ((c (char->integer c))
         (i1 (bitwise-and #xFF c))
         (i2 (bitwise-and #xFF (arithmetic-shift c 8)))
         (i3 (bitwise-and #xFF (arithmetic-shift c 16)))
         (tmp (##vector-ref (##vector-ref (##vector-ref *lower-casing* i3) i2) i1)))
    (integer->char (if (zero? tmp)
                       c
                       tmp))))

(define (make-unicode-string-upper/lower proc)
  (lambda (s)
    (let* ((l (string-length s))
           (ns (make-string l)))
      (let loop ((i 0))
        (if (> l i)
            (begin
              (string-set! ns i (proc (string-ref s i)))
              (loop (+ i 1)))))
      ns)))

(define (make-unicode-string-upper/lower! proc)
  (lambda (s)
    (let* ((l (string-length s)))
      (let loop ((i 0))
        (if (> l i)
            (begin
              (string-set! s i (proc (string-ref s i)))
              (loop (+ i 1)))))
      s)))

;; "aBc" => "ABC"
(define unicode-string-upper  (make-unicode-string-upper/lower  unicode-char-upper))

(define unicode-string-upper! (make-unicode-string-upper/lower! unicode-char-upper))

;; "aBc" => "abc"
(define unicode-string-lower  (make-unicode-string-upper/lower  unicode-char-lower))

(define unicode-string-lower! (make-unicode-string-upper/lower! unicode-char-lower))

;; "aBc" => "Abc"
(define (unicode-string-capitalize-strict! s)
  (let ((l (string-length s)))
    (let loop ((i 0))
      (if (< i l)
          (begin
            (##string-set! s i ((if (zero? i) unicode-char-upper unicode-char-lower)
                                (##string-ref s i)))
            (loop (fx+ i 1)))))))

(define (unicode-string-capitalize-strict s)
  (let ((s (string-copy s)))
    (unicode-string-capitalize-strict! s)
    s))

(define (string-unicode-ci<?  . a) (apply string-ci<?  (map unicode-string-upper a)))
(define (string-unicode-ci<=? . a) (apply string-ci<=? (map unicode-string-upper a)))
(define (string-unicode-ci=?  . a) (apply string-ci=?  (map unicode-string-upper a)))
(define (string-unicode-ci>?  . a) (apply string-ci>?  (map unicode-string-upper a)))
(define (string-unicode-ci>=? . a) (apply string-ci>=? (map unicode-string-upper a)))
