;;!!! x-www-form-urlencoded encoding and decoding.
;; .author Marc Feeley, 2005-2006
;; .author Per Eckerdal, 2008-2009
;; .author Mikael More, 2010-2012
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/net x-www-form-urlencoded)
  (export urlencode
          urlencode-ISO8859
          write-urlencoded
          write-urlencoded-ISO8859
          write-urlencoded-u8vector
          write-urlencoded-u8vector-lambda
          urlencode->u8vector
          urldecode
          urldecode-ISO8859
          urldecode->u8vector
          write-x-www-form-urlencoded/ISO-8859-1
          write-x-www-form-urlencoded/UTF-8
          encode-x-www-form-urlencoded/ISO-8859-1
          encode-x-www-form-urlencoded/UTF-8
          decode-x-www-form-urlencoded/ISO-8859-1
          decode-x-www-form-urlencoded/UTF-8
          sack-app-give-x-www-form-urlencoded/ISO-8859-1-form-post-decoder
          sack-app-give-x-www-form-urlencoded/UTF-8-form-post-decoder
          application/x-www-form-urlencoded/ISO8859-data-form-post-decode
          application/x-www-form-urlencoded/UTF-8-data-form-post-decode)
  (import (spheres/string string) ;; string-trim
          (spheres/string string-extra) ;; string-split-char string-split-at-first-nice
          (spheres/string u8vector))

  (define-macro (write-urlencoded-u8vector-lambda . dontencode-clause)
    (let ((dontencode-clause
           (if (null? dontencode-clause)
               '(or ;; (char->integer #\a), (char->integer #\z)
                 (fx<= 97 b 122)
                 ;; (char->integer #\A), (char->integer #\Z)
                 (fx<= 65 b  90)
                 ;; (char->integer #\0), (char->integer #\9)
                 (fx<= 48 b  57)
                 ;; (char->integer #\_)
                 (eq? b 95)
                 ;; (char->integer #\.) - Microsoft (api.bing.com) doesn't handle .:s urlencoded in HTTP query key values
                 (eq? b 46))
               (car dontencode-clause))))
      `(lambda (u8v)
         (define (write-nibble n)
           ;; The following vector comes from
           ;; (list->vector (map char->integer (string->list "0123456789ABCDEF")))
           (write-u8 (##vector-ref '#(48 49 50 51 52 53 54 55 56 57 65 66 67 68 69 70) n)))
         (define l (u8vector-length u8v))
         (let loop ((i 0))
           (if (< i l)
               (let ((b (u8vector-ref u8v i)))
                 (cond (,dontencode-clause
                        (write-u8 b))
                       ((eq? b 32)      ; (char->integer #\space)
                        (write-u8 (char->integer #\+)))
                       (else
                        (write-u8 (char->integer #\%))
                        (write-nibble
                         (fxand (fxarithmetic-shift-right b 4) 15))
                        (write-nibble (fxand b 15))))
                 (loop (+ i 1))))))))

  (include "x-www-form-urlencoded.scm"))
