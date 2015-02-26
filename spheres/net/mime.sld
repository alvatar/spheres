;;!!! Decode and encode data using the MIME format.
;; .author Marc Feeley, 2006-2007
;; .author Alvaro Castro-Castilla, 2015
;; .license lgpl/v2.1
;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.
;;
;;
;; See the following RFCs:
;;
;;   RFC 2045 - Multipurpose Internet Mail Extensions (MIME) Part One:
;;   Format of Internet Message Bodies
;;
;;   RFC 2046 - Multipurpose Internet Mail Extensions (MIME) Part Two:
;;   Media Types
;;
;;   RFC 2047 - MIME (Multipurpose Internet Mail Extensions) Part Three:
;;   Message Header Extensions for Non-ASCII Text
;;
;;   RFC 2048 - Multipurpose Internet Mail Extensions (MIME) Part Four:
;;   Registration Procedures
;;
;;   RFC 2049 - Multipurpose Internet Mail Extensions (MIME) Part Five:
;;   Conformance Criteria and Examples
;;
;;   RFC 2822 - Internet Message Format

(define-library (spheres/net mime)
  (export mime-format-header
          mime-parse-header
          mime-encode-x-www-form-urlencoded
          mime-decode-x-www-form-urlencoded
          mime-decode-text
          mime-encode-text
          mime-encode-application
          mime-decode-application
          mime-encode-multipart
          mime-decode-multipart-with-boundary
          mime-encode
          mime-decode
          mime-string->content-type
          mime-eol-str)
  (import (spheres/algorithm u8vector)
          (spheres/string u8vector))

  (include "mime.scm"))
