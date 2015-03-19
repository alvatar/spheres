(define-library (spheres/net http-util)
  (export crlf-s
          headers-join
          http-status-code
          header-key->string
          display-crlf
          display-header
          display-headers
          chunk-return-value->u8v&u8v-length
          http-write-with-chunked-encoding
          split-attribute-line
          read-line-until-crlf/skip-empty-line/-s
          read-headers
          read-content-chars
          chunked-coding-read-hex)

  (import (spheres/algorithm list)
          (spheres/string string)
          (spheres/string string-extra)
          (spheres/string u8vector)
          (spheres/net x-www-form-urlencoded)
          (spheres/net/sack io-primitives))

  (define-macro (http-util#chunk-return-value->u8v&u8v-length chunk . code)
    `(call-with-values
         (lambda () ,chunk)
       (lambda* (data (u8v-length #f))
           (let* ((u8v (if (u8vector? data)
                           data
                           ;; (string? data) should hold true here.
                           (string->utf8-u8vector data)))
                  (u8v-length (or u8v-length (u8vector-length u8v))))
             ,@code))))

  (include "http-util.scm"))
