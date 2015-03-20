(define-library (spheres/net/sack multipart-form-data)
  (export multipart/form-data-form-post->      ; Sack app wrapper adding 'form:data environment variable
         multipart/form-data-form-post-get    ; For manually invoking a decode
         multipart/form-data-form-post-decode ; Internal decoder procedure
         form-data-extract-uploaded-files
         form-data-extract-uploaded-file
         with-form-data-extract-uploaded-file
         multipart/form-data-verbose?)
  (import (spheres/string string)
          (spheres/string string-extra)
          (spheres/string u8vector)
          (spheres/net mime))

  (define-macro (with-form-data-extract-uploaded-file form-data . code)
    `(apply (lambda (form-field-name filename sender-content-type data-u8v)
              ,@code)
            (let* ((v                   (form-data-extract-uploaded-file ,form-data))
                   (form-field-name     (and v (car    v)))
                   (filename            (and v (cadr   v)))
                   (sender-content-type (and v (cadr   v)))
                   (data-u8v            (and v (cadddr v))))
              (list form-field-name filename sender-content-type data-u8v))))

  (include "multipart-form-data.scm"))
