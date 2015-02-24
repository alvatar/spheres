;;!!! Tar archiving
;; .author Marc Feeley, 2006-2012
;; Copyright (c) 2006-2012 by Marc Feeley, All Rights Reserved.

(define-library (spheres/dataformat tar)
  (export implement-type-tar-rec
          make-tar-rec
          tar-rec-name
          tar-rec-mode
          tar-rec-uid
          tar-rec-gid
          tar-rec-mtime
          tar-rec-type
          tar-rec-linkname
          tar-rec-uname
          tar-rec-gname
          tar-rec-devmajor
          tar-rec-devminor
          tar-rec-atime
          tar-rec-ctime
          tar-rec-content

          make-tar-condition
          tar-condition?
          tar-condition-msg

          tar-pack-genport
          tar-pack-file
          tar-pack-u8vector
          tar-unpack-genport
          tar-unpack-file
          tar-unpack-u8vector

          tar-read
          tar-write
          tar-write-unchecked

          tar-file
          untar-file)
  (import (spheres/io genport)
          (spheres/dataformat zlib))

  (include "tar.scm"))
