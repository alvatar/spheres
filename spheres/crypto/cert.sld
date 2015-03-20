;;!!! Provides procedures to create and manage digital certificates, and
;; to sign and verify messages.
;; .author Marc Feeley, 2006-2007
;; .author Alvaro Castro-Castilla, 2015
;; .license: lgpl/v2.1
;;
;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

(define-library (spheres/crypto cert)
  (export create-certificate-pair
          certificate-owner
          lookup-certificate
          remove-certificate
          certificate=
          certs-member
          certs-union
          certs-intersection
          certs-difference
          certs-equal?
          certificate->list
          list->certificate
          u8vector->cert-alist
          cert-alist->u8vector
          cert->base64-string
          base64-string->cert
          base64-cert?
          write-publ-certs
          read-publ-certs
          write-priv-certs
          read-priv-certs
          create-signature
          sign-u8vector
          verify-u8vector-detailed
          verify-u8vector
          filter-certificates-of-class
          sign-certificate
          verify-certificate
          show-certificate
          list-certificates)

  (import (spheres/algorithm list)
          (spheres/string string) ;; string-index
          (spheres/string string-extra)
          (spheres/string u8vector)
          (spheres/crypto digest)
          (spheres/crypto rsa)
          (spheres/crypto io)
          (spheres/dataenc base64)
          (spheres/dataenc bignum)
          (spheres/io genport)
          (spheres/io ttyui))

  (include "cert.scm"))
