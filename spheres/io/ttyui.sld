;;!!! Procedures to enter text in console
;; .author Marc Feeley, 2006-2007
;; .author Alvaro Castro-Castilla, 2015

(define-library (spheres/io ttyui)
  (export flush-console-input
          read-line-from-console
          enter-y-or-n
          enter-line-ascii
          enter-password
          enter-new-password)

  (include "ttyui.scm"))
