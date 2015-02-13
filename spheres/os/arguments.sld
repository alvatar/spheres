;;!!! Argument processing for command-line programs
;; .author Per Eckerdal, for the Blackhole module system
;; .author Alvaro Castro-Castilla, 2013-2015

(define-library (spheres/os arguments)
  (export parse-arguments
          handle-opts!
          ensure-no-args!
          ensure-args!
          ensure-one-arg!)

  (include "arguments.scm"))
