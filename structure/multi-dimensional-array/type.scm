;;!!! Multi-dimensional array
;;; Jussi Piitulainen, 2002

;; As record type
(define-record-type
  array:srfi-9-record-type-descriptor
  (array:make vec ind shp)
  array:array?
  (vec array:vector)
  (ind array:index)
  (shp array:shape))
