;!!! rfc1123 - date format
;; .author Marco Benelli, 2012
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (c) 2012 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.


(declare
 (standard-bindings)
 (extended-bindings)
 (block)
 (generic)
 (not safe))


(define EPOCH-YEAR 1970)

(define weekdays
  (vector
    "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(define months
  (vector
    "Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define month-length
  (let ((months
          (vector
            31 28 31 30 31 30 31 31 30 31 30 31    ; non-leap years
            31 29 31 30 31 30 31 31 30 31 30 31))) ; leap years
    (lambda (m y)
      (vector-ref months (+ m (if (leap-year? y) 12 0))))))

(define (leap-year? y)
  (and (zero? (modulo y 4))
       (or (not (zero? (modulo y 100)))
           (zero? (modulo y 400)))))

(define (year-length y)
  (if (leap-year? y) 366 365))

;;! Convert seconds from epoch (an integer) to a formatted string.
(define (seconds->string x)
  (define (padstr x)
    (if (< x 10) "0" ""))
  (let* ((dayno (quotient x 86400))
         (dayclock (modulo x 86400))
         (sec (modulo dayclock 60))
         (min (quotient (modulo dayclock 3600) 60))
         (hour (quotient dayclock 3600))
         (wday (modulo (+ dayno 3) 7))  ; day 0 was a thursday
         (year EPOCH-YEAR)
         (month 0))
    (do ((y EPOCH-YEAR (+ y 1))
         (d dayno (- d (year-length y))))
        ((< d (year-length y)) (begin
                                 (set! dayno d)
                                 (set! year y))))
    (do ((m month (+ 1 m))
         (d dayno (- d (month-length m year))))
        ((< d (month-length m year))
         (begin
           (set! month m)
           (set! dayno (+ d 1)))))
    (with-output-to-string
      ""
      (lambda ()
        (print
         (vector-ref weekdays wday)
         ", "
         (padstr dayno) dayno
         #\space
         (vector-ref months month)
         #\space
         year
         #\space
         (padstr hour) hour
         #\:
         (padstr min) min
         #\:
         (padstr sec) sec
         " GMT")))))

;;! Convert Scheme time to a formatted string.
(define (time->string t)
  (seconds->string
   (inexact->exact
    (truncate
     (time->seconds t)))))
