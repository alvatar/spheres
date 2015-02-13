;;!!! SRFI-19: Time Data Types and Procedures.
;; .author Marco Benelli, 2009-2011
;; .author Alvaro Castro-Castilla, 2015. Minor modifications

(define-library (spheres/os time)
  (export
   ;; Make native Gambit procedure with name clash available
   ##current-time
   ;; Constants
   time-duration
   time-monotonic
   time-process
   time-tai
   time-thread
   time-utc
   ;; Current time and clock resolution
   current-date
   current-julian-day
   current-modified-julian-day
   current-time
   time-resolution
   ;; Time object and accessors
   make-time
   time?
   time-type
   time-nanosecond
   time-second
   set-time-type!
   set-time-nanosecond!
   set-time-second!
   copy-time
   ;; Time comparison procedures
   time<=?
   time<?
   time=?
   time>=?
   time>?
   ;; Time arithmetic procedures
   time-difference
   time-difference!
   add-duration
   add-duration!
   subtract-duration
   subtract-duration!
   ;; Date object and accessors
   make-date
   date?
   date-nanosecond
   date-second
   date-minute
   date-hour
   date-day
   date-month
   date-year
   date-zone-offset
   date-year-day
   date-week-day
   date-week-number
   ;; Time/Date/Julian Day/Modified Julian Day Converters
   date->julian-day
   date->modified-julian-day
   date->time-monotonic
   date->time-tai
   date->time-utc
   julian-day->date
   julian-day->time-monotonic
   julian-day->time-tai
   julian-day->time-utc
   modified-julian-day->date
   modified-julian-day->time-monotonic
   modified-julian-day->time-tai
   modified-julian-day->time-utc
   time-monotonic->date
   time-monotonic->julian-day
   time-monotonic->modified-julian-day
   time-monotonic->time-tai
   time-monotonic->time-tai!
   time-monotonic->time-utc
   time-monotonic->time-utc!
   time-tai->date
   time-tai->julian-day
   time-tai->modified-julian-day
   time-tai->time-monotonic
   time-tai->time-monotonic!
   time-tai->time-utc
   time-tai->time-utc!
   time-utc->date
   time-utc->julian-day
   time-utc->modified-julian-day
   time-utc->time-monotonic
   time-utc->time-monotonic!
   time-utc->time-tai
   time-utc->time-tai!
   ;; Date to String/String to Date Converters
   date->string
   string->date)

  (include "date-time.scm"))
